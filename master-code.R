# Load libraries
#################################
library(twitteR) # For searchTwitter
library(plyr) # For laply, ddply
library(stringr) # For str_split
library(ggplot2) # For box plot
library(car) # For Levene's test
library(coin) # For wilcox_test
library(tm) # For word cloud
library(wordcloud) # For word cloud

# Declare Twitter API Credentials
####################################
api_key <- *** 
api_secret <- *** 
token <- *** 
token_secret <- *** 

# Create Twitter Connection
##############################
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Obtain tweets
##########################
get.tweet <- function(search.word){
  data <- searchTwitter(search.word, n = 5000, lang = "en",
                      since = "2016-04-23", until = "2016-05-02")
  data.df <- twListToDF(data)
  data.df$date <- as.character(format(data.df$created, format = "%Y-%m-%d"))
  data.df
}

yhoo.df <- get.tweet("$YHOO")
goog.df <- get.tweet("$GOOG")

yhoo.text <- yhoo.df$text
goog.text <- goog.df$text

# Clean tweets
###########################
clean.tweet <- function(tweet, leave) {
  tweet <- unlist(str_split(tweet, "\\s+"))
  #split at white space, tab, new line
  if (tweet[1] == "RT") {tweet <- tweet[-1]}
  #remove "RT" (show up in retweets)
  tweet <- tweet[!grepl("http", tweet)] 
  #remove web links
  tweet <- tweet[!grepl("&amp", tweet)]
  tweet <- gsub("[^[:alnum:]]", "", tweet)
  tweet <- tolower(tweet)
  tweet <- tweet[!grepl(paste0(leave[1], "|", leave[2], collapse = ""), tweet)]
  paste(tweet, collapse = " ")
}

yhoo.cleaned <- laply(yhoo.text, clean.tweet, c("yhoo", "yahoo"))
goog.cleaned <- laply(goog.text, clean.tweet, c("goog", "alphabet"))

# Opinion Lexicon
###########################
hu.liu.pos <- scan("positive-words.txt",
                   what = "character", comment.char = ";")
hu.liu.neg <- scan("negative-words.txt",
                   what = "character", comment.char = ";")

pos.words <- c(hu.liu.pos, scan("positive-stock.txt", what = "character"))

neg.words <- c(hu.liu.neg, scan("negative-stock.txt", what = "character"))

# Sentiment analysis
###########################
sentiment.score <- function(tweet) {
	tweet <- unlist(str_split(tweet, "\\s+")) 
	#split at white space, tab, new line
	pos.matches <- !is.na(match(tweet, pos.words))
	neg.matches <- !is.na(match(tweet, neg.words))
	sum(pos.matches) - sum(neg.matches)
}

yhoo.score <- laply(yhoo.cleaned, sentiment.score)
goog.score <- laply(goog.cleaned, sentiment.score)

yhoo.freq <- table(yhoo.score)
goog.freq <- table(goog.score)

part1 <- cbind(rep("yhoo", length.out = length(yhoo.score)), yhoo.score)
part2 <- cbind(rep("goog", length.out = length(goog.score)), goog.score)
new.df <- as.data.frame(rbind(part1, part2), stringsAsFactors = F)
new.df$yhoo.score <- as.numeric(new.df$yhoo.score)
colnames(new.df) <- c("comp", "score")

# Histogram
###################################
par(mfrow = c(2,1))
hist(yhoo.score, breaks = seq(-7, 6) - 0.5, col = "grey",
     xlab = "Sentiment Score", ylim = c(0, 700),
     main = "Histogram of Sentiment Score for Yahoo! Inc.")
box()
hist(goog.score, breaks = seq(-7, 6) - 0.5, col = "grey",
     xlab = "Sentiment Score", ylim = c(0, 2200),
     main = "Histogram of Sentiment Score for Google Inc.")
box()

# Box plots
#########################
comp.factor <- factor(new.df$comp, labels = c("Google", "Yahoo"))

ggplot(data = new.df, aes(x = comp.factor, y = new.df$score)) +
  geom_boxplot(aes(fill = factor(comp))) +
  ## Choose colors:
  scale_fill_manual(values = c("#C390D4", "#12AAE6")) +
  ## Change to white background
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey")) +
  ylab("Sentiment Score") +
  xlab("Company") +
  theme(legend.position = "none") + #remove legend
  ggtitle("Sentiment Scores for Yahoo! Inc. and Google Inc.\
          Outliers are displayed by dots.")

#Levene's test for constant variance
#####################################
#H0: variance of group 1 (Google) = variance of group 2 (Yahoo)

new.df$comp <- as.factor(new.df$comp)

levene.out <- leveneTest(lm(score ~ comp, data = new.df))
#p-value = 0.4919; fail to reject H0

# Mann-Whitney test (in lieu of t-test since data non-normal)
################################################################
# H_0: Y_1 - Y_2 = 0, 
# Y_s: median of the responses in the s^{th} sample 

pvalue(wilcox_test(score ~ comp, data = new.df,
                   alternative = "two.sided") ) #reject H0

# Word Cloud
###############
word.cloud <- function(tweets) {
  # turn tweets into a Corpus
  tweet.corpus <- Corpus(VectorSource(tweets))
  # remove numbers and stopwords
  tweet.corpus <- tm_map(tweet.corpus, removeNumbers)
  tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords("SMART"))
  
  tdm <- TermDocumentMatrix(tweet.corpus,
                            control = list(minWordLength = 1))
  
  m <- as.matrix(tdm)
  m <- sort(rowSums(m), decreasing = TRUE)
  
  wordcloud(names(m), m, scale = c(3, 0.5), 
            min.freq = 2, max.words = 50,
            colors = brewer.pal(8, "Dark2"))  
}
set.seed(5)
word.cloud(yhoo.cleaned)
set.seed(5)
word.cloud(goog.cleaned)

# Run R Shiny App
#############################
library(shiny)
runApp("shinyWordcloud")
