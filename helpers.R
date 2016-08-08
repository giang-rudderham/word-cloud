# Function to clean tweets
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

# Function to prepare for plot of word cloud
word.cloud <- function(tweets) {
  tweet.corpus <- Corpus(VectorSource(tweets))
  tweet.corpus <- tm_map(tweet.corpus, removeNumbers)
  tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords("SMART"))
  
  tdm <- TermDocumentMatrix(tweet.corpus,
                            control = list(minWordLength = 1))
  
  m <- as.matrix(tdm)
  m <- sort(rowSums(m), decreasing = TRUE)
  m
}