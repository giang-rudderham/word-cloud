# Load libraries
#################################
library(shiny)
library(twitteR) # For searchTwitter
library(plyr) # For laply, ddply
library(stringr) # For str_split
library(tm) # For word cloud
library(wordcloud) # For word cloud

# Source scripts
###################################
source("helpers.R")

# Declare Twitter API Credentials
####################################
api_key <- "FaebkHZ1EMYkvMyEnYsXLcRmX" 
api_secret <- "uiRaHrtrIlA8jJR2VXLv1KiPTmx06evdHn2a1fa0injm0WpD0T" 
token <- "712674756386893824-Lsfur00QL4paZEJ3QbnJTES17CorLj5" 
token_secret <- "kCJflbhJCUxw8svxQmhvB2mGfsLC70vxPTif4eCltNQN3" 

# Create Twitter Connection
##############################
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Start shinyServer
###############################
shinyServer(function(input,output){
  dataInput <- reactive({
    # Search for tweets
    ###################
    if (input$chosen == "Apple") {
      search.word <- "$AAPL"
      leave.word <- c("appl", "aapl")
    }
    if (input$chosen == "Google") {
      search.word <- "$GOOG"
      leave.word <- c("goog", "alphabet")
    }
    if (input$chosen == "Yahoo") {
      search.word <- "$YHOO"
      leave.word <- c("yhoo", "yahoo")
    }
    
    withProgress({
      setProgress(message = "Searching for tweets...",
                  detail = "This might take a while...
                  especially if you are searching for tweets about Apple.")
      data <- suppressWarnings(
        searchTwitter(search.word, n = 3000, lang = "en",
                      since = as.character(Sys.Date() - input$day), 
                      until = as.character(Sys.Date()))
        )  
    })
    # Data cleaning
    ##################
    withProgress({
      setProgress(message = "Cleaning tweets...")
      data.df <- twListToDF(data)
      data.df$date <- as.character(format(data.df$created, 
                                          format = "%Y-%m-%d"))
      text <- data.df$text
      date <- data.df$date
      cleaned.text <- laply(text, clean.tweet, leave.word)
    })
    
    # Prepare for word cloud
    ###########################
    withProgress({
      setProgress(message = "Nearly there...")
      m <- word.cloud(cleaned.text)
    })
  })
  
  wordcloud_seed <- repeatable(wordcloud)
  
  output$wordcloud <- renderPlot({
    m <- dataInput()
    suppressWarnings(
      wordcloud_seed(names(m), m, 
                     scale = c(input$scale * 3, input$scale / 2),
                     min.freq = input$time, max.words = input$number,
                     colors = brewer.pal(8, "Dark2"))
      )
  })
})
