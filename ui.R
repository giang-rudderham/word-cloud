library(shiny)

shinyUI(fluidPage(
  titlePanel("Trending on Twitter"),
  sidebarLayout(
    sidebarPanel(
      p("This Shiny App displays word clouds created from tweets
        about stocks of selected tech companies."),
      br(),
      selectInput("chosen", label = "Choose a company:", 
                  choices = c("Apple","Google", "Yahoo"), 
                  selected = "Yahoo",
                  multiple = FALSE),   
      sliderInput("day", "Number of days into the past to obtain tweets
                  (More days will increase processing time):",
                  min = 1, max = 5, value = 1, step = 1),
      sliderInput("scale", "Change scale of the cloud:",
                  min = 1, max = 4, value = 2),
      sliderInput("time", "Minimum times a word is tweeted:",
                  min = 1, max = 20, value = 4, step = 1),
      sliderInput("number", "Maximum number of words to be displayed:",
                  min = 1, max = 100, value = 40)
          ),
    mainPanel(
      plotOutput("wordcloud"),
      br(),
      br(),
      br(),
      br(),
      br(),
      p(strong("Created by:"), "Giang Nguyen (giang-nguyen@uiowa.edu)."),
      p(strong("Source of tweets:"), "From https://twitter.com/."),
      p(strong("Twitter search keywords:"), "$AAPL (Apple Inc.), 
        $GOOG (Google Inc.), and $YHOO (Yahoo! Inc.).")
    )
  )
))