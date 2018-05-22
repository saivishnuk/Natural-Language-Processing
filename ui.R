
library(shiny)
library(lexicon)
library(sentimentr)
library(tidytext)
library(tidyr)
library(data.table)
library(shinythemes)
library(rvest)
library(tm)
library(rpart)
library(dplyr)
library(ggplot2)


  navbarPage("Sai Vishnu Kanisetty - Natural Language Processing    (Analyze Toyota Camry car reviews by extracting data from cars.com)",
             theme = shinythemes::shinytheme("yeti"),
             #shinythemes::themeSelector(),
             tabsetPanel(id = "tabs",
                         tabPanel(
                           "Functionality", 
                           #htmlOutput("contents12")
                           h4("Purpose"),
                           h6("To understand Toyota Camry customers' opinion by analyzing reviews from cars.com"),
                           tags$hr(),
                           h4("Data"),
                           #tags$i("Upload data and View Result:"),
                           h6("Data from 2012-2017 toyota camry models has been considered for the analysis and data has been further split into test(2017) data and the rest as training data "),
                           
                           tags$hr(),
                           h4("Sentiment Analysis"),
                           tags$i("Dictionary used for calculating sentiment"),
                           h6("Different polarity lexicons have been tested, and the 'hash_sentiment_jockers_rinker' lexicon in 'sentimentr' package has been used "),
                           h6("Advantage of using this lexicon over afinn is, For Example: sentiment of the sentence 'I am not happy' is -0.375 using 'hash_sentiment_jockers_rinker' lexicon, where as afinn would give a sentiment of 3"),
                           tags$i("Comparison between user rating and sentiment"),
                           h6("The avg. user rating and avg. sentiment are not directly comparable by magnitude because of the limited range of word polarity in 'hash_sentiment_jockers_rinker' lexicon "),
                           h6("But, they can be compared in terms of direction. For Example: While the avg. user rating is high for test data (4.7) when compared to train (4.5), the same pattern can be observed in avg. sentiment, i.e avg. sentiment for test data(0.59) is higher than avg. sentiment for train data (0.49)  "),
                           h6("Therefore, both user rating and sentiment analysis are analogous, and we can conclude that, 'Avergae rating of 2017(test) model cars is high when compared to average rating of other models (train) considered' "),
                           tags$i("Predicting user rating using sentiment"),
                           h6("Multiple machine learning techniques have been experimented and Decision trees has been selected for predicting the user rating based on sentiment score. Training data has been used for building the prediction model"),
                           h6("Test accuracy is more than train accuracy because of the difference in percentage of reviews available for each user rating in train data when compared to test data"),
                           
                           tags$hr(),
                           h4("TfIdf"),
                           h6("Stop words have been removed and TfIdf has been calculated separately for reviews with tags 'service', 'price', 'handling', 'interior'"),
                           h6("Important words that are associated with the tag 'interior' are: dash (representing dash board), console, music, radio, room"),
                           h6("Though stop words have been removed, removing nouns, pronouns and correcting misspelled words will help in identifying words that are more relevant")
                         ),
                         tabPanel(
                           " Input Link and Display Result ",
                           sidebarLayout(
                             sidebarPanel(
                               h4("The required columns have been displayed for all data and also separately for train and test data"),
                               h4("Default link is provided as part of the text input"),
                               h5("Application requires an active internet connection and it takes ~5-6 seconds to load the data"),
                               textInput("file2","Enter the url:",value = "https://www.cars.com/research/toyota-camry/")#,
                               #submitButton("Update")
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",       
                                           tabPanel("All Data",  dataTableOutput("contents_all")),
                                           tabPanel("Train Data",  dataTableOutput("contents1")),
                                           tabPanel("Test Data",  dataTableOutput("contents2"))
                                           #,
                                           #tabPanel("Test Data",  textOutput("url"))         
                               )))),
                         tabPanel(
                           "Sentiment",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Q5. A dataset containing a combined and augmented version of Jockers (2017) & Rinker's
                                  augmented Hu & Liu (2004) positive/negative word list is used as sentiment lookup values."),
                               # h5(""),
                               h4("Q6 (a,b). Average user rating and Average sentiment have been compared for (All,Train,Test) data"),
                               h4("Q7 & 8. Decision trees have been used for predicting the user rating based on sentiment score. Training data has been used for building the prediction model. Both the test and train accuracy have been displayed")
                               ),
                             mainPanel(
                               tabsetPanel(type = "tabs",       
                                           tabPanel("Q5. All reviews",  dataTableOutput("sentiment_all")),
                                           tabPanel("Q6a. Sentiment comparison",  dataTableOutput("sentiment_avg")),
                                           tabPanel("Q6b. Sentiment comparison by tags",  dataTableOutput("sentiment_avg_tags")),
                                           tabPanel("Q7,8. Prediction",  dataTableOutput("sentiment_pre"))
                               )))),
                         tabPanel(
                           " TfIdf",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Q9. Reviews with no stop words have been displayed and a new variable, 'id', has been created as an identifier for each review"),
                               h4("Top 10 tfidf values for all tags from train data have been visualized in separate tabs and 'id' can be cross-referenced to understand the review associated to the word"),
                               h6("The tf, idf, tf_idf values have been rounded to 5 decimals")
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",       
                                           tabPanel("Normalized with no stop words",  dataTableOutput("tfidf_normal")),
                                           tabPanel("Tfidf - #service",  fluidRow(
                                             column(width = 6, dataTableOutput("tfidf_service")),
                                             column(width = 1, "" ),
                                             column(width = 5, plotOutput("tfidf_service_plot"))
                                           )),
                                           tabPanel("Tfidf - #price",  fluidRow(
                                             column(width = 6, dataTableOutput("tfidf_price")),
                                             column(width = 1, "" ),
                                             column(width = 5, plotOutput("tfidf_price_plot"))
                                           )),
                                           tabPanel("Tfidf - #handling",  fluidRow(
                                             column(width = 6, dataTableOutput("tfidf_handling")),
                                             column(width = 1, "" ),
                                             column(width = 5, plotOutput("tfidf_handling_plot"))
                                           )),
                                           tabPanel("Tfidf - #interior",  fluidRow(
                                             column(width = 6, dataTableOutput("tfidf_interior")),
                                             column(width = 1, "" ),
                                             column(width = 5, plotOutput("tfidf_interior_plot"))
                                           ))
                                           
                               ))))
                         
             )
  )


