################## LOAD PACKAGES ####################
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinybusy)
library(mdsr)
library(shinycssloaders)
library(wordcloud)
library(reshape2)


# UI  --------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),

  navbarPage(title = "History of Amherst — Sentiment Analysis"),

  # Row 1 (inputs)
  fluidRow(
    column(
      width = 3,
      img(src = "amherst_logo.png", height = 100),
    ),
    column(
      3,
      selectInput(
        inputId = "chosen_sentiment_library",
        label = "Choose sentiment library: ",
        choices = c(
          "Bing" = "bing",
          "NRC" = "nrc",
          "AFINN" = "afinn"
        )
      )
    ),
    column(
      width = 3,
      selectInput(
        inputId = "chosen_chapter",
        label = "Choose a chapter: ",
        choices = c(
          "Preface" = "chapter00.txt",
          "Chapter 1" = "chapter01.txt",
          "Chapter 2" = "chapter02.txt",
          "Chapter 3" = "chapter03.txt",
          "Chapter 4" = "chapter04.txt",
          "Chapter 5" = "chapter05.txt",
          "Chapter 6" = "chapter06.txt",
          "Chapter 7" = "chapter07.txt",
          "Chapter 8" = "chapter08.txt",
          "Chapter 9" = "chapter09.txt",
          "Chapter 10" = "chapter10.txt",
          "Chapter 11" = "chapter11.txt",
          "Chapter 12" = "chapter12.txt",
          "Chapter 13" = "chapter13.txt",
          "Chapter 14" = "chapter14.txt",
          "Chapter 15" = "chapter15.txt",
          "Chapter 16" = "chapter16.txt",
          "Chapter 17" = "chapter17.txt",
          "Chapter 18" = "chapter18.txt",
          "Chapter 19" = "chapter19.txt",
          "Chapter 20" = "chapter20.txt",
          "Chapter 21" = "chapter21.txt",
          "Chapter 22" = "chapter22.txt",
          "Chapter 23" = "chapter23.txt",
          "Chapter 24" = "chapter24.txt",
          "Chapter 25" = "chapter25.txt",
          "Chapter 26" = "chapter26.txt",
          "Chapter 27" = "chapter27.txt",
          "Chapter 28" = "chapter28.txt"
        ),
        selected = "chapter01.txt"
      )
    ),
    column(
      width = 3,
      radioButtons(
        input = "switch_sentiment",
        label = "Show sentiment in word cloud?",
        choices = c(
          "Yes",
          "No"
        ),
        selected = "Yes"
      )
    ),
    align = "center"
  ),

  # Row 3 (slider input)
  fluidRow(
    column(
      width = 9,
      uiOutput(outputId = "slider_input")
    ),
    column(width = 3,
           helpText("To show the selected sentiment analysis in the word cloud, select 'yes.'")),
    align = "center"
  ),

  hr(),

  # Row 4 (text output and plots)
  fluidRow(
    # column to output text
    column(
      width = 5,
      h3("Selected Chapter Text"),
      shinycssloaders::withSpinner(
        verbatimTextOutput(
          outputId = "text_output"
        )
      )
    ),
    # make font size smaller for text output
    tags$head(tags$style(HTML("#text_output { font-size: 13.5px; 
                              font-family: Times; overflow-y: scroll; 
                              max-height: 425px; background: ghostwhite;}"))),

    # column to output plots (as tabs)
    column(
      width = 7,
      shinycssloaders::withSpinner(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Word Cloud",
            shinycssloaders::withSpinner(
              plotOutput("wordcloud")
            )
          ),
          tabPanel(
            "Sentiment Analysis Plot",
            shinycssloaders::withSpinner(
              plotOutput("sentiment_plot")
            )
          ),
          tabPanel(
            "Topic Modeling",
            shinycssloaders::withSpinner(
              plotOutput("topic_plot")
            )
          )
        )
      ),
      style = "padding:0 10px 0px 0px;"
    ),

    tags$head(tags$style(HTML("#wordcloud { overflow-y: scroll;
                                            overflow-x: scroll; }"))),
  ) # end row 4
) # end UI
