library(tidyverse)
library(shiny)
library(tidytext)
library(wordcloud)
library(textdata)
library(topicmodels)
library(scales)
library(tm)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # reactive DF for chapter tidied into format for word cloud / sentiment analysis
  tidy_chapter <- reactive({

    # Read in the specified chapter
    chapter_num <- readr::parse_number(input$chosen_chapter)
    selected_chapter_text <- readLines(paste0("chapter_files/", input$chosen_chapter))
    # Remove dashes from specified chapter
    selected_chapter_text <- remove_dashes(selected_chapter_text)

    # condense chapter to specified lines
    selected_lines <- selected_chapter_text[input$line_range[1]:input$line_range[2]]

    # Unnest tokens from chapter text
    chapter_text <- tibble(text = selected_lines) %>%
      mutate(
        linenumber = row_number(),
        chapter = chapter_num
      )

    data <- chapter_text %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words)

    return(data)
  })

  # reactive DF specifically for the AFINN sentiment library
  afinn <- reactive({
    tidy_chapter() %>%
      inner_join(get_sentiments("afinn")) %>%
      mutate(gen_val = case_when(
        value > 0 ~ "positive",
        value < 0 ~ "negative"
      ))
  })

  # the raw text from the specified chapter / lines
  chapter_text <- reactive({

    # Read in the specified chapter
    chapter_num <- readr::parse_number(input$chosen_chapter)
    selected_chapter_text <- readLines(paste0("chapter_files/", input$chosen_chapter))
    # Remove dashes from specified chapter
    selected_chapter_text <- remove_dashes(selected_chapter_text)

    # condense chapter to specified lines
    selected_lines <- selected_chapter_text[input$line_range[1]:input$line_range[2]]
    return(selected_lines)
  })

  # keep track of how many lines are in a selected chapter
  max_line_number <- reactive({
    # Read in the specified chapter
    chapter_num <- readr::parse_number(input$chosen_chapter)
    selected_chapter_text <- readLines(paste0("chapter_files/", input$chosen_chapter))
    # Remove dashes from specified chapter
    selected_chapter_text <- remove_dashes(selected_chapter_text)
    # return number of lines in the chapter
    return(length(selected_chapter_text))
  })

  # create dynamic slider based on number of lines in chapter
  output$slider_input <- renderUI({
    sliderInput(
      inputId = "line_range",
      label = "Select a range of lines to analyze:",
      min = 1,
      max = max_line_number(),
      value = c(1, max_line_number()),
      step = 1,
      dragRange = TRUE,
      width = "95%"
    )
  })

  # print selected chapter text
  output$text_output <- renderText(
    expr = chapter_text(),
    sep = "\n"
  )

  color_palette <- brewer.pal(5, "Purples")

  # word cloud output
  output$wordcloud <- renderPlot(
    {
      if (input$switch_sentiment == "Yes") {
        req(input$chosen_sentiment_library)

        if (input$chosen_sentiment_library == "bing") {
          validate(
            need(
              length(unique(inner_join(tidy_chapter(), get_sentiments("bing"))$sentiment)) > 1,
              "Need at least two words of differing sentiment that are in Bing sentiment library. Please increase the line range."
            )
          )

          tidy_chapter() %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 0) %>%
            comparison.cloud(
              max.words = 250, title.size = 1.2,
              random.order = FALSE, colors = c("orange", "turquoise")
            )
        }
        else if (input$chosen_sentiment_library == "nrc") {
          validate(
            need(
              length(unique(inner_join(tidy_chapter(), get_sentiments("nrc"))$sentiment)) > 2,
              "Need at least three words of differing sentiment that are in NRC sentiment library. Please increase the line range."
            )
          )

          tidy_chapter() %>%
            inner_join(get_sentiments("nrc")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 0) %>%
            comparison.cloud(
              max.words = 500, random.order = FALSE,
              title.size = 1.2
            )
        }
        else if (input$chosen_sentiment_library == "afinn") {
          validate(
            need(
              length(unique(afinn()$gen_val)) > 1,
              "Need at least two words of differing sentiment that are in AFINN sentiment library. Please increase the line range."
            )
          )

          afinn() %>%
            count(word, gen_val, sort = TRUE) %>%
            acast(word ~ gen_val, value.var = "n", fill = 0) %>%
            comparison.cloud(
              max.words = 500, random.order = FALSE,
              title.size = 1.2,
              colors = c("orange", "turquoise")
            )
        }
      }
      else if (input$switch_sentiment == "No") {
        tidy_chapter() %>%
          anti_join(stop_words) %>%
          count(word) %>%
          with(wordcloud(word, n,
            max.words = 20,
            colors = color_palette
          ))
      }
    },
    height = 700
  )

  # sentiment analysis plot
  output$sentiment_plot <- renderPlot({
    req(input$chosen_sentiment_library)

    if (input$chosen_sentiment_library == "bing") {
      bing_word_counts <- tidy_chapter() %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()

      bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(
          word = reorder(word, n),
          n = as.factor(n)
        ) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(
          y = "Frequency",
          x = "Term",
          title = paste(
            "Sentiment of terms in Chapter",
            parse_number(input$chosen_chapter),
            "from the Bing library"
          )
        ) +
        coord_flip()
    }

    else if (input$chosen_sentiment_library == "nrc") {
      nrc_word_counts <- tidy_chapter() %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment) %>%
        slice(1:5)

      ggplot(data = nrc_word_counts, aes(
        x = reorder(word, n),
        y = as.factor(n), fill = sentiment
      )) +
        geom_col(show.legend = FALSE) +
        labs(
          y = "Frequency",
          x = "Term",
          title = paste(
            "Sentiment of terms in Chapter",
            parse_number(input$chosen_chapter),
            "from the NRC library"
          )
        ) +
        facet_wrap(~sentiment, ncol = 2, scales = "free") +
        coord_flip()
    }

    else if (input$chosen_sentiment_library == "afinn") {
      afinn_word_counts <- afinn() %>%
        count(word, gen_val, sort = TRUE) %>%
        ungroup()

      afinn_word_counts %>%
        group_by(gen_val) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(
          word = reorder(word, n),
          n = as.factor(n)
        ) %>%
        ggplot(aes(word, n, fill = gen_val)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~gen_val, scales = "free_y") +
        labs(
          y = "Frequency",
          x = "Term",
          title = paste(
            "Sentiment of terms in Chapter",
            parse_number(input$chosen_chapter),
            "from the AFINN library"
          )
        ) +
        coord_flip()
    }
  })

  # topic model plot
  output$topic_plot <- renderPlot({
    req(input$chosen_chapter)

    # Tidy data for topic modeling
    text_corpus <- SimpleCorpus(VectorSource(tidy_chapter()))

    dtm <- DocumentTermMatrix(text_corpus,
      control = list(
        tolower = TRUE,
        removePunctuation = TRUE,
        removeNumbers = TRUE,
        stopwords = TRUE,
        sparse = TRUE
      )
    )

    rowTotals <- apply(dtm, 1, sum) # Find the sum of words in each Document
    dtm.new <- dtm[rowTotals > 0, ] # remove all docs without words

    top_mod <- LDA(dtm.new, k = 2, method = "Gibbs", control = NULL)


    # top_mod <- topicmodels::LDA(dtm, k = 2, control = list(seed = 495))
    ap_topics <- tidy(top_mod, matrix = "beta")

    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    ap_top_terms$topic <- paste("Topic", ap_top_terms$topic)


    ap_top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free") +
      coord_flip() +
      scale_x_reordered() +
      labs(
        y = "Probability of belonging to topic",
        x = "Term",
        title = paste("Topic modeling for Chapter", parse_number(input$chosen_chapter))
      )
  })
}