library(tidyverse)
library(shiny)

# Initial code -----------------------------------------------------------------
# generate
location <- "chapter_files/"
chapter_names <- list.files(location)

# a function to remove dashes from the ends of line (and combine the truncated words)
# NOTE: this function won't work if the chapter passed in ends in a hyphen,
#       but this shouldn't be an issue
remove_dashes <- function(lines) {

  # iterate through all of our lines
  for (line in 1:(length(lines) - 1)) {

    # if the current line we're on is blank then skip it
    if (lines[line] == " " | lines[line] == "") {
      next
    }

    # otherwise, we need to check for a dash at the end of the line
    # keep track of the last word in the current line
    last_word <- word(lines[line], start = -3)

    # if the line that  we're on ends in a dash, then we want to remove it
    if (substr(last_word, nchar(last_word), nchar(last_word)) == "-") {

      # if the next line is blank, then we need to skip it and check the
      # next line for the first word
      if (lines[line + 1] == " " | lines[line + 1] == "") {
        # keep track of the first word
        first_word_next_line <- word(lines[line + 2], start = 1)
      }
      # otherwise, the first word is on the next line
      else {
        first_word_next_line <- word(lines[line + 1], start = 1)
      }

      # keep track of the beginning of the current line
      # (not including the truncated word at the end)
      beginning_of_line <- word(lines[line], end = -4)
      # remove the dash from the last (truncated) word
      word_without_dash <- gsub("-", "", last_word)
      # update the current line so that the dash is gone and the word from
      # the line below is added on
      lines[line] <- paste0(beginning_of_line, " ", word_without_dash, first_word_next_line)

      # now, we need to remove the first part of the word from the line below
      # so check again to see if the next line is blank
      if (lines[line + 1] == " " | lines[line + 1] == "") {
        # if it is, we need to update the line two lines down
        # this gets rid of only the first word in the specified line
        lines[line + 2] <- word(lines[line + 2],
          start = 2:nchar(lines[line + 2]),
          end = -1
        )[1]
      }

      # otherwise we just update the line one down
      else {
        lines[line + 1] <- word(lines[line + 1],
          start = 2:nchar(lines[line + 1]),
          end = -1
        )[1]
      }
    } # end if statement that checks for a dash
  } # end for loop

  # return our updated lines with removed dashes
  lines
} # end function

# load sentiment analysis datasets
data(stop_words)
