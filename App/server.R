#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tm) 
library(dplyr)


# Load the n-gram df
n.gram.files <- c('https://raw.githubusercontent.com/nthehai01/swiftkey/main/Data/n-gram/uni-gram.csv',
                  'https://raw.githubusercontent.com/nthehai01/swiftkey/main/Data/n-gram/bi-gram.csv',
                  'https://raw.githubusercontent.com/nthehai01/swiftkey/main/Data/n-gram/tri-gram.csv')
ngram.matrices <- lapply(n.gram.files, read.csv, header=TRUE)
n = length(ngram.matrices) - 1


# Get number of words in a sentence
len <- function(sentence) {
  s = strsplit(sentence, split=" ")[[1]]
  return(length(s))
}


# Sentence preprocessing
preprocessor <- function(sentence) {
  corpus <- VCorpus(VectorSource(sentence)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower))
  
  return(corpus[[1]]$content)
}


# Get the last n words
getLastWord <- function(sentence, n) {
  s = strsplit(sentence, split=" ")[[1]]
  res <- s[(length(s) - n + 1):length(s)]
  res <- paste(res, collapse = ' ')
  return(res)
}


# N-gram model
# Return the next most probable word
ngram.model <- function(ngram.matrix, lastWords.regex) {
  nextWord.set <- ngram.matrix[grepl(lastWords.regex, ngram.matrix$Word),]
  if(dim(nextWord.set)[1] == 0) 
    return(NULL)
  
  nextWord <- nextWord.set[which.max(nextWord.set$frequency), 1]
  nextWord <- word(nextWord, -1)
  
  return(nextWord)
}


# Get the next word
predictWord <- function(sentence, i, n.gram) {
  # get the last i words
  lastWords <- getLastWord(sentence, i)
  
  # Get the regex pattern of the last word(s) 
  # Using regex starts with
  lastWords.regex <- paste("^", lastWords, " ", sep="")
  
  # predict the next word
  nextWord <- ngram.model(n.gram, lastWords.regex)
  return(nextWord)
}


# Get the next word based on the context of the given sentence
predictWord.wrapper <- function(sentence, ngram.matrices, n) {
  # If the len of sentence is zero, predict the next word using uni-gram
  if(len(sentence) == 0) {
    uni.gram <- ngram.matrices[[1]]
    nextWord <- uni.gram[which.max(uni.gram$frequency), 1]
    
    return(nextWord)
  }
  
  # If the len of sentence is 1, predict the next word using bi-gram
  if(len(sentence) == 1) {
    bi.gram <- ngram.matrices[[2]]
    nextWord <- predictWord(sentence, 1, bi.gram)
    return(nextWord)
  }  
  
  # If the len of sentence is greater than 1, predict the next word using tri-gram.
  # But if tri-gram model can not predict the next word, use the bi-model
  tri.gram <- ngram.matrices[[3]]
  nextWord <- predictWord(sentence, 2, tri.gram)
  if(is.null(nextWord) == FALSE)
    return(nextWord)
  
  bi.gram <- ngram.matrices[[2]]
  nextWord <- predictWord(sentence, 1, bi.gram)
  return(nextWord)
}


shinyServer(function(input, output) {
  output$nextWord <- renderText({
    sentence <- preprocessor(input$sentence)
    nextWord <- predictWord.wrapper(sentence, ngram.matrices, n)
    
    ifelse(is.null(nextWord), "<unknown>", nextWord)
  })
})
