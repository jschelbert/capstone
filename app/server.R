#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table)
library(dplyr)
library(shiny)
library(markdown)

## source prediction function
source("predict_stupid_backoff.R")

## load the n-gram data
folder <- "data/en_US/"

folder_en <- "data/en_US/"
folder_de <- "data/de_DE/"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observeEvent(input$language, {
        if (input$language == "English") {
            folder <- folder_en
        }
        else if (input$language == "German") {
            folder <- folder_de
        }
        ngrams_5 <<- readRDS(paste0(folder, "5-grams_DT_sb_mkn_threshold_reduced.rds"))
        ngrams_4 <<- readRDS(paste0(folder, "4-grams_DT_sb_mkn_threshold_reduced.rds"))
        ngrams_3 <<- readRDS(paste0(folder, "3-grams_DT_sb_mkn_threshold_reduced.rds"))
        ngrams_2 <<- readRDS(paste0(folder, "2-grams_DT_sb_mkn_threshold_reduced.rds"))
        ngrams_1 <<- readRDS(paste0(folder, "1-grams_DT_sb_mkn_threshold_reduced.rds"))

        setDT(ngrams_5)
        setDT(ngrams_4)
        setDT(ngrams_3)
        setDT(ngrams_2)
        setDT(ngrams_1)

        setkey(ngrams_5, ngram)
        setkey(ngrams_4, ngram)
        setkey(ngrams_3, ngram)
        setkey(ngrams_2, ngram)
        setkey(ngrams_1, ngram)

        setorder(ngrams_5, ngram, -mkn)
        setorder(ngrams_4, ngram, -mkn)
        setorder(ngrams_3, ngram, -mkn)
        setorder(ngrams_2, ngram, -mkn)
        setorder(ngrams_1, -mkn)

        setkey(ngrams_5, ngram)
        setkey(ngrams_4, ngram)
        setkey(ngrams_3, ngram)
        setkey(ngrams_2, ngram)
        setkey(ngrams_1, ngram)

        setorder(ngrams_5, ngram, -mkn)
        setorder(ngrams_4, ngram, -mkn)
        setorder(ngrams_3, ngram, -mkn)
        setorder(ngrams_2, ngram, -mkn)
        setorder(ngrams_1, -mkn)

        smearwords <<- fread(paste0(folder,"smearwords.txt"), sep = ";") %>% .[[1]]
    })

    # Prediction ----------------------------------------------------------------------------------
    output$predictionvalue_sb <- renderTable({
        prediction_table <- predict_sb_mkn_DT(input$usertext)
        if (input$smearwordfilter) {
            prediction_table <- prediction_table %>% filter(!(word %in% smearwords))
        }
        prediction_table
    },
    digits = 5,
    hover = TRUE,
    align = 'lcc',
    spacing = "m",
    width = "40%")

    # Random Babble -------------------------------------------------------------------------------
    output$random_babble <- renderText({
        babble <- vector(mode = "character", length = input$numwordsbabble)
        randomseed <- input$randomseed
        if (is.na(input$randomseed) | is.null(input$randomseed)) {
            randomseed <- sample(ngrams_1[, ngram],
                                 size = 1,
                                 prob = ngrams_1[, mkn])
        }
        babble[1] <- randomseed
        for (i in 2:input$numwordsbabble) {
            next_word_table <- predict_sb_mkn_DT(paste(babble[1:i], collapse = " "))
            babble[i] <- sample(next_word_table[, word],
                                size = 1,
                                prob = next_word_table[, mkn])
        }
        paste(babble, collapse = " ")
    })

})
