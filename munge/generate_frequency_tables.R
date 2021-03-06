# generate frequency tables:
library(tidyr)
library(dplyr)
library(pryr)
library(data.table)
library(tidytext)
library(readr)
library(quanteda)
library(readtext)
library(stringi)


preprocess_texts <- function(folder, convert_to_ascii = FALSE) {
  textdata <- readtext::readtext(paste0(folder, "*.txt"))
  
  if (convert_to_ascii){
    textdata <- textdata %>% 
      stri_trans_general("latin-ascii") %>% 
      char_tolower()
  }
  else {
    textdata <- textdata %>% 
      stri_c() %>% 
      char_tolower()
  }

  #remove URLs
  URLREGEX <- "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
  textdata <- stri_replace_all_regex(textdata, URLREGEX, "")
  
  #remove symbols
  textdata <- stri_replace_all_charclass(textdata, "[\\p{S}]", "")
  
  #remove numbers
  textdata <- stri_replace_all_regex(textdata, "\\b\\d+\\b", "")
}


compute_tidy_ngrams <- function(folder, textdata, use_6_ngrams=FALSE) {
  tok_sentences <- textdata %>% quanteda::corpus() %>% quanteda::tokenize(what = "sentence",
                                         remove_numbers = TRUE,
                                         remove_punct = TRUE,
                                         remove_symbols = TRUE,
                                         remove_separators = TRUE,
                                         remove_twitter = TRUE,
                                         remove_url = TRUE,
                                         verbose = TRUE,
                                         simplify = TRUE)
  df_tok_sentences <- data_frame(text = tok_sentences)
  rm(textdata, tok_sentences)
  gc()
  tok <- df_tok_sentences %>% unnest_tokens(ngram, text) %>% count(ngram, sort = TRUE)
  saveRDS(tok, paste0(folder, "1-grams_tidy.rds"))
  for (i in 2:ifelse(use_6_ngrams, 6, 5)) {
    elapsedtime <- system.time({
      tok <- df_tok_sentences %>% unnest_tokens(ngram, text, token = "ngrams", n = i) %>% count(ngram, sort = TRUE)
    })
    saveRDS(tok, paste0(folder, i, "-grams_tidy.rds"))
    print(paste0(i, "-grams generated with:"))
    print(elapsedtime)
    rm(tok)
    gc()
  }
  return(TRUE)
}


compute_sb_ngrams <- function(folder, use_6_ngrams=FALSE){
  source('munge/compute_score_stupid_backoff.R')
  
  ngrams_1 <<- readRDS(paste0(folder, "1-grams_tidy.rds"))
  ngrams_2 <<- readRDS(paste0(folder, "2-grams_tidy.rds"))
  ngrams_3 <<- readRDS(paste0(folder, "3-grams_tidy.rds"))
  ngrams_4 <<- readRDS(paste0(folder, "4-grams_tidy.rds"))
  ngrams_5 <<- readRDS(paste0(folder, "5-grams_tidy.rds"))
  if(use_6_ngrams){
    ngrams_6 <<- readRDS(paste0(folder, "6-grams_tidy.rds"))
    setDT(ngrams_6)
    ngrams_6[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
    ngrams_6 <- ngrams_6 %>% separate(ngram, c("word1", "word2", "word3", "word4", "word5", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, word4, word5, sep = " ")
    setDT(ngrams_6)
    setkey(ngrams_6, ngram)
    saveRDS(ngrams_6, paste0(folder, "6-grams_DT_sb.rds"))
    rm(ngrams_6)
  }
  setDT(ngrams_5)
  setDT(ngrams_4)
  setDT(ngrams_3)
  setDT(ngrams_2)
  setDT(ngrams_1)
  
  ngrams_5[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
  ngrams_4[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
  ngrams_3[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
  ngrams_2[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
  ngrams_1[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
  
  ngrams_5 <<- ngrams_5 %>% separate(ngram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, word4, sep = " ")
  setDT(ngrams_5)
  setkey(ngrams_5, ngram) 
  saveRDS(ngrams_5, paste0(folder, "5-grams_DT_sb.rds"))
  rm(ngrams_5)
  
  ngrams_4 <<- ngrams_4 %>% separate(ngram, c("word1", "word2", "word3", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, sep = " ")
  setDT(ngrams_4)  
  setkey(ngrams_4, ngram)
  saveRDS(ngrams_4, paste0(folder, "4-grams_DT_sb.rds"))
  rm(ngrams_4)
  
  ngrams_3 <<- ngrams_3 %>% separate(ngram, c("word1", "word2", "word"), sep = " ") %>% unite(ngram, word1, word2, sep = " ")
  setDT(ngrams_3)
  setkey(ngrams_3, ngram)
  saveRDS(ngrams_3, paste0(folder, "3-grams_DT_sb.rds"))
  rm(ngrams_3)
  
  ngrams_2 <<- ngrams_2 %>% separate(ngram, c("ngram", "word"), sep = " ")
  setDT(ngrams_2)
  setkey(ngrams_2, ngram)
  saveRDS(ngrams_2, paste0(folder, "2-grams_DT_sb.rds"))
  rm(ngrams_2)
  
  # no need for ngrams_1 adjustments... already only one word...
  setDT(ngrams_1)
  setkey(ngrams_1, ngram)
  saveRDS(ngrams_1, paste0(folder, "1-grams_DT_sb.rds"))
  rm(ngrams_1)
  
  return(TRUE)
}



###############################################################################
compute_reduced_ngrams_threshold <- function(folder,
                                             use_6_ngrams=FALSE,
                                             number_of_ngrams_1=50,
                                             threshold_ngrams_2=5,
                                             threshold_ngrams_3=5,
                                             threshold_ngrams_4=5,
                                             threshold_ngrams_5=5,
                                             threshold_ngrams_6=5){
  threshold_ngrams_i <- c(NA, 
                          threshold_ngrams_2,
                          threshold_ngrams_3,
                          threshold_ngrams_4,
                          threshold_ngrams_5,
                          threshold_ngrams_6)
  
  ngrams_1 <- readRDS(paste0(folder, 1, "-grams_DT_sb_mkn.rds"))
  ngrams_1_nrow_before <- nrow(ngrams_1)
  ngrams_1_size_before <- as.numeric(object_size(ngrams_1))
  ngrams_1 <- ngrams_1 %>% arrange(desc(mkn)) %>% head(number_of_ngrams_1)
  ngrams_1_nrow_after <- nrow(ngrams_1)
  ngrams_1_size_after <- as.numeric(object_size(ngrams_1))
  setDT(ngrams_1)
  setkey(ngrams_1, ngram)
  setorder(ngrams_1, -mkn)
  saveRDS(ngrams_1, paste0(folder, "1-grams_DT_sb_mkn_threshold.rds"))

  reductions <- data.frame(ngrams=1, 
                           threshold=number_of_ngrams_1,
                           nrows_before_reduction=ngrams_1_nrow_before, 
                           nrows_after_reduction=ngrams_1_nrow_after,
                           size_before=ngrams_1_size_before,
                           size_after=ngrams_1_size_after)
  
  for(i in 2:ifelse(use_6_ngrams, 6, 5)){
    ngrams_i <- readRDS(paste0(folder, i, "-grams_DT_sb_mkn.rds"))
    ngrams_i_nrow_before <- nrow(ngrams_i)
    ngrams_i_size_before <- as.numeric(object_size(ngrams_i))
    ngrams_i <- ngrams_i %>% filter(n>=threshold_ngrams_i[i])
    ngrams_i_nrow_after <- nrow(ngrams_i)
    ngrams_i_size_after <- as.numeric(object_size(ngrams_i))
    setDT(ngrams_i)
    setkey(ngrams_i, ngram)
    setorder(ngrams_i, ngram, -mkn)
    saveRDS(ngrams_i, paste0(folder, i, "-grams_DT_sb_mkn_threshold.rds"))
    
    reductions <- rbind(reductions, c(i, threshold_ngrams_i[i], 
                        ngrams_i_nrow_before, ngrams_i_nrow_after,
                        ngrams_i_size_before, ngrams_i_size_after))
  }
  return(reductions)
}



###############################################################################
compute_reduced_ngrams_suggestions <- function(folder,
                                               use_6_ngrams=FALSE,
                                               number_of_suggestions=5){
  
  reductions <- data.frame(ngrams=1, 
                           number_of_suggestions=number_of_suggestions,
                           nrows_before_reduction=NA_integer_, 
                           nrows_after_reduction=NA_integer_,
                           size_before=NA_real_,
                           size_after=NA_real_)
  ngrams_1 <- readRDS(paste0(folder, 1, "-grams_DT_sb_mkn_threshold.rds"))
  saveRDS(ngrams_1, paste0(folder, 1, "-grams_DT_sb_mkn_threshold_reduced.rds"))
  
  for(i in 2:ifelse(use_6_ngrams, 6, 5)){
    ngrams_i <- readRDS(paste0(folder, i, "-grams_DT_sb_mkn_threshold.rds"))
    ngrams_i_nrow_before <- nrow(ngrams_i)
    setDT(ngrams_i)
    ngrams_i_size_before <- as.numeric(object_size(ngrams_i))
    ngrams_i <- ngrams_i[ngrams_i[, head(.I, number_of_suggestions), by=ngram]$V1]
    ngrams_i_nrow_after <- nrow(ngrams_i)
    setDT(ngrams_i)
    ngrams_i_size_after <- as.numeric(object_size(ngrams_i))
    saveRDS(ngrams_i, paste0(folder, i, "-grams_DT_sb_mkn_threshold_reduced.rds"))
    
    reductions <- rbind(reductions, c(i, number_of_suggestions, 
                        ngrams_i_nrow_before, ngrams_i_nrow_after,
                        ngrams_i_size_before, ngrams_i_size_after))
  }
  return(reductions)
}