# generate frequency tables:
library(tidyr)
library(dplyr)
library(data.table)
library(tidytext)
library(quanteda)
#folder <- "data/samples_small/en_US/"
#folder <- "data/samples/en_US/"
folder <- "data/final/en_US/"

textdata <- readtext::readtext(paste0(folder, "*.txt")) %>% quanteda::corpus()
tok_sentences <- textdata %>% tokenize(what="sentence", remove_twitter = TRUE, verbose=TRUE, simplify=TRUE)
df_tok_sentences <- data_frame(text = tok_sentences)
rm(textdata, tok_sentences)
gc()
tok <-  df_tok_sentences %>% unnest_tokens(ngram, text) %>% count(ngram, sort = TRUE) 
saveRDS(tok, paste0(folder, "1-grams_tidy.rds"))
for(i in 2:6){
  elapsedtime <- system.time({
    tok <- df_tok_sentences %>% unnest_tokens(ngram, text, token="ngrams", n=i) %>% count(ngram, sort = TRUE)
  })
  saveRDS(tok, paste0(folder, i, "-grams_tidy.rds"))
  print(paste0(i,"-grams generated with:"))
  print(elapsedtime)
  rm(tok)
  gc()
}

#tok <- readRDS(paste0(folder, i, "-grams.rds"))
ngrams_1 <- readRDS(paste0(folder, "1-grams_tidy.rds"))
ngrams_2 <- readRDS(paste0(folder, "2-grams_tidy.rds"))
ngrams_3 <- readRDS(paste0(folder, "3-grams_tidy.rds"))
ngrams_4 <- readRDS(paste0(folder, "4-grams_tidy.rds"))
ngrams_5 <- readRDS(paste0(folder, "5-grams_tidy.rds"))
ngrams_6 <- readRDS(paste0(folder, "6-grams_tidy.rds"))


threshold_ngrams_1 <- 1
threshold_ngrams_2 <- 4
threshold_ngrams_3 <- 4
threshold_ngrams_4 <- 4
threshold_ngrams_5 <- 4

ngrams_1_nrow_before <- nrow(ngrams_1)
ngrams_1 <- ngrams_1 %>% filter(n>threshold_ngrams_1)
ngrams_1_nrow_after <- nrow(ngrams_1)
compression_ngrams_1 <- ngrams_1_nrow_after/ngrams_1_nrow_before

ngrams_2_nrow_before <- nrow(ngrams_2)
ngrams_2 <- ngrams_2 %>% filter(n>threshold_ngrams_2)
ngrams_2_nrow_after <- nrow(ngrams_2)
compression_ngrams_2 <- ngrams_2_nrow_after/ngrams_2_nrow_before

ngrams_3_nrow_before <- nrow(ngrams_3)
ngrams_3 <- ngrams_3 %>% filter(n>threshold_ngrams_3)
ngrams_3_nrow_after <- nrow(ngrams_3)
compression_ngrams_3 <- ngrams_3_nrow_after/ngrams_3_nrow_before

ngrams_4_nrow_before <- nrow(ngrams_4)
ngrams_4 <- ngrams_4 %>% filter(n>threshold_ngrams_4)
ngrams_4_nrow_after <- nrow(ngrams_4)
compression_ngrams_4 <- ngrams_4_nrow_after/ngrams_4_nrow_before

ngrams_5_nrow_before <- nrow(ngrams_5)
ngrams_5 <- ngrams_5 %>% filter(n>threshold_ngrams_5)
ngrams_5_nrow_after <- nrow(ngrams_5)
compression_ngrams_5 <- ngrams_5_nrow_after/ngrams_5_nrow_before


source('munge/compute_score_stupid_backoff.R')

ngrams_5 <- ngrams_5 %>% mutate(sb_score=compute_score_stupid_backoff(ngram))
ngrams_4 <- ngrams_4 %>% mutate(sb_score=compute_score_stupid_backoff(ngram))
ngrams_3 <- ngrams_3 %>% mutate(sb_score=compute_score_stupid_backoff(ngram))
ngrams_2 <- ngrams_2 %>% mutate(sb_score=compute_score_stupid_backoff(ngram))
ngrams_1 <- ngrams_1 %>% mutate(sb_score=compute_score_stupid_backoff(ngram))

ngrams_5 <- ngrams_5 %>% separate(ngram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, word4, sep = " ") %>% as.data.table()
ngrams_4 <- ngrams_4 %>% separate(ngram, c("word1", "word2", "word3", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, sep = " ") %>% as.data.table()
ngrams_3 <- ngrams_3 %>% separate(ngram, c("word1", "word2", "word"), sep = " ") %>% unite(ngram, word1, word2, sep = " ") %>% as.data.table()
ngrams_2 <- ngrams_2 %>% separate(ngram, c("ngram", "word"), sep = " ") %>% as.data.table()
ngrams_1 <- ngrams_1 %>% as.data.table()




