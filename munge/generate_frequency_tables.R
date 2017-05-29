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
tok_sentences <- textdata %>% tokenize(what="sentence",remove_numbers = TRUE,
                                       remove_punct = TRUE, remove_symbols = TRUE, 
                                       remove_separators = TRUE, remove_twitter = TRUE, 
                                       verbose=TRUE, simplify=TRUE)
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



ngrams_1 <- readRDS(paste0(folder, "1-grams_tidy.rds"))
ngrams_2 <- readRDS(paste0(folder, "2-grams_tidy.rds"))
ngrams_3 <- readRDS(paste0(folder, "3-grams_tidy.rds"))
ngrams_4 <- readRDS(paste0(folder, "4-grams_tidy.rds"))
ngrams_5 <- readRDS(paste0(folder, "5-grams_tidy.rds"))

source('munge/compute_score_stupid_backoff.R')

## usage of data.table package
ngrams_5 <- ngrams_5 %>% as.data.table()
ngrams_4 <- ngrams_4 %>% as.data.table()
ngrams_3 <- ngrams_3 %>% as.data.table()
ngrams_2 <- ngrams_2 %>% as.data.table()
ngrams_1 <- ngrams_1 %>% as.data.table()

ngrams_5[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_4[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_3[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_2[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_1[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]

ngrams_5 <- ngrams_5 %>% separate(ngram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, word4, sep = " ")
ngrams_4 <- ngrams_4 %>% separate(ngram, c("word1", "word2", "word3", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, sep = " ")
ngrams_3 <- ngrams_3 %>% separate(ngram, c("word1", "word2", "word"), sep = " ") %>% unite(ngram, word1, word2, sep = " ")
ngrams_2 <- ngrams_2 %>% separate(ngram, c("ngram", "word"), sep = " ")
# no need for ngrams_1 adjustments... already only one word...

setkey(ngrams_5, ngram)
setkey(ngrams_4, ngram)
setkey(ngrams_3, ngram)
setkey(ngrams_2, ngram)
setkey(ngrams_1, ngram)

saveRDS(ngrams_5, paste0(folder, "5-grams_DT_sb.rds"))
saveRDS(ngrams_4, paste0(folder, "4-grams_DT_sb.rds"))
saveRDS(ngrams_3, paste0(folder, "3-grams_DT_sb.rds"))
saveRDS(ngrams_2, paste0(folder, "2-grams_DT_sb.rds"))
saveRDS(ngrams_1, paste0(folder, "1-grams_DT_sb.rds"))


threshold_ngrams_2 <- 5
threshold_ngrams_3 <- 5
threshold_ngrams_4 <- 5
threshold_ngrams_5 <- 5

ngrams_1_nrow_before <- nrow(ngrams_1)
ngrams_1 <- ngrams_1 %>% arrange(desc(sb_score)) %>% head(50)
ngrams_1_nrow_after <- nrow(ngrams_1)
compression_ngrams_1 <- ngrams_1_nrow_after/ngrams_1_nrow_before

ngrams_2_nrow_before <- nrow(ngrams_2)
ngrams_2 <- ngrams_2 %>% filter(n>=threshold_ngrams_2) %>% arrange(desc(sb_score))
ngrams_2_nrow_after <- nrow(ngrams_2)
compression_ngrams_2 <- ngrams_2_nrow_after/ngrams_2_nrow_before

ngrams_3_nrow_before <- nrow(ngrams_3)
ngrams_3 <- ngrams_3 %>% filter(n>=threshold_ngrams_3) %>% arrange(desc(sb_score))
ngrams_3_nrow_after <- nrow(ngrams_3)
compression_ngrams_3 <- ngrams_3_nrow_after/ngrams_3_nrow_before

ngrams_4_nrow_before <- nrow(ngrams_4)
ngrams_4 <- ngrams_4 %>% filter(n>=threshold_ngrams_4) %>% arrange(desc(sb_score))
ngrams_4_nrow_after <- nrow(ngrams_4)
compression_ngrams_4 <- ngrams_4_nrow_after/ngrams_4_nrow_before

ngrams_5_nrow_before <- nrow(ngrams_5)
ngrams_5 <- ngrams_5 %>% filter(n>=threshold_ngrams_5) %>% arrange(desc(sb_score))
ngrams_5_nrow_after <- nrow(ngrams_5)
compression_ngrams_5 <- ngrams_5_nrow_after/ngrams_5_nrow_before

saveRDS(ngrams_5, paste0(folder, "5-grams_DT_sb_threshold.rds"))
saveRDS(ngrams_4, paste0(folder, "4-grams_DT_sb_threshold.rds"))
saveRDS(ngrams_3, paste0(folder, "3-grams_DT_sb_threshold.rds"))
saveRDS(ngrams_2, paste0(folder, "2-grams_DT_sb_threshold.rds"))
saveRDS(ngrams_1, paste0(folder, "1-grams_DT_sb_threshold.rds"))


number_of_suggestions <- 5

ngrams_5 <- ngrams_5[,head(.SD, number_of_suggestions), ngram]
ngrams_5 <- ngrams_5[ngrams_5[, head(.I, number_of_suggestions), by=ngram]$V1]
ngrams_4 <- ngrams_4[,.SD[1:number_of_suggestions], by=ngram]
ngrams_3 <- ngrams_3[,.SD[1:number_of_suggestions], by=ngram]
ngrams_2 <- ngrams_2[,.SD[1:number_of_suggestions], by=ngram]

saveRDS(ngrams_5, paste0(folder, "5-grams_DT_sb_threshold_reduced.rds"))
saveRDS(ngrams_4, paste0(folder, "4-grams_DT_sb_threshold_reduced.rds"))
saveRDS(ngrams_3, paste0(folder, "3-grams_DT_sb_threshold_reduced.rds"))
saveRDS(ngrams_2, paste0(folder, "2-grams_DT_sb_threshold_reduced.rds"))
saveRDS(ngrams_1, paste0(folder, "1-grams_DT_sb_threshold_reduced.rds"))



## usage of dplyr and data.frame
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




