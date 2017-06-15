compute_score_stupid_backoff <- function(s, lambda=.4){
  # This function assumes that the string s is in the ngrams data set
  s_tokens <- strsplit(s, " ")[[1]]
  score <- 0
  s_length <- length(s_tokens)
 
  if(s_length==6){
    count_ngram_6 <- ngrams_6 %>% filter(ngram==s) %>% .[[2]]
    count_ngram_5 <- ngrams_5 %>% filter(ngram==paste(s_tokens[1:5], collapse=" ")) %>% .[[2]]
    score <- 1/lambda * count_ngram_6/count_ngram_5
    return(score)
  } 
  else if(s_length==5){
    count_ngram_5 <- ngrams_5 %>% filter(ngram==s) %>% .[[2]]
    count_ngram_4 <- ngrams_4 %>% filter(ngram==paste(s_tokens[1:4], collapse=" ")) %>% .[[2]]
    score <- count_ngram_5/count_ngram_4
    return(score)
  }
  else if(s_length==4){
    count_ngram_4 <- ngrams_4 %>% filter(ngram==s) %>% .[[2]]
    count_ngram_3 <- ngrams_3 %>% filter(ngram==paste(s_tokens[1:3], collapse=" ")) %>% .[[2]]
    score <- lambda * count_ngram_4/count_ngram_3
    return(score)
  }
  else if(s_length==3){
    count_ngram_3 <- ngrams_3 %>% filter(ngram==s) %>% .[[2]]
    count_ngram_2 <- ngrams_2 %>% filter(ngram==paste(s_tokens[1:2], collapse=" ")) %>% .[[2]]
    score <- lambda * lambda * count_ngram_3/count_ngram_2
    return(score)
  }
  else if(s_length==2){
    count_ngram_2 <- ngrams_2 %>% filter(ngram==s) %>% .[[2]]
    count_ngram_1 <- ngrams_1 %>% filter(ngram==s_tokens[1]) %>% .[[2]]
    score <- lambda * lambda * lambda * count_ngram_2/count_ngram_1
    return(score)
  }
  else if(s_length==1){
    count_ngram_1 <- ngrams_1 %>% filter(ngram==s) %>% .[[2]]
    score <- lambda * lambda * lambda * lambda * count_ngram_1/nrow(ngrams_1)
    return(score)
  }
  return(score) # return 0 if length 0
}


# Function for usage of data.table package (not using dplyr)
compute_score_stupid_backoff_DT <- function(s, lambda=.4){
  # This function assumes that the string s is in the ngrams data set
  s_tokens <- strsplit(s, " ")[[1]]
  score <- 0
  s_length <- length(s_tokens)
  
  if(s_length==6){
    count_ngram_6 <- ngrams_6[ngram==s, n]
    count_ngram_5 <- ngrams_5[ngram==paste(s_tokens[1:5], collapse=" "), n]
    score <- count_ngram_6/count_ngram_5
    return(score)
  }  
  if(s_length==5){
    count_ngram_5 <- ngrams_5[ngram==s, n]
    count_ngram_4 <- ngrams_4[ngram==paste(s_tokens[1:4], collapse=" "), n]
    score <- count_ngram_5/count_ngram_4
    return(score)
  }
  else if(s_length==4){
    count_ngram_4 <- ngrams_4[ngram==s, n]
    count_ngram_3 <- ngrams_3[ngram==paste(s_tokens[1:3], collapse=" "), n]
    score <- lambda * count_ngram_4/count_ngram_3
    return(score)
  }
  else if(s_length==3){
    count_ngram_3 <- ngrams_3[ngram==s, n]
    count_ngram_2 <- ngrams_2[ngram==paste(s_tokens[1:2], collapse=" "), n]
    score <- lambda * lambda * count_ngram_3/count_ngram_2
    return(score)
  }
  else if(s_length==2){
    count_ngram_2 <- ngrams_2[ngram==s, n]
    count_ngram_1 <- ngrams_1[ngram==s_tokens[1], n]
    score <- lambda * lambda * lambda * count_ngram_2/count_ngram_1
    return(score)
  }
  else if(s_length==1){
    count_ngram_1 <- ngrams_1[ngram==s, n]
    score <- lambda * lambda * lambda * lambda * count_ngram_1/nrow(ngrams_1)
    return(score)
  }
  return(score) # return 0 if length 0
}