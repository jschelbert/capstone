predict_stupid_backoff <- function(s, n_pred=5){
  # function to predict the next most likely words given a string s
  # assumes that string is already cleaned (nice whitespaces, no puctuation etc.)
  
  s <- gsub("[^[:alnum:][:space:]']", "", trimws(tolower(s)))
  
  s_tokens <- strsplit(s, " ")[[1]]
  prediction <- NULL
  s_length <- length(s_tokens)
  
  # we can currently only use four words to make our prediction. all 
  if(s_length>4){
    s_tokens <- s_tokens[s_length - (3:0)]
    s_length <- 4
    s <- paste(s_tokens[1:4], collapse=" ")
  }
  
  # trying 5-grams to estimate word
  prediction_5 <- ngrams_5 %>% filter(ngram==s) %>% arrange(desc(sb_score)) %>% head(n_pred) %>%  .[[2]]
  
  # if no word is found, use lower n-grams
  if(length(prediction_5)==n_pred){
    return(prediction_5)
  }
  else{
    prediction_4 <- ngrams_4 %>% filter(ngram==paste(s_tokens[2:4], collapse=" ")) %>% arrange(desc(sb_score)) %>% head(n_pred-length(prediction_5)) %>%  .[[2]]
    prediction <- union(prediction_5, prediction_4)
  }
  
  if(length(prediction)==n_pred){
    return(prediction)
  }
  else{
    prediction_3 <- ngrams_3 %>% filter(ngram==paste(s_tokens[3:4], collapse=" ")) %>% arrange(desc(sb_score)) %>% head(n_pred-length(prediction)) %>%  .[[2]]
    prediction <- union(prediction, prediction_3)
  } 
 
  if(length(prediction)==n_pred){
    return(prediction)
  }
  else{
    prediction_2 <- ngrams_2 %>% filter(ngram==s_tokens[4]) %>% arrange(desc(sb_score)) %>% head(n_pred-length(prediction)) %>%  .[[2]]
    prediction <- union(prediction, prediction_2)
  }  
  
  if(length(prediction)==n_pred){
    return(prediction)
  }
  else{
    prediction_1 <- ngrams_1 %>% arrange(desc(sb_score)) %>% head(n_pred-length(prediction)) %>%  .[[1]]
    prediction <- union(prediction, prediction_1)
  }
  
  return(prediction)
}




predict_stupid_backoff_DT <- function(s, n_pred=5){
  # function to predict the next most likely words given a string s
  # assumes that string is already cleaned (nice whitespaces, no puctuation etc.)
  
  s <- gsub("[^[:alnum:][:space:]']", "", trimws(tolower(s)))
  
  s_tokens <- strsplit(s, " ")[[1]]
  prediction <- NULL
  s_length <- length(s_tokens)
  
  # we can currently only use four words to make our prediction. all 
  if(s_length>4){
    s_tokens <- s_tokens[s_length - (3:0)]
    s_length <- 4
    s <- paste(s_tokens[1:4], collapse=" ")
  }
  
  # trying 5-grams to estimate word
  prediction_5 <- ngrams_5[s, word, nomatch=0] %>% head(n_pred)

  # if not enough words are found, use lower n-grams for remaining number of predicted words
  if(length(prediction_5)==n_pred){
    return(prediction_5)
  }
  else{
    prediction_4 <- ngrams_4[paste(s_tokens[2:4], collapse=" "), word, nomatch=0] %>% head(n_pred)
    prediction <- union(prediction_5, prediction_4)
  }
  
  if(length(prediction)>=n_pred){
    return(prediction[1:n_pred])
  }
  else{
    prediction_3 <- ngrams_3[paste(s_tokens[3:4], collapse=" "), word, nomatch=0] %>% head(n_pred)
    prediction <- union(prediction, prediction_3)
  } 
  
  if(length(prediction)>=n_pred){
    return(prediction[1:n_pred])
  }
  else{
    prediction_2 <- ngrams_2[paste(s_tokens[4], collapse=" "), word, nomatch=0] %>% head(n_pred)
    prediction <- union(prediction, prediction_2)
  }  
  
  if(length(prediction)>=n_pred){
    return(prediction[1:n_pred])
  }
  else{
    prediction_1 <- ngrams_1[1:n_pred, ngram]
    prediction <- union(prediction, prediction_1)
  }
  return(prediction[1:n_pred])
}


predict_stupid_backoff_DT_all <- function(s, n_pred=5){
  # function to predict the next most likely words given a string s
  # assumes that string is already cleaned (nice whitespaces, no puctuation etc.)
  
  s <- gsub("[^[:alnum:][:space:]']", "", trimws(tolower(s)))
  
  s_tokens <- strsplit(s, " ")[[1]]
  prediction <- NULL
  s_length <- length(s_tokens)
  
  # we can currently only use four words to make our prediction. all 
  if(s_length>4){
    s_tokens <- s_tokens[s_length - (3:0)]
    s_length <- 4
    s <- paste(s_tokens[1:4], collapse=" ")
  }
  
  # trying 5-grams to estimate word
  prediction_5 <- ngrams_5[s, .(word, sb_score), nomatch=0] %>% head(n_pred)
  prediction_4 <- ngrams_4[paste(s_tokens[2:4], collapse=" "), .(word, sb_score), nomatch=0] %>% head(n_pred)
  prediction_3 <- ngrams_3[paste(s_tokens[3:4], collapse=" "), .(word, sb_score), nomatch=0] %>% head(n_pred)
  prediction_2 <- ngrams_2[paste(s_tokens[4], collapse=" "), .(word, sb_score), nomatch=0] %>% head(n_pred)
  prediction_1 <- ngrams_1[1:n_pred, .(ngram, sb_score)]
  
  rbindlist(list(prediction_5, prediction_4, prediction_3, prediction_2, prediction_1)) %>% unique(by=word)

  return(prediction[1:n_pred])
}
