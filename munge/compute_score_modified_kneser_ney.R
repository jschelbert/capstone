library(data.table)

folder <- "data/samples/en_US/"
use_6_ngrams <- FALSE

ngrams_1 <<- readRDS(paste0(folder, "1-grams_DT_sb.rds"))
ngrams_2 <<- readRDS(paste0(folder, "2-grams_DT_sb.rds"))
ngrams_3 <<- readRDS(paste0(folder, "3-grams_DT_sb.rds"))
ngrams_4 <<- readRDS(paste0(folder, "4-grams_DT_sb.rds"))
ngrams_5 <<- readRDS(paste0(folder, "5-grams_DT_sb.rds"))
setDT(ngrams_1)
setDT(ngrams_2)
setDT(ngrams_3)
setDT(ngrams_4)
setDT(ngrams_5)

if(use_6_ngrams){
  ngrams_6 <- readRDS(paste0(folder, "6-grams_DT_sb.rds"))
  setDT(ngrams_6)
}

### Precompute some data:
# We store the number of n-grams with count 1-4 in the following matrix for n=1-5 (or 6)

if(use_6_ngrams){
  #all_ngrams <- list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5, ngrams_6)
  mkn_n <- sapply(list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5, ngrams_6), function(x){sapply(1:4, function(i){x[n==i,.N]})})
  max_n <- 6
} else {
  #all_ngrams <- list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5)
  mkn_n <- sapply(list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5), function(x){sapply(1:4, function(i){x[n==i,.N]})})
  max_n <- 5
}

mkn_Y <- mkn_n[1,] / (mkn_n[1,] + 2 * mkn_n[2,])

mkn_D <- matrix(0, nrow=max_n, ncol=4)
mkn_D[,2] <- 1 - 2 * mkn_Y * mkn_n[2,] / mkn_n[1,]
mkn_D[,3] <- 2 - 3 * mkn_Y * mkn_n[3,] / mkn_n[2,]
mkn_D[,4] <- 3 - 4 * mkn_Y * mkn_n[4,] / mkn_n[3,]
mkn_D[is.na(mkn_D)] <- 0

# calculate the number of unique words with exactly k (for k=1-2) or more than 3 counts that follow the history w_{i-n+1}^{i-1}
mkn_N1 <- rbindlist(list(ngrams_2[n==1, .N, by=ngram], 
                     ngrams_3[n==1, .N, by=ngram], 
                     ngrams_4[n==1, .N, by=ngram], 
                     ngrams_5[n==1, .N, by=ngram]))
mkn_N2 <- rbindlist(list(ngrams_2[n==2, .N, by=ngram], 
                     ngrams_3[n==2, .N, by=ngram], 
                     ngrams_4[n==2, .N, by=ngram], 
                     ngrams_5[n==2, .N, by=ngram]))
mkn_N3 <- rbindlist(list(ngrams_2[n>=3, .N, by=ngram], 
                     ngrams_3[n>=3, .N, by=ngram], 
                     ngrams_4[n>=3, .N, by=ngram], 
                     ngrams_5[n>=3, .N, by=ngram]))
setnames(mkn_N1, old="N", new="count_N1")
setnames(mkn_N2, old="N", new="count_N2")
setnames(mkn_N3, old="N", new="count_N3")
setkey(mkn_N1, "ngram")
setkey(mkn_N2, "ngram")
setkey(mkn_N3, "ngram")

ngrams_1[,mkn:=0]
ngrams_2[,mkn:=0]
ngrams_3[,mkn:=0]
ngrams_4[,mkn:=0]
ngrams_5[,mkn:=0]

# unigrams need special treatment...
# p0 <- 1 / (obj$frequencies[[1]]$unique_entries - 1); ==> 1/nrow(ngrams_1)
# p1 <- 1 / (obj$frequencies[[2]]$unique_entries - 1); ==> 1/nrow(ngrams_2)

temp <- merge(ngrams_1, ngrams_2, all.x = TRUE, by.x = "ngram", by.y = "word")[, .N, by="ngram"]
ngrams_1[temp, mkn := N/nrow(ngrams_2) + 1/nrow(ngrams_1)] # Do we need to add 1/|vocabulary| ?


#TODO: Wie mache ich das für einen Join mit sich selbst? Brauche ich den überhaupt?
#temp <- ngrams_2[, count_word:=sum(n), by="word"]
#setnames(temp, old="N", new="count_word")
#setkey(temp, "word")
#test <- ngrams_2[temp, , on="word"]
ngrams_2[, count_word:=sum(n), by="ngram"]
ngrams_2 <- merge(ngrams_2, mkn_N1, all.x=TRUE, by="ngram")
ngrams_2 <- merge(ngrams_2, mkn_N2, all.x=TRUE, by="ngram")
ngrams_2 <- merge(ngrams_2, mkn_N3, all.x=TRUE, by="ngram")
ngrams_2[is.na(count_N1), count_N1 := 0]
ngrams_2[is.na(count_N2), count_N2 := 0]
ngrams_2[is.na(count_N3), count_N3 := 0]
#???? brauche eigentlich nur mkn von ngrams_1
test <- merge(ngrams_2, ngrams_1, all.x=TRUE, by="ngram")

test[, blah1 := (pmax(n.x - mkn_D[2, 1 + pmin(3, n.x)], 0)) / count_word + ((mkn_D[2, 2] * count_N1 + mkn_D[2, 3] * count_N2 + mkn_D[2, 4] * count_N3) / count_word) * mkn.y]
test[, blah2 := (pmax(n.x - mkn_D[2, 1 + pmin(3, n.x)], 0)) / count_word]
test[, blah3 := (pmax(n.x - mkn_D[2, 1 + pmin(3, n.x)], 0))]
test[, blah4 := mkn_D[2, 1 + pmin(3, n.x)]]
test[, blah5 := 1 + pmin(3, n.x)]
test[, blah6 := ((mkn_D[2, 2] * count_N1 + mkn_D[2, 3] * count_N2 + mkn_D[2, 4] * count_N3) / count_word) * mkn.y]

# get count of 2-grams that end with "word":
temp <- ngrams_2[, .N, by="word"]
setkey(temp, "word")
for (i in 1:nrow(ngrams_2)){
  ngram <- ngrams_2[[i, 1]]
  word <- ngrams_2[[i, 2]]
  count <- ngrams_2[[i, 3]]
  ngramcount_word <- temp[word, N]
  N1 <- mkn_N1[ngram, count_N1]
  N2 <- mkn_N2[ngram, count_N2]
  N3 <- mkn_N3[ngram, count_N3]
  N1 <- ifelse(is.na(N1), 0, N1)
  N2 <- ifelse(is.na(N2), 0, N2)
  N3 <- ifelse(is.na(N3), 0, N3)
  gamma <- (mkn_D[2, 2] * N1 + mkn_D[2, 3] * N2 + mkn_D[2, 4] * N3) / ngramcount_word
  mkn_value <- max(count - mkn_D[2, 1 + min(3, count)], 0) / ngramcount_word + gamma * ngrams_1[word, mkn]
  set(ngrams_2, i, 5, mkn_value)
}


#ngrams_2[, mkn := (pmax(n - D[2, 1 + pmin(3, n)], 0)) / temp[ngram, N] + ((mkn_D[2, 2] * mkn_N1[ngram, N] + mkn_D[2, 3] * mkn_N2[ngram, N] + mkn_D[2, 4] * mkn_N3[ngram, N])/temp[ngram, N]) * ngrams_1[ngram, mkn]]


# 
# 
# # Function for usage of data.table package (not using dplyr)
# compute_score_modified_kneser_ney_DT <- function(s){
#   # This function assumes that the string s is in the ngrams data set
#   s_tokens <- strsplit(s, " ")[[1]]
#   score <- 0
#   s_length <- length(s_tokens)
#   
#   
#   
# }