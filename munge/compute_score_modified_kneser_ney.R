library(data.table)
library(stringr)
library(purrr)

compute_mkn_ngrams <- function(folder, use_6_ngrams = FALSE){
  ngrams_1 <- readRDS(paste0(folder, "1-grams_DT_sb.rds"))
  ngrams_2 <- readRDS(paste0(folder, "2-grams_DT_sb.rds"))
  ngrams_3 <- readRDS(paste0(folder, "3-grams_DT_sb.rds"))
  ngrams_4 <- readRDS(paste0(folder, "4-grams_DT_sb.rds"))
  ngrams_5 <- readRDS(paste0(folder, "5-grams_DT_sb.rds"))
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
    all_ngrams <- list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5, ngrams_6)
    mkn_n <- sapply(list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5, ngrams_6), function(x){sapply(1:4, function(i){x[n==i,.N]})})
    max_n <- 6
  } else {
    all_ngrams <- list(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5)
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
  
  
  
  temp <- merge(ngrams_1, ngrams_2, all.x = TRUE, by.x = "ngram", by.y = "word")[, .N, by="ngram"]
  all_ngrams[[1]][temp, mkn := N/nrow(ngrams_2) + 1/nrow(ngrams_1)] # Do we need to add 1/|vocabulary| ?
  rm(temp)
  
  #TODO: Wie mache ich das für einen Join mit sich selbst? Brauche ich den überhaupt?
  #temp <- ngrams_2[, count_word:=sum(n), by="word"]
  #setnames(temp, old="N", new="count_word")
  #setkey(temp, "word")
  #test <- ngrams_2[temp, , on="word"]
  rm(ngrams_1, ngrams_2, ngrams_3, ngrams_4, ngrams_5)
  
  all_ngrams[[2]][, count_word:=sum(n), by="ngram"]
  all_ngrams[[2]] <- merge(all_ngrams[[2]], mkn_N1, all.x=TRUE, by="ngram")
  all_ngrams[[2]] <- merge(all_ngrams[[2]], mkn_N2, all.x=TRUE, by="ngram")
  all_ngrams[[2]] <- merge(all_ngrams[[2]], mkn_N3, all.x=TRUE, by="ngram")
  all_ngrams[[2]][is.na(count_N1), count_N1 := 0]
  all_ngrams[[2]][is.na(count_N2), count_N2 := 0]
  all_ngrams[[2]][is.na(count_N3), count_N3 := 0]
  #???? brauche eigentlich nur mkn von ngrams_1
  all_ngrams[[2]] <- merge(all_ngrams[[2]], all_ngrams[[1]], all.x=TRUE, by.x = "word", by.y = "ngram")
  
  all_ngrams[[2]][, mkn := (pmax(n.x - mkn_D[2, 1 + pmin(3, n.x)], 0)) / count_word + ((mkn_D[2, 2] * count_N1 + mkn_D[2, 3] * count_N2 + mkn_D[2, 4] * count_N3) / count_word) * mkn.y]
  all_ngrams[[2]][,`:=`(mkn.x = NULL, count_word = NULL, count_N1 = NULL, count_N2 = NULL, count_N3 = NULL, n.y = NULL, sb_score.y = NULL, mkn.y = NULL)]
  setnames(all_ngrams[[2]], old="n.x", new="n")
  setnames(all_ngrams[[2]], old="sb_score.x", new="sb_score")
  
  saveRDS(all_ngrams[[1]], paste0(folder, "1-grams_DT_sb_mkn.rds"))
  all_ngrams[[1]] <- integer(0)
  gc()
  
  saveRDS(all_ngrams[[2]], paste0(folder, "2-grams_DT_sb_mkn.rds"))
  
  for (ci in 3:max_n){
    all_ngrams[[ci]][, count_word:=sum(n), by="ngram"]
    all_ngrams[[ci]] <- merge(all_ngrams[[ci]], mkn_N1, all.x=TRUE, by="ngram")
    all_ngrams[[ci]] <- merge(all_ngrams[[ci]], mkn_N2, all.x=TRUE, by="ngram")
    all_ngrams[[ci]] <- merge(all_ngrams[[ci]], mkn_N3, all.x=TRUE, by="ngram")
    all_ngrams[[ci]][is.na(count_N1), count_N1 := 0]
    all_ngrams[[ci]][is.na(count_N2), count_N2 := 0]
    all_ngrams[[ci]][is.na(count_N3), count_N3 := 0]
    #???? brauche eigentlich nur mkn von ngrams_1
    all_ngrams[[ci]][, ngram_last := map_chr(str_split(ngram, pattern=" "), function(x) paste(x[length(x) - ((ci-3):0)], collapse=" "))]
    all_ngrams[[ci]] <- merge(all_ngrams[[ci]], all_ngrams[[ci - 1]], all.x=TRUE, by.x=c("ngram_last", "word"), by.y = c("ngram", "word"))
    all_ngrams[[ci-1]] <- integer(0)
    gc()
    all_ngrams[[ci]][, mkn := (pmax(n.x - mkn_D[ci, 1 + pmin(3, n.x)], 0)) / count_word + 
                       ((mkn_D[ci, 2] * count_N1 + 
                           mkn_D[ci, 3] * count_N2 + 
                           mkn_D[ci, 4] * count_N3) / count_word) * mkn.y]
    all_ngrams[[ci]][,`:=`(mkn.x = NULL, count_word = NULL, count_N1 = NULL, count_N2 = NULL, 
                           count_N3 = NULL, n.y = NULL, sb_score.y = NULL, 
                           mkn.y = NULL, word.y = NULL, ngram_last = NULL)]
    setnames(all_ngrams[[ci]], old="n.x", new="n")
    setnames(all_ngrams[[ci]], old="sb_score.x", new="sb_score")
    try({setnames(all_ngrams[[ci]], old="word.x", new="word")})
    saveRDS(all_ngrams[[ci]], paste0(folder, ci, "-grams_DT_sb_mkn.rds"))
  }
  rm(all_ngrams)
  gc()
}
