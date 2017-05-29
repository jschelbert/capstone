
##### Parallelization
# see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/#The_parallel_package
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
# Once we are done we need to close the cluster so that resources such as memory are returned to the operating system.
stopCluster(cl)



##### Reading data via tm package:
library(tm)
en_us <- VCorpus(DirSource("data/final/en_US/", encoding = "UTF-8"))



# removal of stopwords
en_us <- tm_map(en_us, removeWords, stopwords("english"))

# stemming
en_us <- tm_map(en_us, stemDocument(language="english"))






##### Quiz - Week 1
library(dplyr)
library(stringr)
# 1) The en_US.blogs.txt file is how many megabytes? --> 210 MB

# 2) The en_US.twitter.txt has how many lines of text? --> over 2 Million
length(content(en_us[[3]]))

# 3) What is the length of the longest line seen in any of the three en_US data sets? --> over 40K in blogs
max_blog <- en_us[[1]] %>% content() %>% nchar %>% max # or with stringr str_length instead of nchar
max_news <- en_us[[2]] %>% content() %>% nchar %>% max
max_twitter <- en_us[[3]] %>% content() %>% nchar %>% max

# 4) In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
n_love <- en_us[[3]] %>% content() %>% str_detect("love") %>% sum
n_hate <- en_us[[3]] %>% content() %>% str_detect("hate") %>% sum
n_love/n_hate

# 5) The one tweet in the en_US twitter data set that matches the word "biostats" says what? --> Has to study for the upcoming exam
en_us[[3]] %>% content() %>% str_subset("biostats")

# 6) How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.) --> 3
en_us[[3]] %>% content() %>%  str_subset("A computer once beat me at chess, but it was no match for me at kickboxing")







##### Week 2
# using ngram package as tm seems a bit too much overhead
alldata <- multiread("data/final/en_US/", extension="txt") #<- seems not to work!
#Warnmeldungen:
#  1: In FUN(X[[i]], ...) : Zeile 167155 scheint ein nul Zeichen zu enthalten
#  2: In FUN(X[[i]], ...) : Zeile 268547 scheint ein nul Zeichen zu enthalten
#  3: In FUN(X[[i]], ...) : Zeile 1274086 scheint ein nul Zeichen zu enthalten
#  4: In FUN(X[[i]], ...) : Zeile 1759032 scheint ein nul Zeichen zu enthalten

blogs <- readLines("data/final/en_US/en_US.blogs.txt")
news <- readLines("data/final/en_US/en_US.news.txt")
twitter <- readLines("data/final/en_US/en_US.twitter.txt") ##<- twitter seems to have some characters that cause an error in readLines!

data("crude")
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
inspect(tdm[340:345,1:10])

ng1 <- ngram(concatenate(blogs), n=1)
pt1 <- get.phrasetable(ng1)

ng2 <- ngram(concatenate(blogs), n=2)
pt2 <- get.phrasetable(ng2)

ng3 <- ngram(concatenate(blogs), n=3)
pt3 <- get.phrasetable(ng3)

ng4 <- ngram(concatenate(blogs), n=4)
pt4 <- get.phrasetable(ng4)

g <- ggplot(data=head(pt4, 20), aes(x=reorder(ngrams, freq), y=freq))
g + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Run this every time the work space is freed of unneccessary objects. It releases unused memory.
gc()


g <- ggplot(data=head(pt1, 20), aes(x=reorder(ngrams, -freq), y=freq))
g + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + labs(x="n-grams", y="frequency", title="Frequency of top-20 2-grams")

string.summary(concatenate(blogs))







## quanteda
library(quanteda)
library(readtext)
library(tidyr)
library(dplyr)
library(data.table)

folder <- "data/final/en_US/"
folder <- "data/samples/en_US/"
folder <- "data/samples_small/en_US/"

textdata <- readtext::readtext(paste0(folder, "*.txt")) %>% quanteda::corpus()

#        User      System verstrichen 
#      64.110       4.188      68.324 

tok_sentences <- textdata %>% tokenize(what="sentence", remove_twitter = TRUE, verbose=TRUE, simplify=TRUE)
#        User      System verstrichen 
#      73.819      19.556      93.375 
mycorpus <- tok_sentences %>% char_tolower() %>% quanteda::corpus()
#        User      System verstrichen 
#      27.087       1.550      28.633

rm(tok_sentences, textdata)
gc()

elapsedtime <- system.time(grams <- tokens(mycorpus, what="word", remove_numbers=TRUE, remove_punct = TRUE,
                                     remove_symbols = TRUE, remove_separators = TRUE,
                                     remove_twitter = TRUE, remove_hyphens = FALSE, remove_url = TRUE,
                                     ngrams = 1L, verbose=TRUE))
#        User      System verstrichen 
#     119.572       4.470     124.127 
saveRDS(grams, paste0(folder, "1-grams.rds"))
#gram_1 <- readRDS(paste0(folder, "1-grams.rds"))
rm(gram)
gc()


mycorpus <- tokens(mycorpus, what="word", remove_numbers=TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_twitter = TRUE, remove_hyphens = FALSE, remove_url = TRUE,
                     ngrams = 1L, verbose=TRUE)

for(i in 2:5){
  elapsedtime <- system.time(grams <- tokens_ngrams(mycorpus, n = i))
  saveRDS(grams, paste0(folder, i, "-grams.rds"))
  print(paste0(i,"-grams generated with:"))
  print(elapsedtime)
  rm(grams)
  gc()
}





## tidytext
library(tidyr)
library(dplyr)
library(tidytext)
library(quanteda)
folder <- "data/samples_small/en_US/"
textdata <- readtext::readtext(paste0(folder, "*.txt")) %>% quanteda::corpus()
tok_sentences <- textdata %>% tokenize(what="sentence", remove_twitter = TRUE, verbose=TRUE, simplify=TRUE)
df_tok_sentences <- data_frame(text = tok_sentences)

tok <-  df_tok_sentences %>% unnest_tokens(word, text)
tok %>% count(word, sort = TRUE) 


tok <- df_tok_sentences %>% unnest_tokens(bigram, text, token="ngrams", n=2)
bigrams_separated <- tok %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated %>% count(word1, word2, sort = TRUE)



# generate frequency tables:
library(tidyr)
library(dplyr)
library(tidytext)
library(quanteda)
folder <- "data/samples/en_US/"
textdata <- readtext::readtext(paste0(folder, "*.txt")) %>% quanteda::corpus()
tok_sentences <- textdata %>% tokenize(what="sentence", remove_twitter = TRUE, verbose=TRUE, simplify=TRUE)
df_tok_sentences <- data_frame(text = tok_sentences)
rm(textdata, tok_sentences)
gc()
tok1 <-  df_tok_sentences %>% unnest_tokens(ngram, text) %>% count(word, sort = TRUE) 
saveRDS(tok1, paste0(folder, "1-grams_tidy.rds"))
for(i in 2:5){
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


ngrams_5 <- ngrams_5 %>% separate(ngram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, word4, sep = " ") %>% as.data.table()
ngrams_4 <- ngrams_4 %>% separate(ngram, c("word1", "word2", "word3", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, sep = " ")
ngrams_3 <- ngrams_3 %>% separate(ngram, c("word1", "word2", "word"), sep = " ") %>% unite(ngram, word1, word2, sep = " ")
ngrams_2 <- ngrams_2 %>% separate(ngram, c("word1", "word"), sep = " ")



score_stupid_backoff <- function(s){
  # Funciton assumes a clean string with only " " as a separator without additional whitespaces
  
  # split string s into different tokens
  s_tokens <- strsplit(s, " ")

  s_is_5gram <- FALSE
  s_is_4gram <- FALSE
  s_is_3gram <- FALSE
  s_is_2gram <- FALSE
  s_is_1gram <- FALSE
  
  switch(length(s_tokens),
    5 = s_is_5gram <- TRUE,
    4 = s_is_4gram <- TRUE,
    3 = s_is_3gram <- TRUE,
    2 = s_is_2gram <- TRUE,
    1 = s_is_1gram <- TRUE,
    stop("something went wrong with ", s)
  )
  
  count_ngram_5 <- ngrams_5 %>% filter(ngram==s) %>% .[[2]]
  if(length(count_ngram_5)==0){
    
  }
  

  if (s_is_5gram){
    s_score <- ngrams_5
  }
  
}

## Benchmark for finding words in a data frame
library(microbenchmark)
bigram_df <- bigrams_separated %>% select(-doc_id) %>% head(100000)
rm(austen_bigrams, tok, bigrams_separated, textdata)
gc()
count_df1 <- bigram_df %>% count(word1, word2, sort=TRUE)
cols <- c("word1", "word2")
count_df2 <- bigram_df %>% count(word1, word2, sort=TRUE)
count_df2[cols] <- lapply(count_df2[cols], factor)

testwords1 <- c("system", "capitalism")
testwords2 <- c("zoom", "lens")
testwords3 <- c("the", "size")
testwords4 <- c("do", "i")
testwords5 <- c("from", "the")
testwords6 <- c("zoom", "blashfdj")
filter(count_df1, word1==testwords1[1], word2==testwords1[2])


microbenchmark(filter(count_df1, word1==testwords1[1], word2==testwords1[2]),
               filter(count_df2, word1==testwords1[1], word2==testwords1[2]),
               times=10L)

microbenchmark(filter(count_df1, word1==testwords2[1], word2==testwords2[2]),
               filter(count_df2, word1==testwords2[1], word2==testwords2[2]),
               times=10L)

microbenchmark(filter(count_df1, word1==testwords3[1], word2==testwords3[2]),
               filter(count_df2, word1==testwords3[1], word2==testwords3[2]),
               times=10L)

microbenchmark(filter(count_df1, word1==testwords4[1], word2==testwords4[2]),
               filter(count_df2, word1==testwords4[1], word2==testwords4[2]),
               times=10L)

microbenchmark(filter(count_df1, word1==testwords5[1], word2==testwords5[2]),
               filter(count_df2, word1==testwords5[1], word2==testwords5[2]),
               times=10L)

microbenchmark(filter(count_df1, word1==testwords6[1], word2==testwords6[2]),
               filter(count_df2, word1==testwords6[1], word2==testwords6[2]),
               times=10L)

## => factor columns are about a factor 10 slower (no pun intended :D). We should use character as class for columns containing words


## for 4-grams what is better? data frame with colums "word123 word4" or data frame with columns "word1 word2 word3 word4"?




blah <- austen_books() %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 4) %>%
    separate(trigram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    count(word1, word2, word3, word4, sort = TRUE)








