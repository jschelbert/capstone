
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

