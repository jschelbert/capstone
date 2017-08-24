#folder <- "data/samples_supersmall/en_US/"
#folder <- "data/samples_small/en_US/"
#folder <- "data/samples/en_US/"
#folder <- "data/final/en_US/"
#folder <- "data/final/de_DE/"
#folder <- "data/news-2014/en_US/"

for (folder in c("data/final/en_US/", "data/final/de_DE/")) {
  setwd(
    "/Volumes/Daten/Dokumente/workspace/Coursera/Data-Science-Specialization/capstone"
  )
  source("munge/generate_frequency_tables.R")
  source("src/predict_stupid_backoff.R")
  compute_tidy_ngrams(folder)
  compute_sb_ngrams(folder)
  
  benchmark_results <- data.frame()
  df_threshold <- data.frame()
  df_suggestions <- data.frame()
  
  for (threshold in 2:5) {
    setwd("/Volumes/Daten/Dokumente/workspace/Coursera/Data-Science-Specialization/capstone")
    df_threshold_i <- compute_reduced_ngrams_threshold(
      folder,
      number_of_ngrams_1 = 50,
      threshold_ngrams_2 = threshold,
      threshold_ngrams_3 = threshold,
      threshold_ngrams_4 = threshold,
      threshold_ngrams_5 = threshold,
      threshold_ngrams_6 = threshold
    )
    df_suggestions_i <- compute_reduced_ngrams_suggestions(folder)
    df_threshold <- rbind(df_threshold, df_threshold_i)
    df_suggestions <-
      rbind(df_suggestions, cbind(threshold, df_suggestions_i))
    
    ngrams_5 <<-
      readRDS(paste0(folder, "5-grams_DT_sb_threshold_reduced.rds"))
    ngrams_4 <<-
      readRDS(paste0(folder, "4-grams_DT_sb_threshold_reduced.rds"))
    ngrams_3 <<-
      readRDS(paste0(folder, "3-grams_DT_sb_threshold_reduced.rds"))
    ngrams_2 <<-
      readRDS(paste0(folder, "2-grams_DT_sb_threshold_reduced.rds"))
    ngrams_1 <<-
      readRDS(paste0(folder, "1-grams_DT_sb_threshold_reduced.rds"))
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
    setorder(ngrams_5, ngram,-sb_score)
    setorder(ngrams_4, ngram,-sb_score)
    setorder(ngrams_3, ngram,-sb_score)
    setorder(ngrams_2, ngram,-sb_score)
    setorder(ngrams_1,-sb_score)
    gc()
    
    setwd(
      "/Volumes/Daten/Dokumente/workspace/Coursera/Data-Science-Specialization/dsci-benchmark"
    )
    sprintf("Threshold set to %i", threshold)
    source("benchmark.R")
    benchmark_results_i <- benchmark(
      predict_stupid_backoff_DT,
      3,
      # additional parameters to be passed to the prediction function can be inserted here
      sent.list = list('tweets' = tweets,
                       'blogs' = blogs),
      ext.output = F
    )
    benchmark_results <-
      rbind(benchmark_results, cbind(threshold, benchmark_results_i))
  }
  setwd("/Volumes/Daten/Dokumente/workspace/Coursera/Data-Science-Specialization/capstone")
  saveRDS(benchmark_results, paste0(folder, "benchmark_results.rds"))
  saveRDS(df_suggestions, paste0(folder, "suggestions_results.rds"))
  saveRDS(df_threshold, paste0(folder, "thresholds_results.rds"))
}
