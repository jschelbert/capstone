library(keras)
library(data.table)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)


# Parameters --------------------------------------------------------------

maxlen <- 100L
bucketsize <- 100000L

# Data preparation --------------------------------------------------------

path <- get_file(
  'nietzsche.txt', 
  origin='https://s3.amazonaws.com/text-datasets/nietzsche.txt'
)

text <- read_lines(path) %>%
  str_to_lower() %>%
  str_c(collapse = "\n") %>%
  tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)

print(sprintf("corpus length: %d", length(text)))

chars <- text %>%
  unique() %>%
  sort()

print(sprintf("total chars: %d", length(chars)))  

# cut the text in semi-redundant sequences of maxlen characters
dataset <- map(
  seq(1, length(text) - maxlen - 1, by = 3), 
  ~list(sentece = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
)

dataset <- transpose(dataset)
setDT(dataset)



# Model definition --------------------------------------------------------

model <- keras_model_sequential()

model %>%
  layer_lstm(256, input_shape = c(maxlen, length(chars))) %>%
  layer_dense(length(chars)) %>%
  layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = optimizer,
  metrics = c(categorical_crossentropy = "categorical_crossentropy",
              categorical_accuracy = "categorical_accuracy",
              top_k_categorical_accuracy = "top_k_categorical_accuracy")
)


# Training and results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

for(iteration in 1:60){
  
  cat(sprintf("iteration: %02d ---------------\n\n", iteration))
  
  dataset_sample <- dataset[sample(.N, bucketsize)]
  
  # vectorization
  X <- array(0, dim = c(length(dataset_sample$sentece), maxlen, length(chars)))
  y <- array(0, dim = c(length(dataset_sample$sentece), length(chars)))
  
  for(i in 1:length(dataset_sample$sentece)){
    
    X[i,,] <- sapply(chars, function(x){
      as.integer(x == dataset_sample$sentece[[i]])
    })
    
    y[i,] <- as.integer(chars == dataset_sample$next_char[[i]])
    
  }
  
  cat(sprintf("It %02d: Sampling and preprocessing done ---------------\n\n", iteration))
  
  
  # besser: fit_generator(generatorfunction)
  model %>% fit(
    X, y,
    batch_size = 256,
    epochs = 5
  )
  
  for(diversity in c(0.2, 0.5, 1)){
    
    cat(sprintf("diversity: %f ---------------\n\n", diversity))
    
    start_index <- sample(1:(length(text) - maxlen), size = 1)
    sentence <- text[start_index:(start_index + maxlen - 1)]
    generated <- ""
    
    for(i in 1:400){
      
      x <- sapply(chars, function(x){
        as.integer(x == sentence)
      })
      dim(x) <- c(1, dim(x))
      
      preds <- predict(model, x)
      next_index <- sample_mod(preds, diversity)
      next_char <- chars[next_index]
      
      generated <- str_c(generated, next_char, collapse = "")
      sentence <- c(sentence[-1], next_char)
      
    }
    
    cat(generated)
    cat("\n\n")
    
  }
}
