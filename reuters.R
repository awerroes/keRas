library(keras)

reuters <- dataset_reuters(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters

length(train_data)
length(test_data)
train_data[[1]]

# # Dekodowanie indeksow
# # Sprawdz z orginalem!
# word_index = dataset_reuters_word_index()
# reverse_word_index = names(word_index)
# names(reverse_word_index) = word_index
# decoded_newswire <- sapply(train_data[[1]], function(index) {
#   word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
#   if (!is.null(word)) word else "?"
#   })

# Konwersja danych

vectorize_sequence <- function(sequences, dimension = 10000){
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

x_train = vectorize_sequence(train_data)
x_test = vectorize_sequence(test_data)