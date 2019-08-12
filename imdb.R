# Chapter 3
# Zbiór danych IMDB

library(keras)

imdb = dataset_imdb(num_words = 10000)
# operator wielokrotnego przypisania %<-%
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb
str(train_data[[1]])
train_labels[[1]]
max(sapply(train_data, max))

# # DEKODOWANIE
# # slownik przypisujacy slowom wartosci indeksow
# word_index = dataset_imdb_word_index()
# # odwrócenie slownika przypisuje indeksy do slow
# reverse_word_index = names(word_index)
# names(reverse_word_index) = word_index
# # Kod dekodujacy recenzje. Zauwaz, ze indeksy są przesuniete o 3, poniewaz pod trzema
# # pierwszymi indeksami znajduja sie indeksy symbolizujace „wypełnienie”, „poczatek
# # sekwencji” i „nieznane slowo”.
# 
# decode_review = sapply(train_data[[1]], 
#                        function(index){
#                          word = if (index >= 3) reverse_word_index[[as.character(index - 3)]]
#                          if (!is.null(word)) word else "?"
# })

vectorize_sequence = function(sequences, dimension = 10000){
  results = matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] = 1
  results
}
x_train = vectorize_sequence(train_data)
x_test = vectorize_sequence(test_data)
y_train = vectorize_sequence(train_labels)
y_test = vectorize_sequence(test_labels)

model = keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# # kompilowanie modelu
# 
# model %>% compile(
#   optimizer = "rmsprop",
#   loss = "binary_crossentropy",
#   metrics = c("accuracy")
# )
# 
# # konfiguracja optymalizatora
# 
# model %>% compile(
#   optimizer = optimizer_rmsprop(lr = 0.001),
#   loss = "binary_crossentropy",
#   metrics = c("accuracy")
# )
# 
# # korzystanie z wlasnych funkcji straty i metryki
# 
# model %>% compile(
#   optimizer = optimizer_rmsprop(lr = 0.001),
#   loss = "binary_crossentropy",
#   metrics = metrics_binary_accuracy
# )

# tworzenie zbioru walidacyjnego

val_indices = 1:10000

x_val = x_train[val_indices,]
partial_x_train = x_train[-val_indices,]
y_val = y_train[val_indices]
partial_y_train = y_train[-val_indices]

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history = model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 10,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

str(history)
plot(history)
# history jako obiekt data.frame
history_df = as.data.frame(history)
str(history_df)

# Listing 3.9
# Ponowne uczenie modelu od poczatku
model = keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results = model %>% evaluate(x_test, y_test)
