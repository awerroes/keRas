# Chapter 2, listing 2.1 - 2.5 
library(keras)

# Load datasets, create test and train sets

mnist <- dataset_mnist()
train_images = mnist$train$x
train_labels = mnist$train$y
test_images = mnist$test$x
test_labels = mnist$test$y

# network architecture
network = keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
  layer_dense(units = 10, activation = "softmax")

# compilation
network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# image preparation
train_images = array_reshape(train_images, c(60000, 28 * 28))
train_images = train_images / 255

test_images = array_reshape(test_images, c(10000, 28 * 28))
test_images = test_images / 255

# labels preparation
train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)

# fit !
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# metrics
metrics = network %>% evaluate(test_images, test_labels, verbose = 0)
metrics

# test samples
network %>% predict_classes(test_images[1:10,])
