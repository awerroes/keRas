# Chapter 2, listing

# Data types
# vector
x <- c(12, 3, 6, 14, 10)
str(x)
# matrix
x <- matrix(rep(0, 3*5), nrow = 3, ncol = 5)
x
dim(x)
# tensor
x = array(rep(0, 2*3*2), dim = c(2,3,2))
str(x)
dim(x)


library(keras)

# Load datasets, create test and train sets

mnist <- dataset_mnist()
train_images = mnist$train$x
train_labels = mnist$train$y
test_images = mnist$test$x
test_labels = mnist$test$y

length(dim(train_images))
dim(train_images)
typeof(train_images)

digit = train_images[5, ,]
plot(as.raster(digit, max = 255))

# 2.2.6

my_slice = train_images[10:99,,]
dim(my_slice)

my_slice = train_images[10:99, 1:28, 1:28]
dim(my_slice)
my_slice = train_images[, 15:28, 15:28]

# 2.2.7
batch = train_images[1:128,,]
batch = train_images[129:256,,]
