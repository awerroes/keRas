# Chapter 2

library(keras)

layer_dense(units = 512, activation = "relu")
output = relu(dot(W, input) + b)

# z = pmax(z, 0)
naive_relu = function(x){
  for (i in nrow(x))
    for (j in ncol(x))
      x[i, j] = max(x[i, j], 0)
  x
}

# z = x + y
naive_add = function(x, y){
  for (i in nrow(x))
    for (j in ncol(x))
      x[i, j] = x[i, j] + y[i, j]
  x
}

# Operacje na tensorach o roznych wymiarach

# x jest tensorem wartości losowych o ksztalcie (64, 3, 32, 10)
x = array(round(runif(1000, 0, 9)), dim = c(64, 3, 32, 10))
# y jest tensorem wypelnionym liczbami 5 o ksztalcie (32, 10)
y = array(5, dim = c(32, 10))
# obiekt wyjsciowy ma taki sam ksztalt jak obiekt x
z = sweep(x, c(3, 4), y, pmax)

# Iloczyn tensorowy: z = x %*% y

naive_vector_dot = function(x, y){
  z = 0
  for (i in 1:length(x))
    z = z + x[[i]] * y[[i]]
  z
}

naive_matrix_vector_dot = function(x, y){
  z = rep(0, nrow(x))
  for (i in 1:nrow(x))
    for (j in 1:ncol(x))
      z[[i]] = z[[i]] + x[[i, j]] * y[[j]]
  z
}

naive_matrix_dot = function(x){
  z = matrix(0, nrow = nrow(x), ncol = ncol(y))
  for (i in 1:nrow(x))
    for (j in 1:ncol(y)){
      row_x = x[i,]
      column_y = y[,j]
      z[i, j] = naive_vector_dot(row_x, column_y)
    }
  z
}

# Zmiana ksztaltu tensora

x = matrix(c(0, 1,
           2, 3,
           4, 5),
           nrow = 3, ncol = 2, byrow = TRUE)
x

x = array_reshape(x, dim = c(6,1))
x
x = array_reshape(x, dim = c(2,3))
x

# transpozycja
 x = matrix(0, nrow = 300, ncol = 20)
 dim(x)
 t(x)
 dim(x)
 
 # # stochastyczny spadek wzdluz gradientu
 # past_velocity = 0
 # momentum = 0.1
 # while (loss > 0.01){
 #   params = get_current_parameters()
 #   w = params$w
 #   loss = params$loss
 #   gradient = params$gradient
 #   
 #   velocity = past_velocity * momentum + learning_rate * gradient
 #   w = w + momentum * velocity - learning_rate * gradient
 #   past_velocity = velocity
 #   
 #   update_parameter(w)
 # }
 
 # ctrl + shift + m == %>% !!!
