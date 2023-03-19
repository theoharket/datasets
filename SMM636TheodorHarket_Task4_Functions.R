library(caret)
#Fisher discrimant analysis-function
myFDA <- function(X, y) {
  
  
  #Calculate the within-class scatter matrix for both the classes
  SW0 = t(X[y == 0,] - colMeans(X[y == 0,])) %*% (X[y == 0,] - colMeans(X[y == 0,]))
  SW1 = t(X[y == 1,] - colMeans(X[y == 1,])) %*% (X[y == 1,] - colMeans(X[y == 1,]))
  
  #Calculating the scatter matrix for both classes
  S_W = SW0 + SW1
  
  #Calculating the difference in means to get the direction of class mean difference
  class_means = colMeans(X[y == 1,]) - colMeans(X[y == 0,])
  
  
  # Calculate the linear discriminant
  w = solve(S_W) %*% class_means
  
  return(w)
}

# Min-Max scaling function
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
