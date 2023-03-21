
#Fisher discrimant analysis-function
myFDA <- function(X, y) {
  
  
  #Calculate the within-class scatter matrix for both the classes
  SW0 = (X[y == 0,] - colMeans(X[y == 0,]))
  SW1 = (X[y == 1,] - colMeans(X[y == 1,]))
  
  diag = ncol(X)
  SW0 = (t(SW0) %*% (SW0))#+diag(diag)*0.01
  SW1 = (t(SW1) %*% (SW1))#+diag(diag)*0.01
  
  #Calculating the scatter matrix for both classes
  S_W = SW0 + SW1
  
  #Calculating the difference in means to get the direction of class mean difference
  class_means = colMeans(X[y == 1,]) - colMeans(X[y == 0,])
  

  #Taking the inverse of the weights, multiplying it with the difference in class means
  w = solve(S_W) %*% class_means
  
  #Returning the corresponding weights
  return(w)
}

# Min-Max scaling function
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
