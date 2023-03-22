
#Fisher discrimant analysis-function
myFDA <- function(x, y) {
  
  #Since it is unclear whether we want the Fisher's Linear Discriminant Analysis, or Fisher's Discriminant Analysis due to the task description being unclear
  #where we assume that the normality assumption is fulfilled, where we expect tjat tje 
  
  #Steps in this function:
  #1. Calculate the within-class scatter matrix for both the classes by subtracting every column for both of the classes from the mean of the feature
  #2. Multiplying the corresponding SW0 and SW1 separately, and then adding them to get the scatter matrix combined for both classes. 
          #Giving me a pxp matrix
  #3. Calculating the difference in means to get the direction of class mean difference
  
  
  #1.
  SW0 = (x[y == 0,] - colMeans(x[y == 0,]))
  SW1 = (x[y == 1,] - colMeans(x[y == 1,]))
  SW0 = (t(SW0) %*% (SW0))
  SW1 = (t(SW1) %*% (SW1))
  
  #2.
  S_W = SW0 + SW1
  
  #Calculating the difference in means to get the direction of class mean difference
  class_means = colMeans(x[y == 1,]) - colMeans(x[y == 0,])
  

  #Taking the inverse of the weights, multiplying it with the difference in class means
  w = solve(S_W) %*% class_means
  
  #Returning the corresponding weights
  return(w)
}

# Min-Max scaling function
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
