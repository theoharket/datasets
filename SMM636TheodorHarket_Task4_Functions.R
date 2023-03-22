
#Fisher discrimant analysis-function
myFDA <- function(x, y) {
  
  #Since it is unclear in the task description whether we want the Fisher's Linear Discriminant Analysis, or Fisher's Discriminant Analysis.
  #This function is implementing the Fisher's Linear Discriminant Analysis.
  
  #It's important to note that this implementation assumes that the covariance matrix is the same for both classes, 
  #the equal covariance assumption. If this assumption does not hold, this function will not have the correct implementation.
  
  #Steps in this function:
  #1. Calculate the within-class scatter matrix for both the classes by subtracting every column for both of the classes from the mean of the feature
  #2. Multiplying the corresponding SW0 and SW1 separately, and then adding them to get the scatter matrix combined for both classes. 
          #Giving me a pxp matrix
  #3. Calculating the difference in means to get the direction of class mean difference
  #4. Taking the inverse of the weights, multiplying it with the difference in class means
  #5. Returning the weights
  
  
  #1.
  SW0 = (x[y == 0,] - colMeans(x[y == 0,]))
  SW1 = (x[y == 1,] - colMeans(x[y == 1,]))
  SW0 = (t(SW0) %*% (SW0))
  SW1 = (t(SW1) %*% (SW1))
  
  #2.
  S_W = SW0 + SW1
  
  #3.
  class_means = colMeans(x[y == 1,]) - colMeans(x[y == 0,])
  
  #4.
  w = solve(S_W) %*% class_means
  
  #5.
  return(w)
}

# Min-Max scaling function
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
