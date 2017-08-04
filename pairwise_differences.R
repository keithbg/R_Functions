## Script to obtain pairwise differences of data collected from a series of sites
## Returns a numeric vector with all pairwise differences of sample1 - sample2

# input.vec = a numeric vector input values to calculate pairwise differences
# absolute = a logical statement (TRUE or FALSE) indicating if you would like the
# function to return the actual differences (FALSE) or the absolute value of the
# differences (TRUE)

pairwise_diff <- function(input.vec, absolute){

  if(class(input.vec) != "numeric" & class(input.vec) != "integer"){
    stop("input.vec must be class: numeric or integer")
  }

  if(class(absolute) != "logical"){
    stop("absolute must be class logical (TRUE or FALSE")
  }

  # Create data frame of pairwise combinations using expand.grid
  pairs <- expand.grid(input.vec, input.vec)

  # Calculate differences between values. Expand.grid sorts by Var2 first, so
  # I substract Var2 - Var1
  if(absolute == FALSE){
    delta <- pairs$Var2 - pairs$Var1
  }

  if(absolute == TRUE){
    delta <- abs(pairs$Var2 - pairs$Var1)
  }

  # Transform into a matrix
  delta.mat <- matrix(delta, sqrt(length(delta)))

  # Remove redundant values (i.e. upper triangle of the matrix)
  # by assigning a known value to the upper triangle, then subseting the matrix
  # by that known value.
  # This value is specified in the function and can be numeric or character
  # depending on the class of your data
  delta.mat.2 <- ifelse(lower.tri(delta.mat, diag= FALSE) == F,
                            "A",
                            delta.mat)

  # Return a numeric vector of only the lower triangle values from the matrix
  delta.lower <- matrix(as.numeric(delta.mat.2[delta.mat.2 != "A"]),
                            ncol=1)

  return(delta.lower)

}