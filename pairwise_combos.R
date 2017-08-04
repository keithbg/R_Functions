## Script to obtain pairwise matches of site locations
## returns a factor variable of all site pairs separated by a ':'

# input.vec = a character vector input values to calculate pairwise matches


pairwise_combos <- function(input.vec){

  if(class(input.vec) != "character" ){
    stop("input.vec must be class: character")
  }

  # Create data frame of pairwise combinations using expand.grid
  pairs <- expand.grid(input.vec, input.vec)

  # Make vector of unique combinations of sites
  pairs.paste <- paste(pairs$Var2, pairs$Var1, sep=":")

  # Transform into a matrix
  pairs.paste.mat <- matrix(pairs.paste, sqrt(length(pairs.paste)))

  # Remove redundant values (i.e. upper triangle of the matrix)
  # by assigning a known value to the upper triangle, then subseting the matrix
  # by that known value.
  # This value is specified in the function and can be numeric or character
  # depending on the class of your data
  pairs.paste.mat.2 <- ifelse(lower.tri(pairs.paste.mat, diag= FALSE) == F,
                        "0",
                        pairs.paste.mat)

  # Return a numeric vector of only the lower triangle values from the matrix
  pairs.lower <- as.factor(matrix(pairs.paste.mat.2[pairs.paste.mat.2 != "0"],
                            ncol=1))


  return(pairs.lower)

}