## Script to remove 1/2 the data from a symmetrical matrix

# input.matrix = matrix with row and column names as the site names
# data.type = the name of the column holding the values, once reshaped into long format
# category.type = the types of categories the data values came from, default is site1 and site2
# diagonal = remove diagonal values, default = TRUE

remove_lower_triangle <- function(input.matrix, data.type, category.type= NULL, diagonal= TRUE){
  library(tidyr)

  if(class(input.matrix) != "matrix"){
    stop("input.matrix must be class matrix")
  }

  if(class(diagonal) != "logical"){
    stop("diagonal must be class logical: TRUE/FALSE or T/F")
  }

  if(is.null(category.type)){
    category.type <- "site"
    }

  # Sort rows and columns by increasing numbers
  input.matrix <- input.matrix[sort(row.names(input.matrix)),
                                          sort(colnames(input.matrix))]

  # turn lower triangle and diagonal into zeros
  if(diagonal == TRUE | diagonal == T){
  input.matrix[lower.tri(input.matrix, diag= T)] <- 0
  }

  if(diagonal == FALSE | diagonal == F){
    input.matrix[lower.tri(input.matrix, diag= F)] <- 0
  }


  # Reshape data frame into long format
  df <- as.data.frame(input.matrix)
  df$site1 <- row.names(input.matrix)
  # Make long format
  df.lowerA <- gather(df, key= site2, value= data, -site1)
  df.lower <- subset(df.lowerA, data != 0) # remove values from lower triangle
  df.lower$site1 <- as.factor(df.lower$site1)
  df.lower$site2 <- as.factor(df.lower$site2)
  df.lower <- df.lower[order(df.lower$site1, df.lower$site2),]

  # Rename columns according to specified arguments
  names(df.lower) <- c(paste(category.type, 1, sep=""), paste(category.type, 2, sep=""),  data.type)

  df.lower <- droplevels(df.lower)


  return(df.lower)
}

