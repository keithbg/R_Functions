## Script to import LiCor data

## files must be saved as .TXT and be tab delimited
## script can either import a single file or batch import files in a directory

licor.import <- function(batch= FALSE, filename, pathway){

#### Single file import ####
if(batch== FALSE){

## Read in file
  licor.df <- read.table(filename, sep= "\t", fill= TRUE, header= TRUE, skip= 6)

## Remove appended data headers
  append.row.num <- grep("Model", licor.df$DATAH)
  if(length(append.row.num) > 0){
  licor.df <- licor.df[-(append.row.num:(append.row.num + 5)), ]
  }

## Format columns
  licor.df <- subset(licor.df, select= -c(X))
  licor.df <- transform(licor.df,
                        Date= as.Date(Date, format= "%Y-%m-%d"),
                        RTime= strptime(Time, format= "%H:%M:%S"))
  licor.df[, 5:14] <- apply(licor.df[, 5:14], 2, function(x) as.numeric(as.character(x)))

return(licor.df)

}



#### Batch import ####
## CODE IS UNWRITTEN FOR THIS

#
# if(batch== TRUE){
#
#   ## Give each element in the list the appropriate name
#   lnames <- substring(temp, 1, nchar(temp)-4)
#   names(myfiles) <- lnames
#
#   ## Function to remove the first lines of metadata from the ibutton .csv files
#   remove.metadata <- function(ibutton.data){
#     row.data.begins <- grep("DATAH", ibutton.data[, 1])
#     temp.data <- ibutton.data[-(1:(row.data.begins-1)), ]
#     return(temp.data)
#   }
#
#   ## Apply the function <remove.metadata>  over each element in the list
#   myfiles.no.metadata <- lapply(myfiles, remove.metadata)
#   myfiles.no.metadata <- lapply(myfiles.no.metadata, droplevels)
# }


}