# Script to batch import ibutton files from the same folder
# First, each file in the folder will be combined into a large list
# Then the function will return a single data frame with all the ibutton data included
# Columns include temperature, date/time, date, year, and day of year 

# Created by KBG Jun-2015
# kbg@berkeley.edu


# Make sure data is saved as .csv 
# Make  sure the only .csv files in the folder are the ibutton files

# Columns need to be titled: Date/time, Unit, Value


# import.pathway = the pathway and folder name where the ibutton files are located


#### Batch importing function ####

ibutton.batch.import <- function(import.pathway){

## Set working directory
  setwd(import.pathway)
  
## Read in filesnames
  temp <- list.files(pattern="*.csv")
	myfiles <- lapply(temp, read.delim)

## Give each element in the list the appropriate name
  lnames <- substring(temp, 1, nchar(temp)-4)  
  names(myfiles)<-lnames

## Remove the first 17 lines of metadata from the ibutton .csv files
  myfiles2 <- lapply(myfiles, "[", -c(1:17), 1)
  myfiles2 <- lapply(myfiles2, droplevels)
  
## Make each element in the list into a standard data frame  
  ibutton.to.dataframe<-function(x){
  
    char.data<-as.character(x)
    char.to.vector<-unlist(strsplit(char.data, ","))
    vector.to.matrix<-matrix(char.to.vector, 
                             nrow = round(length(char.to.vector)/3), 
                             ncol = 3, byrow = TRUE)
    df1<-as.data.frame(vector.to.matrix)
    colnames(df1)<-c("DateTime", "Unit", "Value")
    df1<-df1[-1,]
    return(df1)
  }

## Apply function "ibutton.to.dataframe" over each element in the list   
  myfiles.df <- lapply(myfiles2, ibutton.to.dataframe)  

## Add an iButton ID column to each element in the list
  myfiles.df2 <- mapply(cbind, myfiles.df, "ID"=lnames, SIMPLIFY=F)

## Combine the elements of the list into one single dataframe
  ib.df <- do.call("rbind", myfiles.df2)

## Rename rows
  rownames(ib.df) <- c(1:length(rownames(ib.df)))

## Make temp a numeric class
  ib.df$Value <- as.numeric(as.character(ib.df$Value))

## Format Date/Time to POSIX and add Day-Month, Year, and Julian day columns  
  ib.df$DateTime <- as.POSIXct(strptime(ib.df$DateTime, "%m/%d/%y %I:%M:%S %p"))

  ib.df$Date <- as.Date(strftime(ib.df$DateTime, format = "%Y/%m/%d"))
  ib.df$Year <- as.factor(strftime(ib.df$DateTime, format = "%Y"))

  # Day of Year
  ib.df$DOY <- strftime(ib.df$DateTime, format = "%d-%b")
  ib.df$DOY <- as.Date(ib.df$DOY, "%d-%b")


return(ib.df)
}


