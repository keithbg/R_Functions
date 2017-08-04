
# Script to batch import ibutton files from the same folder
# First, each file in the folder will be combined into a large list
# Then the function will return a single data frame with all the ibutton data included
# Columns include temperature, date/time, date, year, and day of year

# Created by KBG Jun-2015
# kbg@berkeley.edu


# Make sure data is saved as .csv
# Make sure the only .csv files in the folder are the ibutton files

# Columns need to be titled: Date/time, Unit, Value

# import.pathway = the pathway and folder name where the ibutton files are located
  #import.pathway <- "/Users/KeithBG/Dropbox/ibuttonTroubleshooting/Bad"


#### Batch importing function ####

ibutton.batch.import <- function(import.pathway){

## Set working directory
  setwd(import.pathway)

## Read in file names
  temp <- list.files(pattern="*.csv")
	myfiles <- lapply(temp, read.delim)

## Give each element in the list the appropriate name
  lnames <- substring(temp, 1, nchar(temp)-4)
  names(myfiles) <- lnames

## Function to remove the first lines of metadata from the ibutton .csv files
  remove.metadata <- function(ibutton.data){
    row.data.begins <- grep("Date/Time", ibutton.data[, 1])
    temp.data <- ibutton.data[-(1:(row.data.begins-1)), ]
    return(temp.data)
  }

## Apply the function <remove.metadata>  over each element in the list
  myfiles.no.metadata <- lapply(myfiles, remove.metadata)
  myfiles.no.metadata <- lapply(myfiles.no.metadata, droplevels)

## Function to make each element in the list into a standard data frame
  ibutton.to.dataframe <- function(x){
    char.data <- as.character(x)
    char.to.vector <- unlist(strsplit(char.data, ","))
    vector.to.matrix <- matrix(char.to.vector,
                             nrow = round(length(char.to.vector)/3),
                             ncol = 3, byrow = TRUE)
    df1 <- as.data.frame(vector.to.matrix)
    colnames(df1) <- c("DateTime", "Unit", "Value")
    df1 <- df1[-1,]
    return(df1)
  }

## Apply function <ibutton.to.dataframe> over each element in the list
  myfiles.df <- lapply(myfiles.no.metadata, ibutton.to.dataframe)

## Add an iButton ID column to each element in the list
  myfiles.df2 <- mapply(cbind, myfiles.df, "ID"=lnames, SIMPLIFY=F)

## Combine the elements of the list into one single dataframe
  ib.df <- do.call("rbind", myfiles.df2)

## Rename rows
  rownames(ib.df) <- c(1:length(rownames(ib.df)))

## Make temp a numeric class
  ib.df$Value  <- as.numeric(as.character(ib.df$Value))

## Format DateTime to POSIXct
  # Format all 12 Hour clock .csv files
    ib.df$DateTimeR <- as.POSIXct(strptime(ib.df$DateTime, "%m/%d/%y %I:%M:%S %p"))

  # Format 24 hour clock 2 digit year .csv files
    if (length(ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTimeR"]) > 0) {
      ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTimeR"]  <- as.POSIXct(strptime(
        ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTime"],
        "%m/%d/%y %H:%M"))
    }

  # Format 24 hour clock and 4 digit year .csv files
    if (length(ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTimeR"]) > 0) {
      ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTimeR"]  <- as.POSIXct(strptime(
        ib.df[which(is.na(ib.df$DateTimeR) == TRUE), "DateTime"],
        "%m/%d/%Y %H:%M"))
    }

  # Check for NAs, return error if NAs are present
  if (length(ib.df[which(is.na(ib.df$DateTimeR) == TRUE),"DateTimeR"]) > 0) {
    print("NAs generated in DateTime, check 12/24 hour clock formatting"); return()
  }

##  Add Date, Year, and Day of Year columns
  ib.df$Date<-as.Date(strftime(ib.df$DateTimeR, format = "%Y-%m-%d"))
  ib.df$Year<-as.factor(strftime(ib.df$DateTimeR, format = "%Y"))

  ib.df$DOY<-strftime(ib.df$DateTimeR, format = "%d-%b") # Day of Year step 1
  ib.df$DOY<-as.Date(ib.df$DOY, "%d-%b") # Day of Year step 2

## Output the dataframe
  return(ib.df)

}




