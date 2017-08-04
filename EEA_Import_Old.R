# Script inputs a csv for each EEA plate
# then combines all csv's into a single dataframe

# created by KBG April 2016

# Make sure data is saved as .csv
# Make  sure the only .csv files in the folder are the EEA plate files
# eea.import.pathway = the pathway and folder name where the EEA files are located
# sample.date = date of sampling, format: %Y-%m-%d, 2016-04-10

#eea.import.pathway <- "/Users/KeithBG/Documents/UC Berkeley/2016 Spring Classes/EEA_DOC/EEA/EEA_25Apr2016/CSV"


#### Batch importing function ####

eea.batch.import <- function(eea.import.pathway, date){

  ## Set working directory
    setwd(eea.import.pathway)

  ## Read in filesnames
    eea <- list.files(pattern="*.csv")
    eea.files <- lapply(eea, read.csv)

  ## Give each element in the list the appropriate name
    lnames <- substring(eea, 1, nchar(eea)-4) # drops the .csv extension
    names(eea.files) <- lnames

  ## Add a EEA plate ID column to each element in the list
    eea.files2 <- mapply(cbind, eea.files, "plateID"= lnames, SIMPLIFY=F)

  ## Add an enzyme ID, siteID, and plate # column
    eea.files2 <- mapply(cbind, eea.files2, "enzyme"= gsub("_.*$", "", lnames), SIMPLIFY=F)
    eea.files2 <- mapply(cbind, eea.files2, "siteID"= gsub(".*_", "", lnames), SIMPLIFY=F)
    eea.files2 <- mapply(cbind, eea.files2, "plate.num"= as.numeric(gsub("[^0-9]", "", lnames)), SIMPLIFY=F)

  ## Combine the elements of the list into one single dataframe
    eea.df <- do.call("rbind", eea.files2)

  ## Add a date column
    eea.df$date <- as.Date(sample.date, "%Y-%m-%d")

  ## Rename columns, rename rows, reorder columns,
  ### and remove the excitation/emission wavelength column
    names(eea.df)[2:13] <- paste("c", seq(1:12), sep="")
    names(eea.df)[1] <- "read"
    eea.df <- eea.df[, -which(names(eea.df) %in% "X.12")]
    eea.df <- eea.df[, c(18, 14, 15, 16, 17, 1, 2:13)]
    rownames(eea.df) <- c(1:length(rownames(eea.df)))

  ## Remove number from siteID
    eea.df$siteID <- as.factor(gsub("\\d", "", eea.df$siteID))

  ## Remove NA rows between reads
    eea.df <- eea.df[-which(is.na(eea.df$read) == T), ]

  ## Write a CSV file
    #write.csv(eea.df, file = eea.export.pathway, row.names= FALSE)

  ## Return the eea.df data frame
    return(eea.df)
}
