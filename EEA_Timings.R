## Calculate timings for EEA analyses

calculate.timings <- function(timings.pathway){

## Read in CSV file
  times <- read.csv(timings.pathway)
  times <- transform(times,
                     T0 = as.POSIXct(strptime(as.character(times$T0), "%H:%M")),
                     T1 = as.POSIXct(strptime(as.character(times$T1), "%H:%M")),
                     T2 = as.POSIXct(strptime(as.character(times$T2), "%H:%M")),
                     T3 = as.POSIXct(strptime(as.character(times$T3), "%H:%M")),
                     T4 = as.POSIXct(strptime(as.character(times$T4), "%H:%M")))

  ## Calculate the difference in hours between T0 and the different readings
  library(reshape2)
  dif.times <- transform(times,
                         d1 = as.numeric(difftime(times$T1, times$T0, units = "hours")),
                         d2 = as.numeric(difftime(times$T2, times$T0, units = "hours")),
                         d3 = as.numeric(difftime(times$T3, times$T0, units = "hours")),
                         d4 = as.numeric(difftime(times$T4, times$T0, units = "hours")))
  dif.times <- dif.times[, c(1, 7:10)]
  names(dif.times)[2:5] <- c("1", "2", "3", "4")

  ## Melt into long format
  dif.times.L <- with(dif.times, melt(dif.times, measure.vars= c("1", "2", "3", "4") , variable.name= "read", value.name= "hours"))

  return(dif.times.L)
  }