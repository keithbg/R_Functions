## Function to round to the nearest group of minutes with POSIXct format
## Downloaded from Stackoverflow 10-Oct-2015: http://stackoverflow.com/questions/10862056/rounding-time-to-nearest-quarter-hour
## min.rnd variable added by KBG

## x = POSIXct vector
## min.rnd = group of minutes to round to
## convert = return just H:M when TRUE otherwise returns all original units


POSIXct.Round <- function (x, min.rnd, convert = TRUE)  {
  x <- as.POSIXlt(x)
  mins <- x$min
  mult <- mins %/% min.rnd
  remain <- mins %% min.rnd
  if(remain > 7L || (remain == 7L && x$sec > 29))
    mult <- mult + 1
  if(mult > 3) {
    x$min <- 0
    x <- x + 3600
  } else {
    x$min <- min.rnd * mult
  }
  x <- trunc.POSIXt(x, units = "mins")
  if(convert) {
    x <- format(x, format = "%H:%M")
  }
  x
}


