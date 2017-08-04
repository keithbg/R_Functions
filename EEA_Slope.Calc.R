## Calculates the slopes of the enzyme activity using a linear model
## takes input from the EEA_ActivityCalc script

calculate.eea.slopes <- function(ea.df){
  # Initialize values and vectors
    row <- 1
    slope.value <-as.numeric()
    r.2 <- as.numeric()
    plateID.loop <- as.character(NULL)
    sample.loop <- as.numeric(NULL)

  ## drop levels of any plateIDs not in the ea.df
    ea.df$plateID <- droplevels(ea.df$plateID)
    #table(ea.df$plateID)

  ## Loop to calculate slopes  length(levels(ea.df$plateID))
  for(i in 1:length(levels(ea.df$plateID))){
    pID <- levels(ea.df$plateID)[i] #plate ID
    for(j in 1:3){
      sampID <- levels(ea.df$plate.sample)[j] # samples 1-3 on the plate

      get.slope <- lm(enz.activity ~ hours,
                      data= ea.df[which(ea.df$plateID == pID &
                                             ea.df$plate.sample == sampID), ])

      slope.value[row] <- summary(get.slope)$coefficients[2]
      r.2[row] <- summary(get.slope)$r.squared

      plateID.loop[row] <- pID
      sample.loop[row] <- sampID
      row <- row + 1
    }

  }
  activity.slopes <- data.frame(plateID.loop, sample.loop, slope.value, r.2)
  return(activity.slopes)
}









