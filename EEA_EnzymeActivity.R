

#### Calculate the enzyme activity from the EEA import script
#   "/Users/KeithBG/R_Functions/EEA_Import.R"

#eea.data.frame <- eea.data[which(eea.data$plateID == "LAMP_ELDER1"), ]
#eea.data.frame <- eea.df

#sample.plate.ID.pathway <- "/Users/KeithBG/Documents/UC Berkeley/2016 Spring Classes/EEA_DOC/EEA/EEA_2016_08_10/SamplePlateIDsCSV.csv"


eea.enzyme.activity <- function(eea.data.frame, sample.plate.ID.pathway){
  library(plyr)
  eea.avg <- ddply(eea.data.frame, .(plateID, read), summarise,
                   c1 = mean(c1),
                   c2 = mean(c2),
                   c3 = mean(c3),
                   c4 = mean(c4),
                   c5 = mean(c5),
                   c6 = mean(c6),
                   c7 = mean(c7),
                   c8 = mean(c8),
                   c9 = mean(c9),
                   c10 = mean(c10),
                   c11 = mean(c11),
                   c12 = mean(c12)
  )

##### Separate samples from each plate and transform into long format ####

  names(eea.avg) <- c("plateID", "read", rep(c("quench", "samp", "assay"), 4))
  # create a df for the control data
  control <- eea.avg[, 1:5]
  names(control)[3:5] <- c("c.blank", "c.ref", "c.neg")
  control <- rbind(control, control, control)

  # create df for the 3 samples
  samp1 <- eea.avg[, c(1,2, 6:8)]
  samp2 <- eea.avg[, c(1,2, 9:11)]
  samp3 <- eea.avg[, c(1,2, 12:14)]
  samples <- rbind(samp1, samp2, samp3)

  # combine the control and sample df
  # now each sample row corresponds to the correct control data row
  eea <- cbind(control, samples[3:5])
  eea$plate.sample <- rep(c(1, 2, 3), each= length(row.names(eea.avg)))
  eea$plate.sample <- as.factor(eea$plate.sample)
  eea <- eea[, c(1, 2, 9, 3:8)]

#### Identify the samples on each plate and volumes and area/biomass
  identities <- read.csv(sample.plate.ID.pathway)
  eea.ident <- merge(identities, eea, all.y= T)

#### Calculate the Enzyme Activity ####

  ref.std <- with(eea.ident, c.ref - c.blank)
  neg.con <- with(eea.ident, c.neg - c.blank)
  quench.b <- with(eea.ident, quench - c.blank)
  samp.b <- with(eea.ident, samp - c.blank)
  assay.b <- with(eea.ident, assay - c.blank)
  quench.coef <- (quench.b - samp.b) / ref.std
  emmission.coef <- ref.std / 0.5
  net.fluor <- ((assay.b - samp.b) / quench.coef) - neg.con
  enz.activity <- (net.fluor * eea.ident$vol) / (emmission.coef * 0.2 * eea.ident$area)
  # enz.activity units = nmol cm^-2 h^-1

  enz.act <- cbind(eea.ident[, c(1:5, 8)], enz.activity)

  return(enz.act)
}