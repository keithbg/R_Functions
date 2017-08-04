

#### Calculate the net fluorescence from the EEA import script
#   "/Users/KeithBG/R_Functions/EEA_Import.R"

eea.data.frame <- eea.data[which(eea.data$plateID == "LAMP_ELDER1"), ]
vol.area.csv.pathway <- vol.area.pathway
#eea.data.frame <- eea.data

eea.enzyme.activity <- function(eea.data.frame, vol.area.csv.pathway){
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
  eea$sample <- rep(c(1, 2, 3), each= length(row.names(eea.avg)))
  eea <- eea[, c(1, 9, 2:8)]

#### Add volume and area/biomass ####
  vol.area <- read.csv(vol.area.csv.pathway) # read in volume data
  names(vol.area)[2] <- "sample.volID"

  # rename columns for merge with vol.area data frame
  eea$siteID <- gsub(".*_", "", eea$plateID)
  eea$siteID  <- as.factor(gsub("[0-9]", "", eea$siteID))
  eea$plate.num <- as.numeric(gsub("[^0-9]", "", eea$plateID))
  eea$plate.num <- ifelse(is.na(eea$plate.num) == T, 1, eea$plate.num)
  eea$sample.volID <- ifelse(eea$plate.num == 2, (eea$sample + 3), eea$sample )

  eea.vol <- merge(eea, vol.area, all.x = T)


#### Calculate the Enzyme Activity ####

  ref.std <- with(eea.vol, c.ref - c.blank)
  neg.con <- with(eea.vol, c.neg - c.blank)
  quench.b <- with(eea.vol, quench - c.blank)
  samp.b <- with(eea.vol, samp - c.blank)
  assay.b <- with(eea.vol, assay - c.blank)
  quench.coef <- (quench.b - samp.b) / ref.std
  emmission.coef <- ref.std / 0.5
  net.fluor <- ((assay.b - samp.b) / quench.coef) - neg.con
  enz.activity <- (net.fluor * eea.vol$vol) / (emmission.coef * 0.2 * eea.vol$area)

  enz.act <- cbind(eea.vol[, c(1:5, 12)], enz.activity)

  return(enz.act)
}