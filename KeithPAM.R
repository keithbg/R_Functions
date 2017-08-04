

## Read in libraries
library(ggplot2)

## Source the R script that reformats data from the PAM
source('/Users/keithgregson/R_Functions/KBGparsePAM.R')


## File you want to read
pathway<-'/Users/keithgregson/Desktop/Peripalooza/Pam_15May2015TXT.txt'


## Reformat the data from the PAM 
PAMdf<-parsePAM_KBG(pathway)

## Regression parameters
rp<-as.data.frame(PAMdf[1])
colnames(rp)<-gsub("Regression.parameters.", "", colnames(rp))
rp$Mem<-as.factor(rp$Mem)

## Light Curve data
lc<-as.data.frame(PAMdf[2])
colnames(lc)<-gsub("", "", colnames(lc))
lc$Mem<-as.factor(lc$Mem)
lc$Curve<-as.factor(lc$Curve)


## Inspect data
str(rp)
str(lc)

## Subsets of data to plot
plot.data.lc<-lc
plot.data.rp<-rp

## Plot Light Curve Data

lc.plot<-ggplot(data=plot.data, aes(x = PAR, y = ETR, group = as.factor(Mem))) 

lc.plot + geom_line(aes(color=Mem), size = 0.75) + geom_point(aes(color=Mem), size = 4) + labs(x = expression(paste("PAR ",mu,"Mols ",m^{-2}," ", s^{-1} )), y = "ETR") + ggtitle("Rapid Light Curve") + theme_bw(base_size=20)


## Plot Fv/Fm

FvFm.plot<-lc.plot<-ggplot(data=plot.data, aes(x = Mem, y = Fv.Fm..., group = as.factor(Curve))) 

FvFm.plot + geom_point(aes(color=Curve), size = 4) + labs(x = "Sample ID", y = "Fv/Fm") + ggtitle("Fv/Fm") + theme_bw(base_size=20)

## Plot NPQ
NPQ.plot<-lc.plot<-ggplot(data=plot.data, aes(x = PAR, y = NPQ, group = as.factor(Mem))) 

NPQ.plot + geom_point(aes(color=Mem, shape = Curve), size = 4) + labs(x = expression(paste("PAR ",mu,"Mols ",m^{-2}," ", s^{-1} )), y = "NPQ") + ggtitle("NPQ") + theme_bw(base_size=20)

str(rp)
## Plot ETR Max

ETRm.plot<-lc.plot<-ggplot(data=plot.data.rp, aes(x = Mem, y = REG1.ETRm, group = as.factor(Mem))) 

ETRm.plot + geom_point(aes(color=Mem), size = 4) + labs(x ="Sample ID", y = "ETR max") + ggtitle("ETR Max") + theme_bw(base_size=20)
