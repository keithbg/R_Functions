#Script to calculate daily temperature metrics and make graphs from ibutton data

# Created by Keith Bouma-Gregson Oct-2013
# Revised by KBG Jun-2015
# kbg@berkeley.edu


# Make sure data is saved as .csv 
# Columns need to be titled: Date/time, Unit, Value

# Pathway to access files
  import.pathway<-'/Users/keithgregson/Documents/UC Berkeley/2015 Summer Research/PAM Experiment/HeatShock_5Jun2015/HeatShock_iButtons'
	

# I use ggplot to make graphs, so load these librarys or the other packages you use
	
	# Use install.packages() if you haven't installed the packages on your computer yet
	
	library(ggplot2)
	library(RColorBrewer)
	library(reshape2)

# This is a Theme I wrote for ggplot that formats the plots in a way I like
plot_theme1<-theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(colour="black"), axis.line = element_line(colour="black"))



#Read single data files
	ib<-read.csv(file.choose())

## Or batch import files
  setwd(import.pathway)
  temp <- list.files(pattern="*.csv")
	myfiles <- lapply(temp, read.delim)
  names(myfiles)<-c(paste("Array", rep(1:12), sep=""))
  

## Remove the first 17 lines of metadata from the .csv files
  myfiles2 <- lapply(myfiles, "[", -c(1:17), 1)
  myfiles2 <- lapply(myfiles2, droplevels)
  length(myfiles2)
  

## Make into a dataframe  
  ibutton.to.dataframe<-function(x){
  
  char.data<-as.character(x)
  char.to.vector<-unlist(strsplit(char.data, ","))
  vector.to.matrix<-matrix(char.to.vector, nrow = round(length(char.to.vector)/3), ncol = 3, byrow = TRUE)
  
  df1<-as.data.frame(vector.to.matrix)
  colnames(df1)<-c("Date/Time", "Unit", "Value")
  df1<-df1[-1,]
  return(df1)
}
 
   
myfiles3<-lapply(myfiles2, ibutton.to.dataframe)  

  
  paste("Array", rep(1:12), sep="")
  names(myfiles2)
  str(myfiles2)
  
  strsplit(pamdat[n.ilc+j],";")
head(test)
  str(test)
  class(myfiles2[[1]])
  levels(test)
  
  #Create column for date and format ib$Date.Time to POSIX and change column name
	dt<-as.POSIXlt(strptime(ib$Date.Time, "%m/%d/%y"))
	ib$Date.Time<-as.POSIXlt(strptime(ib$Date.Time, "%m/%d/%y %H:%M"))
	ib$Date<-as.Date(dt)
	head(ib)
	sapply(ib, class)


#Calculate daily min, max, avg, and stdev for each day
if (sum.data == "Y"){
	ib.sum<-ddply(lc2, sum.params, summarise,
	              minT = min(Value),
	              maxT = max(Value),
	              sdT = sd(Value),
	              
	)
}
	str(ib.sum)
  
  
###OLD SUMMARIZING#####################
	minimum<-aggregate(Temp ~ Date + Site, data=ib, FUN=min)
	colnames(minimum)[3]<-"minTemp"
	head(minimum)

	maximum<-aggregate(Temp ~ Date + Site, data=ib, FUN=max)
	colnames(maximum)[3]<-"maxTemp"
	head(maximum)
	
	average<-aggregate(Temp ~ Date + Site, data=ib, FUN=mean)
	colnames(average)[3]<-"Avg"
	head(average)

	stdev<-aggregate(Temp ~ Date + Site, data=ib, FUN=sd)
	colnames(stdev)[3]<-"stdev"
	head(stdev)

#Combine summary stats into a new data frame called stats

	stats<-minimum
	stats<-cbind(minimum, maximum[3],average[3],stdev[3])
	head(stats)
##############################
  
  
# Plot daily data for each site
	
	# Use this script if you want to only plot data from a specific site		#stats[which(as.numeric(stats$Site)==6),]

# Initializes data to be plotted. y = defines which column of data from the stats data frame will be plotted options are (Avg, minTemp, maxTemp)
	
	p<-ggplot(data=stats, aes(x=Date,y=Avg, group=Site))

# Makes a line plot	
	p + geom_line(aes(color=Site)) + plot_theme1

# When y=Avg, this plot will draw the average temp and then add a ribbon deliniating the max and min temps for the day. It works best when only graphing a few sites per plot.
	p + geom_ribbon(aes(ymin=minTemp, ymax=maxTemp), fill="blue", alpha=0.1) + geom_line(aes(color=Site)) + geom_vline(xintercept = as.numeric(sampdate), color = "grey", linetype="longdash") + plot_theme1

# Plot a frequency distribution of temperatures, x = Avg, maxTemp, or minTemp

	p2<-ggplot(data=stats, aes(x=Avg, group=Site))
	
	p2 + geom_density(aes(color=Site)) + plot_theme1



# To save your file 
	# 1. save your plot as an object e.g. ( image <- p + geom_line()....)
	# 2. use the below script, and file will be saved in your working directory
		
		# ggsave(image,file="Myplot.png", height=# of inches, width = # of inches, units="in")


