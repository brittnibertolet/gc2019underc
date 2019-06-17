rm(list=ls())
setwd("~/OneDrive - nd.edu/underc-field-2019/gc2019underc/")

# Load libraries and functions 
source("~/OneDrive - nd.edu/R-functions/setTheme_BB.R")
library(reshape2)

# Read in the current GC 2019 database
gc=read.csv("currentDB/GCdatabase2019.csv", stringsAsFactors = F)

date="5/31/19"
# Function for plotting 3M survey data 
plot3M=function(date=dateSample){
  temp=gc[gc$projectID==33 & grepl(date, gc$dateSample) & grepl("lab", gc$depthClass),]
  temp$depthTop=factor(temp$depthTop, levels=c("control", "algal", "BES", "algalBES"))
  temp$replicate=as.character(temp$replicate)
  ggplot(temp, aes(x=runDate, y=CH4original_umolL, color=replicate, group=replicate))+
    geom_point()+
    facet_grid(lakeID~depthTop)+
    stat_smooth(method="lm", se=F)+
    guides(color=F)
}
# Function for plotting PE & PA profile data
plotProfilePP=function(){
  temp=gc[gc$projectID==34 & grepl("pCO2", gc$subsampleClass),]
  temp$depthTop=as.numeric(temp$depthTop)
  temp=temp[, c(c(3,4,5,8,25,26))]
  temp=melt(temp, id.vars = c("lakeID", "siteName", "dateSample", "depthTop"), variable.name = "Carbon", value.name = "Concentration")
  
  ggplot(temp, aes(x=Concentration, y=depthTop))+geom_point(aes(color=Carbon))+
    facet_grid(lakeID~dateSample, scales = "free_y")+
    scale_y_reverse()+
    ylab("Depth (m)")+xlab("Concentration (umol L-1)")+
    scale_color_discrete(name = "", labels = c("Methane", "Carbon Dioxide"))
}
# Function for plotting PE & PA production data for DeepHole
plotProdPP=function(){
  temp=gc[gc$projectID=="34" & grepl("methSlurry", gc$subsampleClass) & grepl("DeepHole", gc$siteName),]
  temp$replicate=as.character(temp$replicate)
  ggplot(temp, aes(x=runDate, y=CH4original_umolL, color=replicate, group=replicate))+
    geom_point()+
    facet_grid(lakeID~dateSample, scales="free")+
    guides(color=F)
}

# Plot data for a certain start date of the 3M survey
plot3M(date="5/23/19")
plot3M(date="5/31/19")

# Plot all current PE & PA profile data
plotProfilePP()

# Plot all current PE & PA production data 
plotProdPP()



  