rm(list=ls())
# What samples to sample today?
# BLB
# 2019-06-06

# Load necessary libraries and functions
library(reshape2)
whatToSample=function(data=sched, date=today){
    data=melt(data, id.vars = c("project","lakeID", "initSample"), variable.name = "subSample", value.name = "date")
    toSample=data[grepl(date, data$date),]
    if(nrow(toSample)>0){
      return(toSample)
    }else{
      return(paste("No slurries to run today!"))
    }
}

# Set arguments for the whatToSample function
sched=read.csv("slurrySamplingSchedule.csv", stringsAsFactors = F)
today="6/13/19"

# Run function
whatToSample()
