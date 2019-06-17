#### Calculating production rates 


rm(list=ls())
setwd("~/OneDrive - nd.edu/underc-field-2019/gc2019underc/")

# Load libraries and functions 
source("~/OneDrive - nd.edu/R-functions/setTheme_BB.R")
library(reshape2)

# Read in the current GC 2019 database
gc=read.csv("currentDB/GCdatabase2019.csv", stringsAsFactors = F)
# Function for calculating CH4 production rate 
calcCH4prod=function(df, lakeIDs){
  
  outProd=data.frame()
  outAdjustedConc=data.frame()
  treatments=c("control", "algal", "BES", "algalBES")
  
  for(k in 1:length(lakeIDs)){
    temp=df[df$lakeID==lakeIDs[k],]
    for(i in 1:length(treatments)){
      temp2=temp[temp$depthTop==treatments[i],]
      for(j in 1:3){
        temp3=temp2[temp2$replicate==j,]
        
        # adjusting for N2 added for repeated sampling
        # for UNDERC 2012 headspace vol=219-50-50 ml and 10 ml of sample was taken and replaced with 10ml of N2 each time
        HSvol=(250-50-50)/1000
        sampVol=0.01
        N2pctadd=sampVol/HSvol
        
        if(length(unique(temp3$subsampleDateTime))>2){
          forRate=data.frame(lakeID=lakeIDs[k], treatment=treatments[i],
                             sample_times=strptime(temp3$subsampleDateTime,format="%m/%d/%y %H:%M"),
                             conc=temp3$CH4original_umolL, 
                             rep=temp3$replicate)
          forRate$incub_days=as.numeric(forRate$sample_times-forRate$sample_times[1])/(60*60*24)
          
          afterMeasure=forRate$conc*(1-N2pctadd)
          delta=forRate$conc[2:length(afterMeasure)]-afterMeasure[1:(length(afterMeasure)-1)]
          
          forRate$adj_conc=c(forRate$conc[1],forRate$conc[1:(length(forRate$conc)-1)]+delta)
          #Used to remove dates of incubation (day1)
          #forRate=forRate[-1,]
          
          fit=lm(forRate$adj_conc~forRate$incub_days)
          outTemp=data.frame(lakeID=lakeIDs[k], treatment=treatments[i], replicate=j, CH4prod=summary(fit)$coefficients[2,1], intercept= summary(fit)$coefficients[1,1])
          # Bind to outputs
          outProd=rbind(outProd, outTemp)
          outAdjustedConc=rbind(outAdjustedConc, forRate)
        }
      }
    }
  }
  outProd$algal=NA
  outProd$BES=NA
  
  outProd$algal[outProd$treatment=="control"]="no addition"
  outProd$algal[outProd$treatment=="algal"]="addition"
  outProd$algal[outProd$treatment=="BES"]="no addition"
  outProd$algal[outProd$treatment=="algalBES"]="addition"
  
  outProd$BES[outProd$treatment=="control"]="no addition"
  outProd$BES[outProd$treatment=="algal"]="no addition"
  outProd$BES[outProd$treatment=="BES"]="addition"
  outProd$BES[outProd$treatment=="algalBES"]="addition"
  
  return(list(outProd, outAdjustedConc))
}

# Subset data of interest
M3=gc[gc$projectID==33 & gc$lakeID%in%c("PE", "PA") & gc$dateSample=="5/23/19",]
M3=gc[gc$projectID==33 & gc$lakeID%in%c("HB", "CB") & grepl("lab", gc$depthClass),]

# Don't include first data point
M3=M3[M3$runDate!="5/24/19" & !(grepl("f", M3$flag)),]

# Calculated CH4 production 
data1=calcCH4prod(df=M3, lakeIDs=c("PE", "PA"))
data1=calcCH4prod(df=M3, lakeIDs=c("HB", "CB"))

# Get dataframes of production rates and adjusted CH4 concentrations 
prod=data1[[1]]
conc=data1[[2]]
# Run ANOVA analysis on both lakes together and seperately
summary(aov(CH4prod~algal+BES, data=prod))
summary(aov(CH4prod~algal+BES, data=prod[prod$lakeID=="PE",]))
summary(aov(CH4prod~algal+BES, data=prod[prod$lakeID=="PA",]))

# Plot
plot1=ggplot(conc, aes(x=incub_days, y=adj_conc, group=rep, color=as.factor(rep)))+
  geom_point(size=3)+
  facet_grid(lakeID~treatment)+
  guides(color=F)+xlab("Incubation days")+
  stat_smooth(method="lm", se=F)+
  ylab("Adjusted CH4 concentration\numol per L")
plot1

plot2=ggplot(prod, aes(x=treatment, y=CH4prod))+
  stat_summary(geom="bar", fun.y="mean", fill="lightblue")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.3)+
  facet_grid(~lakeID)+
  ylab("CH4 production rate\numol per L per day")+xlab("Treatment")
plot2
plot_grid(plot1, plot2, nrow=2, align="hv", axis="lrbt", labels=c("a", "b"))

ggsave("3M-PE&PA-190604.pdf", width=8, height=10)
