# All the data 
rm(list=ls())

# Change the working directory to the current folder to process
setwd("~/OneDrive - nd.edu/underc-field-2019/gc2019underc/rawData/batchProcessed_190610/")

# Read in functions and libraries
source("../../code/raw2summaries_UNDERC2019.R")
library(tidyverse)
library(reshape2)

# Read in standards file
standardsFile=read.table("~/OneDrive - nd.edu/underc-field-2019/data/GCstandards.txt", sep="\t", header=T)

# Get vectors of all the run folders and logs 
runs=list.files()
logs=paste0("run", runs, ".csv")

# Calculate the concentration with the for loop
for(i in 1:length(runs)){
  #read in runfile, process, and reformat for merging with summary file 
  print(runs[i])
  temp=GCraw2sum(runs[i])
  temp=temp[(temp$signal==2 & temp$RT>2.1 & temp$RT<2.2) | (temp$signal==1 & temp$RT>1.4 & temp$RT<1.6), ]
  tempArea=dcast(data=temp,  formula=runName~signal, value.var="area")
  colnames(tempArea)=c("runName", "CH4area", "CO2area")
  tempRT=dcast(data=temp,  value.var="RT", formula=runName~signal)
  colnames(tempRT)=c("runName", "FID_RT", "TCD_RT")
  temp=merge(tempRT, tempArea, by="runName")
  
  #read in logfile
  tempLog=read.csv(paste0("../../logs/", logs[i]), stringsAsFactors = F)
  
  #merge log file with data 
  tempSum=merge(tempLog, temp, by="runName")
  
  #order by prefix 
  tempSum=tempSum[order(tempSum$prefix),]
  
  #make standard curves
  stds=tempSum[grep("standard", tempSum$subsampleClass),]
  stds$CH4ppm=standardsFile$CH4concentration_ppm[match(stds$subsampleClass, standardsFile$gcStdID)]
  stds$CO2ppm=standardsFile$CO2concentration_ppm[match(stds$subsampleClass, standardsFile$gcStdID)]
  
  #create the standard curve plots 
  pdf(paste0("~/OneDrive - nd.edu/underc-field-2019/gc2019underc/standardCurves/", runs[i], ".pdf"))
  par(mfrow=c(2,2))
  plot(stds$CH4area, stds$CH4ppm, main=paste("CH4 all", runs[i], sep=" - "), xlab="area", ylab="ppm")
  plot(stds$CH4area[stds$subsampleClass!="standard038"], stds$CH4ppm[stds$subsampleClass!="standard038"], main=paste("CH4 sub", runs[i], sep=" - "), xlab="area", ylab="ppm")
  plot(stds$CO2area, stds$CO2ppm, main=paste("CO2 all", runs[i], sep=" - "), xlab="area", ylab="ppm")
  plot(stds$CO2area[stds$subsampleClass!="standard038" & stds$subsampleClass!="standard037"], stds$CO2ppm[stds$subsampleClass!="standard038" & stds$subsampleClass!="standard037"], main=paste("CO2 sub", runs[i], sep=" - "), xlab="area", ylab="ppm")
  dev.off()
  
  #create standard curve fits
  CH4fitall=lm(stds$CH4ppm~stds$CH4area)
  CH4fitsub=lm(stds$CH4ppm[stds$subsampleClass!="standard038"]~stds$CH4area[stds$subsampleClass!="standard038"]+0)
  
  CO2fitall=lm(stds$CO2ppm~stds$CO2area)
  CO2fitsub=lm(stds$CO2ppm[stds$subsampleClass!="standard038"]~stds$CO2area[stds$subsampleClass!="standard038"]+0)
  
  #calculate ppm
  tempSum$CH4ppm=0
  tempSum$CO2ppm=0
  
  for(j in 1:nrow(tempSum)){
    # fill CH4 ppm 
    if(!(is.na(tempSum$CH4area[j]))){
      if(tempSum$CH4area[j]>500){
        tempSum$CH4ppm[j]=CH4fitall$coefficients[1]+CH4fitall$coefficients[2]*tempSum$CH4area[j]
      }else{
        tempSum$CH4ppm[j]=0+CH4fitsub$coefficients[1]*tempSum$CH4area[j]
      }
    }
    # fill CO2 ppm 
    if(!(is.na(tempSum$CO2area[j]))){
      if(tempSum$CO2area[j]>500){
        tempSum$CO2ppm[j]=CO2fitall$coefficients[1]+CO2fitall$coefficients[2]*tempSum$CO2area[j]
      }else{
        tempSum$CO2ppm[j]=0+CO2fitsub$coefficients[1]*tempSum$CO2area[j]
      }
    }
  }
  
  #calculate umol L-1
  ppm=tempSum
  ppm$CH4original_umolL=0
  ppm$CO2original_umolL=0
  
  # If pCO2
  # original water concentration from syringe equilibration
  P=1	#atm
  headV=0.01 #L
  liquidV=0.03 #L
  equilT=22 #degrees C
  CH4concMixingGas=0 #ppm	
  CO2concMixingGas=0 #ppm
  
  CH4bunsen=(2.7182818^(-67.1962+(99.1624*(100/(equilT+273.15)))+(27.9015*log((equilT+273.15)/100))))		#L L-1 atm-1
  CH4SourceGasConc=CH4concMixingGas/(0.0821*(equilT+273.15))						#umol L-1
  CH4FinalHeadSpaceConc=ppm$CH4ppm[as.character(ppm$subsampleClass)=="pCO2"]/(0.0821*(equilT+273.15))						#umol L-1
  CH4FinalLiquidConc=ppm$CH4ppm[as.character(ppm$subsampleClass)=="pCO2"]*CH4bunsen*P/(0.0821*(equilT+273.15))				#umol L-1
  CH4TotalSystemGas=(CH4FinalHeadSpaceConc*headV)+(CH4FinalLiquidConc*liquidV)	#umol
  CH4OriginalLiquidConc=(CH4TotalSystemGas-CH4SourceGasConc*headV)/liquidV		#umol L-1
  
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="pCO2"]=CH4OriginalLiquidConc
  
  CO2bunsen=(2.7182818^(-58.0931+(90.5069*(100/(equilT+273.15)))+(22.294*log((equilT+273.15)/100))))*((0.0821*273.15)+((-1636.75+(12.0408*273.15)-(3.27957*0.01*273.15*273.15)+(3.16528*0.00001*273.15*273.15*273.15))/1000))		#L L-1 atm-1
  CO2SourceGasConc=CO2concMixingGas/(0.0821*(equilT+273.15))						#umol L-1
  CO2FinalHeadSpaceConc=ppm$CO2ppm[as.character(ppm$subsampleClass)=="pCO2"]/(0.0821*(equilT+273.15))						#umol L-1
  CO2FinalLiquidConc=ppm$CO2ppm[as.character(ppm$subsampleClass)=="pCO2"]*CO2bunsen*P/(0.0821*(equilT+273.15))				#umol L-1
  CO2TotalSystemGas=(CO2FinalHeadSpaceConc*headV)+(CO2FinalLiquidConc*liquidV)	#umol
  CO2OriginalLiquidConc=(CO2TotalSystemGas-CO2SourceGasConc*headV)/liquidV		#umol L-1
  
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="pCO2"]=CO2OriginalLiquidConc
  
  
  # If DIC
  # Calculate original water concentration from syringe equil.
  P=1					#atm
  headV=0.03			#L
  liquidV=0.03 		#L
  equilT=22			#degrees C
  CH4concMixingGas=0	#ppm	
  CO2concMixingGas=0	#ppm
  
  CH4bunsen=(2.7182818^(-67.1962+(99.1624*(100/(equilT+273.15)))+(27.9015*log((equilT+273.15)/100))))		#L L-1 atm-1
  CH4SourceGasConc=CH4concMixingGas/(0.0821*(equilT+273.15))						#umol L-1
  CH4FinalHeadSpaceConc=ppm$CH4ppm[as.character(ppm$subsampleClass)=="DIC"]/(0.0821*(equilT+273.15))						#umol L-1
  CH4FinalLiquidConc=ppm$CH4ppm[as.character(ppm$subsampleClass)=="DIC"]*CH4bunsen*P/(0.0821*(equilT+273.15))				#umol L-1
  CH4TotalSystemGas=(CH4FinalHeadSpaceConc*headV)+(CH4FinalLiquidConc*liquidV)	#umol
  CH4OriginalLiquidConc=(CH4TotalSystemGas-CH4SourceGasConc*headV)/liquidV		#umol L-1
  
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="DIC"]=CH4OriginalLiquidConc
  
  CO2bunsen=(2.7182818^(-58.0931+(90.5069*(100/(equilT+273.15)))+(22.294*log((equilT+273.15)/100))))*((0.0821*273.15)+((-1636.75+(12.0408*273.15)-(3.27957*0.01*273.15*273.15)+(3.16528*0.00001*273.15*273.15*273.15))/1000))		#L L-1 atm-1
  CO2SourceGasConc=CO2concMixingGas/(0.0821*(equilT+273.15))						#umol L-1
  CO2FinalHeadSpaceConc=ppm$CO2ppm[as.character(ppm$subsampleClass)=="DIC"]/(0.0821*(equilT+273.15))						#umol L-1
  CO2FinalLiquidConc=ppm$CO2ppm[as.character(ppm$subsampleClass)=="DIC"]*CO2bunsen*P/(0.0821*(equilT+273.15))				#umol L-1
  CO2TotalSystemGas=(CO2FinalHeadSpaceConc*headV)+(CO2FinalLiquidConc*liquidV)	#umol
  CO2OriginalLiquidConc=(CO2TotalSystemGas-CO2SourceGasConc*headV)/liquidV		#umol L-1
  
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="DIC"]=CO2OriginalLiquidConc
  
  # If atm
  # is actual concentration, convert to umol L-1 
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="atm"]=ppm$CH4ppm[as.character(ppm$subsampleClass)=="atm"]/(0.0821*(equilT+273.15))
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="atm"]=ppm$CO2ppm[as.character(ppm$subsampleClass)=="atm"]/(0.0821*(equilT+273.15))
  
  # If flux
  # is actual concentration, convert to umol L-1 
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="flux"]=ppm$CH4ppm[as.character(ppm$subsampleClass)=="flux"]/(0.0821*(equilT+273.15))
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="flux"]=ppm$CO2ppm[as.character(ppm$subsampleClass)=="flux"]/(0.0821*(equilT+273.15))
  
  # If air 
  # is actual concentration, convert to umol L-1 
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="air"]=ppm$CH4ppm[as.character(ppm$subsampleClass)=="air"]/(0.0821*(equilT+273.15))
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="air"]=ppm$CO2ppm[as.character(ppm$subsampleClass)=="air"]/(0.0821*(equilT+273.15))
  
  # If production 
  # converts headspace concentration to umol L-1
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="methSlurry"]=ppm$CH4ppm[as.character(ppm$subsampleClass)=="methSlurry"]/(0.0821*(equilT+273.15))
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="methSlurry"]=ppm$CO2ppm[as.character(ppm$subsampleClass)=="methSlurry"]/(0.0821*(equilT+273.15))
  ppm$CH4original_umolL[as.character(ppm$subsampleClass)=="methSlurryPtreatment"]=ppm$CH4ppm[as.character(ppm$subsampleClass)=="methSlurryPtreatment"]/(0.0821*(equilT+273.15))
  ppm$CO2original_umolL[as.character(ppm$subsampleClass)=="methSlurryPtreatment"]=ppm$CO2ppm[as.character(ppm$subsampleClass)=="methSlurryPtreatment"]/(0.0821*(equilT+273.15))
  
  write.csv(file=paste0("~/OneDrive - nd.edu/underc-field-2019/gc2019underc/summaries/", runs[i], ".csv"), x=ppm, row.names = F)
  
}

