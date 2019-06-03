##GC_CH4_raw2summaries.R
#BBertolet 2018-05-29
#Adapted from JColoso
#Function to create a summary file from raw GC data files.  If the GC summary doesn't work, this will create a summary.  Give function the full path to the folder with your GC data.  Output is a dataframe with columns: sampleID, RT, area, height
GCraw2sum<-function(folderName){
  #find subfolder
  subdir=paste(folderName,list.files(folderName), sep="/")
  #find folders to loop through
  subfolders=dir(subdir)[grep("[0-9]{1,3}.D",dir(subdir))]
  #make run name
  runDate=gsub("UNDERC-2018/gc/data/","", folderName)
  
  #loop through each subfolder, open report file, pull out data, then add data to data frame
  sumdata<-data.frame() #create empty dataframe
  
  for (i in 1:length(subfolders)){
    print(paste0("     Sample:", subfolders[i]))
    filei<-paste(subdir,subfolders[i],'New2.txt',sep="/") #file to open from subfolder i
    #read filei and get FID and TCD signals
    if (file.exists(filei)){ 
      rawdata.temp<-read.table(filei,header=FALSE,sep="\\",blank.lines.skip=F) #read filei
      headlength<-grep('----',rawdata.temp[,1])
      
      #check to make sure there are peaks
      if(length(headlength)>0){
          rawdata<-read.table(filei,header=F,sep="\\",blank.lines.skip=F,skip=headlength[1])
          rawdata<-rawdata[1:(nrow(rawdata)-4),]
          
          #make data frame
          data.fix=data.frame()
          
          #read each line one at a time and pull out data
          for (j in 1:length(rawdata)){
            rowi<-strsplit(as.character(rawdata[j]),split=" ")
            goodi<-which(rowi[[1]]!="" & rowi[[1]]!="S" & rowi[[1]]!="T" & rowi[[1]]!="X") #find non blanks
            datai<-t(as.data.frame(as.character(rowi[[1]][goodi])))
            data.fix<-rbind(data.fix,datai)
          }
          #remove some columns and convert to correct data type
          row.names(data.fix)<-NULL #remove row names
          data.fix<-data.fix[,c(-1,-4,-7)] #remove unnecessary columns
          names(data.fix)<-c('RT','sig','area','height')
          data.fix$runName<-rep(gsub(".D", paste("_",runDate, sep=""), subfolders[i]),nrow(data.fix)) #add sample name
          data.fix=data.fix[c('runName','sig','RT','area','height')] #reorder columns
          
          #add data from file to summary data frame
          sumdata<-rbind(sumdata,data.fix)  
        }
    } #end file loop    
  } #end subfolder loop
  
  if (length(sumdata)>0){
    output<-c()
    output$runName<-as.character(sumdata[,1])
    output$signal<-as.character(sumdata[,2])
    output$RT<-as.numeric(levels(sumdata[,3])[as.integer(sumdata[,3])])
    output$area<-as.numeric(levels(sumdata[,4])[as.integer(sumdata[,4])])
    output$height<-as.numeric(levels(sumdata[,5])[as.integer(sumdata[,5])])
    output<-as.data.frame(output)
    return(output)
  }
} #end function
