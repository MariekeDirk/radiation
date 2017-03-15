#Kriging interpolation for SICCS
#With this dataset runtime= 67 minutes

#REMOVE ALL OBJECTS
rm(list=ls(all=TRUE))
system("rm /nobackup/users/dirksen/radiation/Rdata/Kriging/Daily/*")
#LOAD PACKAGES
library(methods)
library(sp)
library(gstat)
library(automap)
library(grid)
library(maps)
library(spam)
library(fields)
library(raster)
library(data.table)

source ("/nobackup/users/dirksen/Temperature/Temperature/Data/KNMIstations/crossvalidate.r")
inputdata_all<-fread("/nobackup/users/dirksen/radiation/Rdata/sat_over_obs2.csv")
names(inputdata_all) <- c("date","DS_CODE","Q","x","y","SAT","DIFF")
inputdata_all$date <- as.Date(inputdata_all$date)
setkey(inputdata_all,date)

#settings kriging
blocksize = 40000
mxdkrige=Inf # maxdist Krige
#Data and directories
##########################################
savedir<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/"
satellite_data_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/",pattern=".rds",full.names = T)
# satellite_file_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = F)

ked_output<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_model_datum.rda"
ked_output_prediction<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_prediction_datum.grd"
output_dir_kedexp<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_pointdifference.txt"

#inputclimatology from observations
# library(xts)
library(rts)

yq<-as.yearqtr(inputdata_all$date)
indices<-format(yq,format="%q")
inputdata_all$quarters<-indices

#(1) Quarters
in.aggregate.fun<-subset(inputdata_all,select=c("Q","SAT","DIFF"))
mean.quarter <- aggregate(in.aggregate.fun,list(inputdata_all$quarters,inputdata_all$DS_CODE),mean)
sd.quarter <- aggregate(in.aggregate.fun,list(inputdata_all$quarters,inputdata_all$DS_CODE),sd)

names(mean.quarter)<-c("quarters","DS_CODE","Q","SAT","DIFF")
names(sd.quarter)<-c("quarters","DS_CODE","Q","SAT","DIFF")



system.time(
for (i in 1:4)({
  Y=year[i]
  SICCS_loc_new<-satellite_data_SICCS[i]
  SICCS_datum<-time[i]
  t=time[i]
  distshore.grd<-raster(SICCS_loc_new,colname="distshore")
  distshore.grd<-as(distshore.grd,"SpatialGridDataFrame")
  
  inputdata = inputdata_all[inputdata_all$date==SICCS_datum,] #The datum of the inputdata equals the RACMO_datum
  inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
  #This is to avoid "Factor" problems with the .txt file
  
  coordinates(inputdata) = ~x+y #the coordinates of inputdata, compare to headers
  inputdata <- inputdata[!is.na(inputdata$Q),] #removing nan data from the temperature
  # inputdata<-SpatialPointsDataFrame(inputdata)
  crs(inputdata)<-crs(distshore.grd)
  
  #Cut NA values from this grid, and make sure the cellsize stays square.
  gridded(distshore.grd)=FALSE;gridded(distshore.grd)=TRUE;fullgrid(distshore.grd) = TRUE
  slot(slot(distshore.grd, "grid"), "cellsize") <-rep(mean(slot(slot(distshore.grd, "grid"), "cellsize")), 2)
  ###############################################################################################
  ############HERE THE INTERPOLATION STARTS######################################################
  ###############################################################################################
   ### Universal kriging (KED) - variogrammodel EXP ###
  
  # Kriging
  
  ked_exp <- autoKrige(Q~SAT, inputdata, distshore.grd, 
                       maxdist=mxdkrige, block=c(blocksize,blocksize), 
                       model = c("Exp"), na.action=na.pass, fix.values=c(NA,NA,NA), 
                       miscFitOptions = list(merge.small.bins = TRUE))  #log(distshore)
  ked_prediction<-ked_exp$krige_output
  ked_prediction<-brick(ked_prediction)
  # Krige Cross validation
  ked_exp.cv <- autoKrige.cv(Q~SAT, inputdata, model = c("Exp"),maxdist=mxdkrige,fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = TRUE))
  teller<-(ked_exp.cv$krige.cv_output$residual^2)
  noemer<-((ked_exp.cv$krige.cv_output$observed-mean(ked_exp.cv$krige.cv_output$observed))^2)
  ked_exp.cv$krige.cv_output$r2<-1-teller/noemer
  ked_exp.cv$krige.cv_output$rmse<-sqrt((ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)^2)
  ked_exp.cv$krige.cv_output$rmse_sd<-ked_exp.cv$krige.cv_output$rmse/sqrt(noemer)
  ked_exp.cv$krige.cv_output$me<-(ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)/mean(ked_exp.cv$krige.cv_output$observed)
  ked_exp.cv$krige.cv_output$date<-t
  ked_exp.cv$krige.cv_output$SAT<-inputdata$SAT
  #save functions
  if(i==1){
  write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=TRUE,sep=",")
  } else {
    write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=FALSE,sep=",",append=TRUE)
  }
  ### KED output
  ked_output_new<-gsub("datum",SICCS_datum,ked_output)
  saveRDS(ked_exp,ked_output_new)
  
  ked_output_prediction_new<-gsub("datum",SICCS_datum,ked_output_prediction)
  writeRaster(ked_prediction,ked_output_prediction_new,overwrite=T)
  
})  
)
 