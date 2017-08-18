#Kriging interpolation for SICCS
#With this dataset runtime= 67 minutes

#REMOVE ALL OBJECTS
rm(list=ls(all=TRUE))
# system("rm /nobackup/users/dirksen/radiation/Rdata/Kriging/Daily/*")
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

pro=CRS("+init=epsg:28992")
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
savedir<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily_distsea/"
satellite_data_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = T)
satellite_file_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = F)

ked_output<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily_distsea//ked_exp_model_datum.rda"
ked_output_prediction<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily_distsea//ked_exp_prediction_datum.grd"
output_dir_kedexp<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily_distsea//ked_exp_pointdifference.txt"

output_dir_statistics_day<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/statistics_daily.txt"
#Time from the data
time<-gsub(".grd","",satellite_file_SICCS)
time<-as.Date(time,format="%Y%m%d")
year=as.numeric(format(time,"%Y"))
# time=as.numeric(format(time, "%Y%m%d"))

coordsKNMI_distshore<-readRDS("/nobackup/users/dirksen/radiation/Rdata/coordsRDknmi_distshore.rda")
distshore.grd<-read.asciigrid('/nobackup/users/dirksen/radiation/Rdata/wn_distshore_001.asc',colname="distshore")
crs(distshore.grd)<-pro
names(distshore.grd)<-"distance.sea"
distshore.grd<-as(distshore.grd,"SpatialGridDataFrame")

R2<-function(obs,pred){cor(obs,pred)^2}
RMSE<-function(obs,pred){sqrt(mean((obs-pred)^2))}

system.time(
for (i in 1:length(time))({
  Y=year[i]
  SICCS_loc_new<-satellite_data_SICCS[i]
  SICCS_datum<-time[i]
  t=time[i]
  
  # distshore.grd<-raster(SICCS_loc_new,colname="distshore")
  # distshore.grd<-as(distshore.grd,"SpatialGridDataFrame")
  
  inputdata = inputdata_all[inputdata_all$date==SICCS_datum,] #The datum of the inputdata equals the RACMO_datum
  inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
  inputdata<-merge(inputdata,coordsKNMI_distshore,by=c("DS_CODE","x","y"))
  #This is to avoid "Factor" problems with the .txt file
  
  coordinates(inputdata) = ~x+y #the coordinates of inputdata, compare to headers
  inputdata <- inputdata[!is.na(inputdata$Q),] #removing nan data from the temperature
  # inputdata<-SpatialPointsDataFrame(inputdata)
  # crs(inputdata)<-crs(distshore.grd)
  # 
  # #Cut NA values from this grid, and make sure the cellsize stays square.
  # gridded(distshore.grd)=FALSE;gridded(distshore.grd)=TRUE;fullgrid(distshore.grd) = TRUE
  # slot(slot(distshore.grd, "grid"), "cellsize") <-rep(mean(slot(slot(distshore.grd, "grid"), "cellsize")), 2)
  ###############################################################################################
  ############HERE THE INTERPOLATION STARTS######################################################
  ###############################################################################################
   ### Universal kriging (KED) - variogrammodel EXP ###
  
  # Kriging
  
  # ked_exp <- autoKrige(Q~distance.sea, inputdata, distshore.grd,
  #                      maxdist=mxdkrige, block=c(blocksize,blocksize),
  #                      model = c("Exp"), na.action=na.pass, fix.values=c(NA,NA,NA),
  #                      miscFitOptions = list(merge.small.bins = TRUE))  #log(distshore)
  # ked_prediction<-ked_exp$krige_output
  # ked_prediction<-brick(ked_prediction)
  # Krige Cross validation
  ked_exp.cv <- autoKrige.cv(Q~SAT, inputdata, model = c("Exp"),maxdist=mxdkrige,fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = TRUE))
  ked_exp.cv.distsea <- autoKrige.cv(Q~distance.sea, inputdata, model = c("Exp"),maxdist=mxdkrige,fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = TRUE))
  
  r2.sat<-R2(inputdata$Q,inputdata$SAT)
  rmse.sat<-RMSE(inputdata$Q,inputdata$SAT)
  
  r2.kedsat<-R2(ked_exp.cv$krige.cv_output$observed,ked_exp.cv$krige.cv_output$var1.pred)
  rmse.kedsat<-RMSE(ked_exp.cv$krige.cv_output$observed,ked_exp.cv$krige.cv_output$var1.pred)
  
  r2.kedsea<-R2(ked_exp.cv.distsea$krige.cv_output$observed,ked_exp.cv.distsea$krige.cv_output$var1.pred)
  rmse.kedsea<-RMSE(ked_exp.cv.distsea$krige.cv_output$observed,ked_exp.cv.distsea$krige.cv_output$var1.pred)
  
  out.sum<-data.frame(t,r2.sat,rmse.sat,r2.kedsat,rmse.kedsat,r2.kedsea,rmse.kedsea)
  
  #OLD CODE
  # teller<-((ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)^2)
  # noemer<-((ked_exp.cv$krige.cv_output$observed-mean(ked_exp.cv$krige.cv_output$observed))^2)
  # 
  # #########CHANGE:
  # #The R2 values like this:
  # # R2 <- 1 - (sum((ked$observed-ked$var1.pred)^2)/sum((ked$observed-mean(ked$observed))^2)) 
  # ked_exp.cv$krige.cv_output$r2<-1-teller/noemer
  # ked_exp.cv$krige.cv_output$rmse<-sqrt((ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)^2)
  # ked_exp.cv$krige.cv_output$rmse_sd<-ked_exp.cv$krige.cv_output$rmse/sqrt(noemer)
  # ked_exp.cv$krige.cv_output$me<-(ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)/mean(ked_exp.cv$krige.cv_output$observed)
  # ked_exp.cv$krige.cv_output$date<-t
  # ked_exp.cv$krige.cv_output$SAT<-inputdata$SAT
  # 
  # #coefficients for the satellite data
  # ked_exp.cv$krige.cv_output$r2.sat<-1-((ked_exp.cv$krige.cv_output$observed-ked_exp.cv$krige.cv_output$SAT)/(ked_exp.cv$krige.cv_output$observed-mean(ked_exp.cv$krige.cv_output$observed)))
  # ked_exp.cv$krige.cv_output$rmse.sat<-sqrt((ked_exp.cv$krige.cv_output$SAT-ked_exp.cv$krige.cv_output$observed)^2)
  # ked_exp.cv$krige.cv_output$rmse_sd.sat<-ked_exp.cv$krige.cv_output$rmse.sat/sqrt(ked_exp.cv$krige.cv_output$observed-mean(ked_exp.cv$krige.cv_output$observed))
  # ked_exp.cv$krige.cv_output$me.sat<-(ked_exp.cv$krige.cv_output$SAT-ked_exp.cv$krige.cv_output$observed)/mean(ked_exp.cv$krige.cv_output$observed)
  # teller <- sum(ked_exp.cv$krige.cv_output$residual^2)
  # noemer <- sum((var$Q-mean(var$Q))^2)
  # ked_exp.r2 <- 1 - teller/noemer
  # ked.zscoremean <- mean(ked_exp.cv$krige.cv_output$zscore)
  # ked.zscore.var <- var(ked_exp.cv$krige.cv_output$zscore)
  # 
  # # FORCE GRID CELLS TO BE SQUARE
  # ked_exp=ked_exp$krige_output
  # slot(slot(ked_exp, "grid"), "cellsize") <- rep(mean(slot(slot(ked_exp, "grid"), "cellsize")), 2)
  
  # Calculate differences at observation points
  # predicted= over (var,ked_exp)
  # var$predicted = predicted$var1.pred
  # var$difference = (var$Q - var$predicted)
  # difmin = min (var$difference,na.rm=TRUE)
  # difmax = max (var$difference,na.rm=TRUE)
  # difmean = mean (var$difference,na.rm=TRUE)
  # difsd = sd (var$difference,na.rm=TRUE)
  # output <- data.frame(var$Stn,var$Locatie,var$RDN_X,var$RDN_Y,var$predicted,var$difference)
  # colnames(output)<-c("Stn","Locatie","RDN_X","RDN_Y","predicted","difference")
  
  #save functions
  # if(i==1){
  # # output_dir_kedexp_new<- gsub( "datum",SICCS_datum,output_dir_kedexp ) 
  # write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=TRUE,sep=",")
  # } else {
  #   write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=FALSE,sep=",",append=TRUE)
  # }
  
  if(i==1){
  # output_dir_kedexp_new<- gsub( "datum",SICCS_datum,output_dir_kedexp )
  write.table(out.sum, output_dir_statistics_day, row.names=FALSE, col.names=TRUE,sep=",")
  } else {
    write.table(out.sum, output_dir_statistics_day, row.names=FALSE, col.names=FALSE,sep=",",append=TRUE)
  }
  
  ### KED output
  # ked_output_new<-gsub("datum",SICCS_datum,ked_output)
  # saveRDS(ked_exp,ked_output_new)
  # 
  # ked_output_prediction_new<-gsub("datum",SICCS_datum,ked_output_prediction)
  # writeRaster(ked_prediction,ked_output_prediction_new,overwrite=T)
  
})  
)
 