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
coordsRD<-readRDS("/nobackup/users/dirksen/radiation/Rdata/coordsRDknmi.rda")
inputdata_all<-fread("/nobackup/users/dirksen/radiation/Rdata/sat_over_obs2.csv")
names(inputdata_all) <- c("date","DS_CODE","Q","x","y","SAT","DIFF")
inputdata_all$date <- as.Date(inputdata_all$date)
setkey(inputdata_all,date)

R2<-function(obs,pred){cor(obs,pred)^2}
RMSE<-function(obs,pred){sqrt(mean((obs-pred)^2))}
#settings kriging
blocksize = 40000
mxdkrige=Inf # maxdist Krige
#Data and directories
##########################################
savedir<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/"
# satellite_data_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/",pattern=".rds",full.names = T)
# satellite_file_SICCS<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = F)
#example to calculate mean for whole period of st: st.mean<-stackApply(st,indices=1,fun=sd,na.rm=TRUE)

ked_output<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_model_datum.rda"
ked_output_prediction<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_prediction_datum.grd"
output_dir_kedexp<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/ked_exp_pointdifference.txt"

output_dir_statistics_quarters<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/statistics_quarters.txt"
output_dir_statistics_12year<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/statistics_12year.txt"

# distshore.grd<-read.asciigrid('/nobackup/users/dirksen/radiation/Rdata/wn_distshore_001.asc',colname="distshore")
# crs(distshore.grd)<-pro
coordsKNMI_distshore<-readRDS("/nobackup/users/dirksen/radiation/Rdata/coordsRDknmi_distshore.rda")


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

mean.quarter<-merge(mean.quarter,coordsRD,by="DS_CODE")
sd.quarter<-merge(sd.quarter,coordsRD,by="DS_CODE")

r.quarters.satellite.mean<-"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/quarters_mean.rds"
r.quarters.satellite.mean<-readRDS(r.quarters.satellite.mean)

#(2) whole period
mean.12year<-aggregate(in.aggregate.fun,list(inputdata_all$DS_CODE),mean)
sd.12year<-aggregate(in.aggregate.fun,list(inputdata_all$DS_CODE),sd)

names(mean.12year)<-c("DS_CODE","Q","SAT","DIFF")
names(sd.12year)<-c("DS_CODE","Q","SAT","DIFF")

mean.12year<-merge(mean.12year,coordsRD,by="DS_CODE")
sd.12year<-merge(sd.12year,coordsRD,by="DS_CODE")

#mean 
distshore.grd<-raster("/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/12year_siccs/siccs_12year_mean.grd")

#sd
distshore.grd<-raster("/nobackup/users/dirksen/radiation/Rdata/Kriging/Climatology/12year_siccs/siccs_12year_sd.grd")

# saveRDS(mean.quarter,"/nobackup/users/dirksen/radiation/Rdata/mean_quarter_obs.rds")
# saveRDS(sd.quarter,"/nobackup/users/dirksen/radiation/Rdata/sd_quarter_obs.rds")
# distshore.grd<-readRDS(satellite_data_SICCS[2])

system.time(
for (i in 1:4)({
  t=i
  # distshore.grd<-r.quarters.satellite.mean[[i]]
  # names(distshore.grd)<-"SAT"
  # distshore.grd<-as(distshore.grd,"SpatialGridDataFrame")
  # inputdata = mean.quarter[mean.quarter$quarters==i,] #The datum of the inputdata equals the RACMO_datum
  inputdata=mean.12year
  
  inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
  #This is to avoid "Factor" problems with the .txt file
  inputdata<-merge(inputdata,coordsKNMI_distshore,by=c("DS_CODE","x","y"))
  coordinates(inputdata) = ~x+y #the coordinates of inputdata, compare to headers
  inputdata <- inputdata[!is.na(inputdata$Q),] #removing nan data from the temperature
  
  # inputdata<-SpatialPointsDataFrame(inputdata)
  # crs(inputdata)<-crs(distshore.grd)
  
  #Cut NA values from this grid, and make sure the cellsize stays square.
  # gridded(distshore.grd)=FALSE;gridded(distshore.grd)=TRUE;fullgrid(distshore.grd) = TRUE
  # slot(slot(distshore.grd, "grid"), "cellsize") <-rep(mean(slot(slot(distshore.grd, "grid"), "cellsize")), 2)
  ###############################################################################################
  ############HERE THE INTERPOLATION STARTS######################################################
  ###############################################################################################
   ### Universal kriging (KED) - variogrammodel EXP ###
  
  # Kriging
  # 
  # ked_exp <- autoKrige(Q~SAT, inputdata, distshore.grd, 
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
  #OLD
  # teller<-(ked_exp.cv$krige.cv_output$residual^2)
  # noemer<-((ked_exp.cv$krige.cv_output$observed-mean(ked_exp.cv$krige.cv_output$observed))^2)
  # ked_exp.cv$krige.cv_output$r2<-1-teller/noemer
  # ked_exp.cv$krige.cv_output$rmse<-sqrt((ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)^2)
  # ked_exp.cv$krige.cv_output$rmse_sd<-ked_exp.cv$krige.cv_output$rmse/sqrt(noemer)
  # ked_exp.cv$krige.cv_output$me<-(ked_exp.cv$krige.cv_output$var1.pred-ked_exp.cv$krige.cv_output$observed)/mean(ked_exp.cv$krige.cv_output$observed)
  # ked_exp.cv$krige.cv_output$date<-t
  # ked_exp.cv$krige.cv_output$quarter<-i
  # ked_exp.cv$krige.cv_output$SAT<-inputdata$SAT
  #save functions
  # if(i==1){
  # write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=TRUE,sep=",")
  # } else {
  #   write.table(data.frame(ked_exp.cv$krige.cv_output), output_dir_kedexp, row.names=FALSE, col.names=FALSE,sep=",",append=TRUE)
  # }
  ### KED output
  # ked_output_new<-gsub("datum","siccs",ked_output)
  # # ked_output_new<-gsub("datum",i,ked_output)
  # saveRDS(ked_exp,ked_output_new)
  # 
  # ked_output_prediction_new<-gsub("datum","siccs",ked_output_prediction)
  # # ked_output_prediction_new<-gsub("datum",i,ked_output_prediction)
  # writeRaster(ked_prediction,ked_output_prediction_new,overwrite=T)
  if(i==1){
    write.table(out.sum, output_dir_statistics_12year, row.names=FALSE, col.names=TRUE,sep=",")
  } else {
    write.table(out.sum, output_dir_statistics_quarters, row.names=FALSE, col.names=FALSE,sep=",",append=TRUE)
  }
})  
)

# coordsKNMI<-readRDS("/nobackup/users/dirksen/radiation/Rdata/coordsRDknmi.rda")
# coordinates(coordsKNMI)<-~x+y
# 
# distances.sea<-over(coordsKNMI,distshore.grd)
# coordsKNMI<-data.frame(coordsKNMI)
# coordsKNMI$distance.sea<-distances.sea$distshore
# 
# coordsKNMI<-subset(coordsKNMI,select=c("x","y","DS_CODE","distance.sea"))
# saveRDS(coordsKNMI,"/nobackup/users/dirksen/radiation/Rdata/coordsRDknmi_distshore.rda")
#inputclimatology from observations
# library(xts)
 