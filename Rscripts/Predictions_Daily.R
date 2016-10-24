
library(adehabitat)
library(automap)
library(caret)
library(caretEnsemble)
library(data.table)
library(doParallel)
library(foreach)
library(GSIF)
library(kernlab)
library(maptools)
library(reshape)
library(raster)
library(rgdal)
library(rhdf5)
library(SDMTools)

#Time period
startdate<-as.POSIXct("2004-01-19")
stopdate<-as.POSIXct("2016-07-31")
dates<-seq(from=startdate,to=stopdate,by="day")
#settings
pro=CRS("+init=epsg:28992")
mxdkrige=Inf # maxdist Krige

#paths
pathsat<-"/net/pc150398/nobackup_1/users/meirink/siccs_wouter/SICCS/daymean/"

#Satellite grid
loc.grid<-"Rdata/meteosat9.euro4m.angles.h5"
h5ls(loc.grid)
data.lat<-h5read(loc.grid,"/lat") #Latitude
data.lon<-h5read(loc.grid,"/lon") #Longitude

data.lat[which(data.lat==-999)]<-NA # replace no data value with NA (this case -999)
data.lon[which(data.lon==-999)]<-NA # replace no data value with NA (this case -999)

#observations
obs<-fread("Rdata/radiation_KNMI_day.csv")
obs$IT_DATETIME<-as.POSIXct(obs$IT_DATETIME,format="%Y%m%d_%H%M%S_000000")

#outputdirs
dir_differences<-"/nobackup/users/dirksen/Radiation_Obs_Satellite/output/differences/"
dir_model<-"/nobackup/users/dirksen/Radiation_Obs_Satellite/output/models/"
dir_st<-"/nobackup/users/dirksen/Radiation_Obs_Satellite/output/predictions/"
summary_differences<-"/nobackup/users/dirksen/Radiation_Obs_Satellite/output/summary_differences/"

for (i in 1:length(dates)) {
time<-as.Date(dates[i])
Y<-year(time)

m<-month(time)
if (m<=9) {
  m<-paste0(0,m)
  } else {print(m)}

d<-mday(time)
if (d<=9) {
  d<-paste0(0,d)
} else {print(d)}

path.sat<-paste0(pathsat,Y,"/",m)
#selecting a test month
files.sat<-list.files(path=paste0(pathsat,Y,"/",m))

d.sat<-gsub("daymean","",files.sat)
d.sat<-gsub("EURO4M.hdf5","",d.sat)
I=which(d.sat==d)
if (length(I)==0){
  print("NO FILE")
  # out<-data.frame(time,"no file")
  # names(out)<-NULL
  # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
  #             append=TRUE,sep=",",row.names=FALSE)
} else{
file<-files.sat[I]
file_and_loc<-paste0(path.sat,"/",file)

h5ls(file_and_loc)
data.direct.irradiance<-h5read(file_and_loc,"direct irradiance")
data.diffuse.irradiance<-h5read(file_and_loc,"diffuse irradiance")
data.total.irradiance<-data.direct.irradiance+data.diffuse.irradiance
data.total.irradiance<-t(data.total.irradiance)
######################################
## Create grid
######################################
r<-raster(data.total.irradiance,crs=CRS("+init=epsg:4326"),
          xmn=min(data.lon,na.rm=T),
          xmx=max(data.lon,na.rm=T),
          ymn=min(data.lat,na.rm=T),
          ymx=max(data.lat,na.rm=T))
# Reproject and crop
r<-projectRaster(r,crs=pro)
r.NED<-crop(r,extent(12621.630033977,278621.630033977,305583.0457758,620583.0457758))
r.NED.matrix<-as.matrix(r.NED)
if (length(which(is.na(r.NED.matrix)))>2000) {
  print("NO data")
  # out<-data.frame(time,"no data NL")
  # names(out)<-NULL
  # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
  #             append=TRUE,sep=",",row.names=FALSE)
} else {
  
######################################
#Observation data
######################################


obs.subset<-obs[which(IT_DATETIME==dates[i]),]
obs.subset<-na.omit(obs.subset,cols=c("DS_LAT","DS_LON","REH1.Q24"))

coordinates(obs.subset)<-~DS_LON+DS_LAT
proj4string(obs.subset)<-CRS("+init=epsg:4326")
obs.subset<-spTransform(obs.subset,pro)
obs.subset$REH1.Q<-obs.subset$REH1.Q24/24

######################################
# Geostatistical analysis
######################################
rASC<-asc.from.raster(r.NED)
spdf<-asc2spixdf(rASC)
proj4string(spdf)<-pro
# Kriging

# over functions
gridded(spdf)=FALSE;gridded(spdf)=TRUE;fullgrid(spdf) = TRUE
slot(slot(spdf, "grid"), "cellsize") <-rep(mean(slot(slot(spdf, "grid"), "cellsize")), 2)
# over Distshore on Var
distshore.ov=over(obs.subset,spdf)
# Copy the values to Var )
var = obs.subset
var$var=distshore.ov$var

#Prepare input  
field = spdf
field@data = cbind(field@data, coordinates(field))
names(field@data) = c("s","x","y")
var$x = over(var,field)$x 
var$y = over(var,field)$y
var$s = over(var,field)$s

# Remove nodata from dataframe based on missing distshore
var = var[!is.na(var$var),]


ked_exp <- autoKrige(REH1.Q~var, var, spdf,maxdist=mxdkrige, model = c("Exp"), na.action=na.pass, fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = FALSE))  #log(distshore)

# Krige Cross validation
ked_exp.cv <- autoKrige.cv(REH1.Q~var, var, model = c("Exp"),maxdist=mxdkrige,fix.values=c(NA,NA,NA), miscFitOptions = list(merge.small.bins = FALSE),verbose=c(FALSE,FALSE))
teller <- sum(ked_exp.cv$krige.cv_output$residual^2)
ked_exp.r2 <- r2(ked_exp.cv$krige.cv_output$residual,var$var)
ked.zscoremean <- mean(ked_exp.cv$krige.cv_output$zscore)
ked.zscore.var <- var(ked_exp.cv$krige.cv_output$zscore)
ked_exp.rmse<-rmse(ked_exp.cv$krige.cv_output$var1.pred,ked_exp.cv$krige.cv_output$observed)
######################################
#Machine Learning
######################################

#Prepare input
obs.subset.Q<-subset(obs.subset,select="REH1.Q")
grid<-as(r.NED,"SpatialPixelsDataFrame")

ov<-over(obs.subset.Q,grid)
ov<-cbind(data.frame(obs.subset.Q["REH1.Q"]),ov)
ov<-rename(ov,c("DS_LON"="x"))
ov<-rename(ov,c("DS_LAT"="y"))

#Set control and tuneGrid
control<-trainControl(method="cv",number=10,repeats=3) #setting a 10 fold-cross validation (best performing)
length<-10 #for the tuneLength of the models

#uncertainty measurements (calibration): 10W/m2, uncertainty sat: 30-50W/m2. 
sigmaRangeReduced<-sigest(as.matrix(ov$layer))[1]
svmRadialRGridReduced<-expand.grid(.sigma=sigmaRangeReduced,.C=2^(seq(-4,4)))
svmLinearRGridReduced<-expand.grid(C=2^(seq(-4,4)))
ctreeGridReduced<-expand.grid(mincriterion=seq(from=0.1,to=0.5,by=0.1))
knnGridReduced<-expand.grid(.k=3:15)
gbmGridReduced<-expand.grid(.shrinkage=c(0.1),.n.trees=1:5,.interaction.depth=1:6,.n.minobsinnode=1:5)
earthGridReduced <- data.frame(.degree = 1, .nprune = (2:4)*2)

#Models
set.seed(50)
m1.lm<-caret::train(REH1.Q~layer,data=ov,method="lm",preProcess=c("center","scale","BoxCox"),tuneLength=length,trControl=control)

set.seed(50)
m2.glm<-caret::train(REH1.Q~layer,data=ov,method="glm",preProcess=c("center","scale","BoxCox"),tuneLength=length,trControl=control)

set.seed(50)
m3.gaussprLinear<-caret::train(REH1.Q~layer,data=ov,method="gaussprLinear",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control)

#Support Vector Models
set.seed(50)
m4.svmRadial<-caret::train(REH1.Q~layer,data=ov,method="svmRadial",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control,tuneGrid=svmRadialRGridReduced)

set.seed(50)
m5.svmLinear<-caret::train(REH1.Q~layer,data=ov,method="svmLinear",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control,tuneGrid=svmLinearRGridReduced)

#Tree models
set.seed(50)
m6.treebag<-caret::train(REH1.Q~layer,data=ov,method="treebag",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control)

set.seed(50)
m7.cubist<-caret::train(REH1.Q~layer,data=ov,method="cubist",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control)

set.seed(50)
m8.ctree<-caret::train(REH1.Q~layer,data=ov,method="ctree",preProcess=c("center","scale","BoxCox"),tuneLength=length,trControl=control,tuneGrid=ctreeGridReduced)

#K-nearest neighbors
set.seed(50)
m9.knn<-caret::train(REH1.Q~layer,data=ov,method="knn",preProcess=c("center","scale","BoxCox"),verbose=FALSE,tuneLength=length,trControl=control,tuneGrid=knnGridReduced)

# Others
set.seed(50)
m10.earth<-caret::train(REH1.Q~layer,data=ov,method="earth",preProcess=c("center","scale","BoxCox"),tuneLength=length,trControl=control,tuneGrid=earthGridReduced)

set.seed(50)
m11.gbm<-caret::train(REH1.Q~layer,data=ov,method="gbm",preProcess=c("center","scale","BoxCox"),tuneLength=length,trControl=control,tuneGrid=gbmGridReduced,verbose=FALSE)

models<-list(lm=m1.lm,
             glm=m2.glm,
             gaussprLinear=m3.gaussprLinear,
             svmRadial=m4.svmRadial,
             svmLinear=m5.svmLinear,
             treebag=m6.treebag,
             cubist=m7.cubist,
             ctree=m8.ctree,
             knn=m9.knn,
             earth=m10.earth,
             gbm=m11.gbm,
             kriging=list(var_model=ked_exp$var_model,
                           exp_var=ked_exp$exp_var,
                           R2=ked_exp.r2,
                           sserr=ked_exp$sserr))


lm.out<-getTrainPerf(m1.lm);lm.rmse<-lm.out$TrainRMSE;lm.r2<-lm.out$TrainRsquared
glm.out<-getTrainPerf(m2.glm);glm.rmse<-glm.out$TrainRMSE;glm.r2<-glm.out$TrainRsquared
gaussprLinear.out<-getTrainPerf(m3.gaussprLinear);gaussprLinear.rmse<-gaussprLinear.out$TrainRMSE;gaussprLinear.r2<-gaussprLinear.out$TrainRsquared
svmRadial.out<-getTrainPerf(m4.svmRadial);svmRadial.rmse<-svmRadial.out$TrainRMSE;svmRadial.r2<-svmRadial.out$TrainRsquared
svmLinear.out<-getTrainPerf(m5.svmLinear);svmLinear.rmse<-svmLinear.out$TrainRMSE;svmLinear.r2<-svmLinear.out$TrainRsquared
treebag.out<-getTrainPerf(m6.treebag);treebag.rmse<-treebag.out$TrainRMSE;treebag.r2<-treebag.out$TrainRsquared
cubist.out<-getTrainPerf(m7.cubist);cubist.rmse<-cubist.out$TrainRMSE;cubist.r2<-cubist.out$TrainRsquared
ctree.out<-getTrainPerf(m8.ctree);ctree.rmse<-ctree.out$TrainRMSE;ctree.r2<-ctree.out$TrainRsquared
knn.out<-getTrainPerf(m9.knn);knn.rmse<-knn.out$TrainRMSE;knn.r2<-knn.out$TrainRsquared
earth.out<-getTrainPerf(m10.earth);earth.rmse<-earth.out$TrainRMSE;earth.r2<-earth.out$TrainRsquared
gbm.out<-getTrainPerf(m11.gbm);gbm.rmse<-gbm.out$TrainRMSE;gbm.r2<-gbm.out$TrainRsquared
#Predictions
st<-stack(grid)
st$lm<-raster::predict(model=m1.lm,object=st)
st$glm<-raster::predict(model=m2.glm,object=st)
st$gaussprLinear<-raster::predict(model=m3.gaussprLinear,object=st)
st$svmRadial<-raster::predict(model=m4.svmRadial,object=st)
st$svmLinear<-raster::predict(model=m5.svmLinear,object=st)
st$treebag<-raster::predict(model=m6.treebag,object=st)
st$cubist<-raster::predict(model=m7.cubist,object=st)
st$ctree<-raster::predict(model=m8.ctree,object=st)
st$knn<-raster::predict(model=m9.knn,object=st)
st$earth<-raster::predict(model=m10.earth,object=st)
st$gbm<-raster::predict(model=m11.gbm,object=st)
st$kriging<-ked_exp$krige_output$var1.pred
print(st)

#Compare with predictions
var.Q<-subset(obs.subset,select=REH1.Q)
I.Q<-which(is.na(var.Q$REH1.Q))
var.name<-subset(obs.subset,select=c(DS_NAME,DS_CODE))
if (length(I.Q)>0){
var.name<-var.name[-I.Q,]
} else print("No missing obs")
SICCS<-RasterOverPoints(r.NED,var.Q,var.Q$REH1.Q,pro)


lm<-RasterOverPoints(st$lm,var.Q,var.Q$REH1.Q,pro)
glm<-RasterOverPoints(st$glm,var.Q,var.Q$REH1.Q,pro)
gaussprLinear<-RasterOverPoints(st$gaussprLinear,var.Q,var.Q$REH1.Q,pro)
svmRadial<-RasterOverPoints(st$svmRadial,var.Q,var.Q$REH1.Q,pro)
svmLinear<-RasterOverPoints(st$svmLinear,var.Q,var.Q$REH1.Q,pro)
svmRadial<-RasterOverPoints(st$svmRadial,var.Q,var.Q$REH1.Q,pro)
treebag<-RasterOverPoints(st$treebag,var.Q,var.Q$REH1.Q,pro)
cubist<-RasterOverPoints(st$cubist,var.Q,var.Q$REH1.Q,pro)
ctree<-RasterOverPoints(st$ctree,var.Q,var.Q$REH1.Q,pro)
knn<-RasterOverPoints(st$knn,var.Q,var.Q$REH1.Q,pro)
earth<-RasterOverPoints(st$earth,var.Q,var.Q$REH1.Q,pro)
gbm<-RasterOverPoints(st$gbm,var.Q,var.Q$REH1.Q,pro)

kriging<-ked_exp.cv

differences<-list(SICCS=SICCS,
                  lm=lm,
                  glm=glm,
                  gaussprLinear=gaussprLinear,
                  svmLinear=svmLinear,
                  svmRadial=svmRadial,
                  treebag=treebag,
                  cubist=cubist,
                  ctree=ctree,
                  knn=knn,
                  earth=earth,
                  gbm=gbm,
                  kriging=kriging)
SICCS<-data.frame(Y,m,d,var.name,SICCS)
lm<-data.frame(Y,m,d,var.name,lm)
glm<-data.frame(Y,m,d,var.name,glm)
gaussprLinear<-data.frame(Y,m,d,var.name,gaussprLinear)
svmRadial<-data.frame(Y,m,d,var.name,svmRadial)
svmLinear<-data.frame(Y,m,d,var.name,svmLinear)
svmRadial<-data.frame(Y,m,d,var.name,svmRadial)
treebag<-data.frame(Y,m,d,var.name,treebag)
cubist<-data.frame(Y,m,d,var.name,cubist)
ctree<-data.frame(Y,m,d,var.name,ctree)
knn<-data.frame(Y,m,d,var.name,knn)
earth<-data.frame(Y,m,d,var.name,earth)
gbm<-data.frame(Y,m,d,var.name,gbm)

###############################################################################
###############################################################################
###############################################################################
#What we want to save
name_differences<-"time_diff.rds"
name_model<-"time_model.rds"
name_st<-"time_st.rds"

name_differences<-gsub("time",time,name_differences)
name_model<-gsub("time",time,name_model)
name_st<-gsub("time",time,name_st)

#(1a) difference between observed and satellite values
#(1b) difference between observed and predicted values (kriging and all caret models)
saveRDS(differences,paste0(dir_differences,name_differences))
#(2) all models
saveRDS(models,paste0(dir_model,name_model))
#(3) All predictions and layer of satellite in 1 object
saveRDS(st,paste0(dir_st,name_st))
#(4) Statistical summary of all models and their date
summary.data<-data.frame(Y,m,d,
                         ked_exp.r2,
                         lm.r2,
                         glm.r2,
                         gaussprLinear.r2,
                         svmLinear.r2,
                         svmRadial.r2,
                         treebag.r2,
                         cubist.r2,
                         ctree.r2,
                         knn.r2,
                         earth.r2,
                         gbm.r2,
                         ked_exp.rmse,
                         lm.rmse,
                         glm.rmse,
                         gaussprLinear.rmse,
                         svmLinear.rmse,
                         svmRadial.rmse,
                         treebag.rmse,
                         cubist.rmse,
                         ctree.rmse,
                         knn.rmse,
                         earth.rmse,
                         gbm.rmse)
                         
if (i==1) {
  print(summary.data)
  print(SICCS)
} else {names(summary.data)<-NULL
        names(SICCS)<-NULL
        names(lm)<-NULL
        names(glm)<-NULL
        names(gaussprLinear)<-NULL
        names(svmRadial)<-NULL
        names(svmLinear)<-NULL
        names(treebag)<-NULL
        names(cubist)<-NULL
        names(ctree)<-NULL
        names(knn)<-NULL
        names(earth)<-NULL
        names(gbm)<-NULL}

write.table(SICCS,file=paste0(summary_differences,"SICCS.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(lm,file=paste0(summary_differences,"lm.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(glm,file=paste0(summary_differences,"glm.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(gaussprLinear,file=paste0(summary_differences,"gaussprLinear.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(svmRadial,file=paste0(summary_differences,"svmRadial.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(svmLinear,file=paste0(summary_differences,"svmLinear.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(treebag,file=paste0(summary_differences,"treebag.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(cubist,file=paste0(summary_differences,"cubist.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(ctree,file=paste0(summary_differences,"ctree.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(knn,file=paste0(summary_differences,"knn.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(earth,file=paste0(summary_differences,"earth.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(gbm,file=paste0(summary_differences,"gbm.csv"),append=TRUE,sep=",",row.names=FALSE)
write.table(summary.data,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/RMSE_Rsquared.csv",
          append=TRUE,sep=",",row.names=FALSE)
}
}
}