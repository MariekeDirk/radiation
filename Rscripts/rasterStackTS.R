# #Sum for asc grid output
# #Generating yearly and montly data
# 
# rm(list=ls(all=TRUE)) # clear the working space
# 
.libPaths("/nobackup/users/dirksen/R/x86_64-redhat-linux-gnu-library/3.3/")
library(sp)
library(adehabitat)
library(lubridate)
library(data.table)
library(raster)
library(rgdal)
library(rhdf5)
library(xts)
library(raster)
library(rts)
library(rgdal)
library(RColorBrewer)
pro <- CRS("+init=epsg:28992")
#kleur.breaks<-seq(115,200,by=1)
#kleur.cols<-colorRampPalette(c("green","yellow","orange"))(length(kleur.breaks-1))
# # kleur.cols<-terrain.colors(length(kleur.breaks-1))
# 
#Natural Earth dataset: unprojected shape files
mymap.unpro=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_admin_0_countries',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map

mymap.unpro_lakes=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_lakes',layer="ne_10m_lakes") # Read in (unprojected) map data
mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map

fun <- function() {
  plot(mymap.pro,add=TRUE)
  plot(mymap.pro_lakes,add=TRUE)
}

# #########rts analysis

# list<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = TRUE)
#list2<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd")

path.grid<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily_distsea/" #kriging with distsea
#path.grid<-"/nobackup/users/dirksen/radiation/Rdata/Kriging/Daily/" #kriging with satellite

#Kriging daily files
list<-list.files(path.grid,pattern=".grd",full.names = TRUE)
list2<-list.files(path.grid,pattern=".grd")
# # 
time.vector<-gsub(".grd","",list2)
time.vector<-gsub("ked_exp_prediction_","",time.vector)
time.vector<-as.POSIXct(time.vector,format="%Y-%m-%d")
st<-stack(list)
st.names<-names(st)
I.pred<-as.numeric(grep("var1.pred.",st.names))
#seq(1,13050,by=3)
st.pred<-subset(x=st,subset=I.pred)
# saveRDS(st,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/stack/st.rds")
#st<-readRDS("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/stack/st.rds")
stts<-rts(st.pred,time.vector)

#mean for all quarters
yq<-as.yearqtr(time.vector)
indices<-format(yq,format="%q")
mean.quarter <- stackApply(st,indices,fun=sd) # Calculating the mean for each month
names(mean.quarter)<-c("Q1","Q2","Q3","Q4")
plot(mean.quarter)

# saveRDS(mean.quarter,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/quarters_sd.rds")

#mean for all months
indices<-format(time.vector,format="%m")
indices<-as.numeric(indices)

mean.month <- stackApply(st.pred,indices,fun=mean) # Calculating the mean for each month
names(mean.month)<-c("Jan","Feb","March","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")

kleur.breaks<-seq(18,245,by=0.5)
kleur.cols<-colorRampPalette(c("green","yellow","orange"))(length(kleur.breaks-1))
plot.new()
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#png(filename="/usr/people/dirksen/reports/EGU2017_proceedings/fig/monthly_mean_distsea.png", width = 3200, height = 2500, units = "px",res=300)

plot(mean.month,addfun=fun,col=kleur.cols,
     ext=extent(11766.11,279946.1,305137.7 ,618857.7 ),
     legend.args=list(text='W/m2', side=3, cex=0.5
     ))
#dev.off()
###################################################################
###################################################################
###################################################################
# 
# pro=CRS("+init=epsg:28992")
# WGS84<-CRS("+init=epsg:4326")
# starttime<-as.POSIXct("2004-01-19")
# stoptime<-as.POSIXct("2016-01-19")
# 
# time.seq<-seq(from=starttime,to=stoptime,by="day")
# 
# 
# for (i in 1:length(time.seq)){
# t<-time.seq[i]
# print(t)
# Y<-year(t)
# M<-month(t)
# 
# if (M<=9) {
#   M<-paste0(0,M)
# } else {print(M)}
# 
# D<-mday(t)
# 
# if (D<=9) {
#   D<-paste0(0,D)
# } else {print(D)}
# 
# file.loc<-"/net/pc150398/nobackup_1/users/meirink/siccs_wouter/SICCS/daymean/harmonie_proj/year"
# file.loc<-gsub("year",Y,file.loc)
# files.in.loc<-list.files(file.loc,pattern=".h5")
# 
# d.sat<-gsub("daymean_reproj_","",files.in.loc)
# d.sat<-gsub(".h5","",d.sat)
# I=which(d.sat==paste0(Y,M,D))
# if (length(I)==0){
#   print("NO FILE")
#   # out<-data.frame(time,"no file")
#   # names(out)<-NULL
#   # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
#   #             append=TRUE,sep=",",row.names=FALSE)
# } else {
#   file.sat<-"/net/pc150398/nobackup_1/users/meirink/siccs_wouter/SICCS/daymean/harmonie_proj/year/daymean_reproj_yyyymmdd.h5"
#   file.sat<-gsub("year",Y,file.sat)
#   file.sat<-gsub("yyyymmdd",paste0(Y,M,D),file.sat)
#   
#   h5ls(file.sat)
#   data.direct.irradiance<-h5read(file.sat,"direct irradiance")
#   data.diffuse.irradiance<-h5read(file.sat,"diffuse irradiance")
#   data.total.irradiance<-data.direct.irradiance+data.diffuse.irradiance
#   data.total.irradiance<-t(data.total.irradiance)
#   
#   data.lat<-h5read(file.sat,"/lat") #Latitude
#   data.lon<-h5read(file.sat,"/lon") #Longitude
#   
#   data.lat[which(data.lat==-999)]<-NA # replace no data value with NA (this case -999)
#   data.lon[which(data.lon==-999)]<-NA # replace no data value with NA (this case -999)
#   
#   r<-raster(data.total.irradiance,crs=WGS84,
#             xmn=min(data.lon,na.rm=T),
#             xmx=max(data.lon,na.rm=T),
#             ymn=min(data.lat,na.rm=T),
#             ymx=max(data.lat,na.rm=T))
#   rr<-flip(r,direction='y')
#   rr<-projectRaster(rr,crs=pro)
#   
#   r.NED<-crop(rr,extent(12621.630033977,278621.630033977,305583.0457758,620583.0457758))
#   names(r.NED)<-"SAT"
#   r.NED.matrix<-as.matrix(r.NED)
#   if (length(which(is.na(r.NED.matrix)))>2000) {
#     print("NO data")
#     # out<-data.frame(time,"no data NL")
#     # names(out)<-NULL
#     # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
#     #             append=TRUE,sep=",",row.names=FALSE)
#   } else {
#     
# writeRaster(r.NED, paste0("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",Y,M,D))
# # if (i==1){
# #   GRID_year<-r.NED
# #   n=1
# # } else  {GRID_year<-GRID_year+r.NED
# #         n=n+1}
# 
# 
#   }
# }
# }

#saveRDS(mean.month,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/months_sd.rds")
##################################################
##################################################
#mean for all years
indices<-format(time.vector,format="%Y")
indices<-as.numeric(indices)

mean.year <- stackApply(st,indices,fun=sd) # Calculating the mean for each month
#names(mean.month)<-c("Jan","Feb","March","Apr","May","June","July","Aug","Sept","Okt","Nov","Dec")
plot(mean.year)

saveRDS(mean.year,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/months_sd.rds")

#####################################################
#Time series R2 and RMSE
mean.solar.irradiance<-summary(mean.month)
mean.solar.irradiance<-mean.solar.irradiance[3,]
library(data.table)
library(adehabitat)
time.series<-fread("/nobackup/users/dirksen/radiation/Rdata/Kriging/statistics_daily.txt")
time.series$t<-month(as.Date(time.series$t))
time.series<-as.data.frame(time.series)
mean.per.month<-aggregate(time.series[-1],list(time.series$t),mean)

test<-cbind(mean.per.month,mean.solar.irradiance)
test<-test/test$mean.solar.irradiance*100
#####################################################
#####################################################
# plot(stts[[1]],addfun=fun)
# #drawExtent()
# # grd.ts<-rts(grd.test,time.vector)
# 
# #MEAN values
# ends<-endpoints(stts,on='quarters')
# out<-period.apply(stts,ends,sd)
# 
# ends2<-endpoints(out,on='months',13)
# out2<-period.apply(out,ends2,sd)
# 
# #standard deviation
# # ends<-endpoints(stts,on='quarters')
# # out<-period.apply(stts,ends,sd)
# 
# ends2<-endpoints(stts,on='months',13)
# out2<-period.apply(stts,ends2,mean)
# 
# ends3<-endpoints(stts,'years',13)
# out2<-period.apply(stts,ends3,sd)
# ###################
# ###################
setwd("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/")

#read all the climatology data
months_mean<-readRDS("months_mean.rds")
months_sd<-readRDS("months_sd.rds")
quarters_mean<-readRDS("quarters_mean.rds")
quarters_sd<-readRDS("quarters_sd.rds")
yearly_mean<-readRDS("yearly_mean.rds")
yearly_sd<-readRDS("yearly_sd.rds")
yearly12_mean<-readRDS("yearly12_mean.rds")
yearly12_sd<-readRDS("yearly12_sd.rds")

st<-months_sd
##Plotting routine
#http://stackoverflow.com/questions/29828821/r-raster-avoid-white-space-when-plotting
plot.new()
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
#plot.window(xlim=ext[1:2], ylim=ext[3:4], xaxs="i",yaxs="i")
plot(st,addfun=fun,col=kleur.cols,legend=T,
     ext=extent(12621.630033977,278621.630033977,305583.0457758,620583.0457758))

r<-mean.month
r.range <- c(minValue(r), maxValue(r))
r.range <- c(18, 145)
plot(r, legend.only=TRUE, col=topo.colors(100),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2],50),
                    labels=seq(r.range[1], r.range[2],50), 
                    cex.axis=0.6),
     legend.args=list(text='W/m2', side=4, font=2, line=4.5, cex=0.8))
## Finally, plot the (sub-)raster to it
# st<-stack(out2@raster)
# names(st)<-c("Spring","Summer","Autumn","Winter")
# names(st)<-c("Jan","Feb","March","Apr","May","June","July","Aug","Sept","Okt","Nov","Dec")

#st<-dropLayer(st,"layer")
# names(st)<-c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
# plot(st,addfun=fun,col=kleur.cols,legend=TRUE)

# saveRDS(st,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/climatology/yearly12_sd.rds")
###############TREND ANALYSIS
#information on grids
plot(stts[[1]],addfun=fun)




Cabauw<-data.frame(x=123295.17 , y=442405.9)
coordinates(Cabauw)<-~x+y
proj4string(Cabauw)<-pro

a <- extract(x=stts,y=Cabauw)

library(mgcv)
a.df<-data.frame(a,time.vector,year(time.vector),yday(time.vector),rep(1,length(time.vector)))
names(a.df)<-c("value","time","year","yday","dummy")
fit1 <- gam(value ~ s(year)+ s(yday, bs = "cc"), data = a.df)
fit2 <- gam(value ~ s(year,bs="ts"), data = a.df)
plot(a.df$time,a.df$value,xlab="time [years]",ylab="solar irradiance [W/m2]",pch="*")
lines(a.df$time,fit1$fitted.values,col="red",lwd=2)
lines(a.df$time,fit2$fitted.values,col="blue",lwd=2)

#reading material for smoothing:
#http://stats.stackexchange.com/questions/12223/how-to-tune-smoothing-in-mgcv-gam-model

#Linear fit is biased at the end and beginning!
# lm.fit<-lm(a.df$value~a.df$time)
# 
# lines(a.df$time,lm.fit$fitted.values,col="yellow",lwd=2)


# plot(a[1:700],pch=".")
# lines(apply.weekly(a,FUN=mean),col="blue")
# lines(apply.monthly(a,FUN=mean),col="green")
# lines(apply.quarterly(a,FUN=mean),col="yellow")
# lines(apply.yearly(a,FUN=mean),col="red")

a.ts<-ts(a,frequency=365)
dec<-decompose(a.ts)

trend.sd<-sd(dec$trend,na.rm=T)
trend.mean<-mean(dec$trend,na.rm=T)



dataforfit<-data.frame(year(time.vector),yday(time.vector),dec$seasonal,dec$trend)
names(dataforfit)<-c("year","yday","seasonal","trend")
fit1 <- gam(seasonal ~ s(year)+ s(yday, bs = "cc"), data = dataforfit)

fit2 <- gam(trend ~ s(year)+ s(yday, bs = "cc"), data = dataforfit)





#to make a nice plot see: http://zevross.com/blog/2014/09/15/recreate-the-gam-partial-regression-smooth-plots-from-r-package-mgcv-with-a-little-style/
#Fit for the seasonal trend
fit3<-gam(seasonal ~ s(year)+s(yday,bs="cc"), data=dataforfit)
out<-predict(fit3,data=dataforfit)
plot(dec$seasonal)
lines(out,col='red')

#Linear fit for the trend over several years
fit4<-gam(trend~s(year),data=dataforfit)
out<-predict(fit4,dataforfit) # to get the values for the fitted model at each time-step I used predict
#out<-data.frame(time.vector,out)
plot(dec$trend)
lines(out,col='red')
#points(out)

stl(a.ts,s.window="period") #http://r-video-tutorial.blogspot.nl/2015/05/introductory-time-series-analysis-of-us.html
# crs(b) <- tura_cropped@crs
# 
# 
# b<-na.omit(b)
# plot(b,col=kleur.cols,legend=TRUE,addfun=fun,xlab="x-coordinates",ylab="y-coordinates")
# title(paste("Solar Irradiance Climatology 2004-2016 \n [W/m2]"))
#TEST FUNCTION
# grd.test<-crop(st, drawExtent())


########################
########################Experiment to calculate all the slopes for all pixels
########################
# library(doParallel)
# grd.test<-stts
# 
# test = function(x) {
#   x <- extract(grd.test, x)
#   fit2 <- gam(value ~ I(year), data = a.df)
#   #varsd <- sd(x, na.rm = TRUE)
#   return(as.numeric(fit2$coefficients[2]))
#   #return(list(as.numeric(fit2$coefficients[2]),as.numeric(summary.gam(fit2)$p.pv[2])))
# }
# 
# test_list <- as.list(c(1:13144))
# test_list <- as.list(c(1:6))
# a <- mclapply(test_list, test, mc.cores=8)
# 
# 
# b <- raster(matrix(unlist(a), nrow=st@nrows, ncol=st@ncols, byrow = TRUE))
# crs(b) <- st@crs
# extent(b) <- st@extent
# plot(b)
####################
####################
####################
#OLD GRIDDING OPTIONS
# GRID_year<-GRID_year/n
# # GRID_year<-raster(GRID_year)
# 
# GRID_new<-"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/satellite12year.rds"
# saveRDS(GRID_year,file=GRID_new)
# 
# #############
# #############Plotting routines
# #############
# GRID_year<-readRDS("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/satellite12year.rds")
# library(RColorBrewer)
# kleur.breaks<-seq(115,200,by=1)
# kleur.cols<-colorRampPalette(c("green","yellow","orange"))(length(kleur.breaks-1))
# # kleur.cols<-terrain.colors(length(kleur.breaks-1))
# 
# #Natural Earth dataset: unprojected shape files
# mymap.unpro=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_admin_0_countries',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
# mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map
# 
# mymap.unpro_lakes=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_lakes',layer="ne_10m_lakes") # Read in (unprojected) map data
# mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map
# 
# fun <- function() {
#   plot(mymap.pro,add=TRUE)
#   plot(mymap.pro_lakes,add=TRUE)
# }
# 
# TestChars <- function(encoding = "ISOLatin1", family = "URWHelvetica")
# {
#   postscript(file="/nobackup/users/dirksen/radiation/Rdata/Satellite_data/test.ps",
#              encoding = encoding, 
#              family = family,
#              onefile = FALSE,
#              horizontal=TRUE)
#   
#   plot(GRID_year,col=kleur.cols,legend=TRUE,addfun=fun,xlab="x-coordinates",ylab="y-coordinates")
#   title(paste("Solar Irradiance Climatology 2004-2016 \n [W/m2]"))
#   dev.off()
# }
# 
# TestChars()
