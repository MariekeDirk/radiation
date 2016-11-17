# #Sum for asc grid output
# #Generating yearly and montly data
# 
# rm(list=ls(all=TRUE)) # clear the working space
# 
library(sp)
library(adehabitat)
library(lubridate)
library(data.table)
library(raster)
library(rgdal)
library(rhdf5)
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

library(xts)
library(rts)

library(RColorBrewer)
pro <- CRS("+init=epsg:28992")
kleur.breaks<-seq(115,200,by=1)
kleur.cols<-colorRampPalette(c("green","yellow","orange"))(length(kleur.breaks-1))
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

# list<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd",full.names = TRUE)
list2<-list.files("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/temp/",pattern=".grd")
# 
time.vector<-gsub(".grd","",list2)
time.vector<-as.POSIXct(time.vector,format="%Y%m%d")
# st<-stack(list)
# saveRDS(st,"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/stack/st.rds")
st<-readRDS("/nobackup/users/dirksen/radiation/Rdata/Satellite_data/stack/st.rds")
stts<-rts(st,time.vector)

plot(stts[[1]],addfun=fun)
#drawExtent()

#########rts analysis
# grd.ts<-rts(grd.test,time.vector)

#MEAN values
ends<-endpoints(stts,on='quarters')
out<-period.apply(stts,ends,mean)

ends2<-endpoints(out,on='months',13)
out2<-period.apply(out,ends2,mean)

#standard deviation
# ends<-endpoints(stts,on='quarters')
# out<-period.apply(stts,ends,sd)

# ends2<-endpoints(out,on='months',13)
# out2<-period.apply(out,ends2,sd)

ends3<-endpoints(stts,'years',13)
out3<-period.apply(stts,ends3,sd)


##Plotting routine
#http://stackoverflow.com/questions/29828821/r-raster-avoid-white-space-when-plotting
w <- ncol(out2)/max(dim(out2))
h <- nrow(out2)/max(dim(out2))
ext<-extent(12621.630033977,278621.630033977,305583.0457758,620583.0457758)
## Set up appropriately sized device with no borders and required coordinate system    
## png("eg.png", width=480*w, height=480*h)
dev.new(width=5*w, height=5*h)
plot.new()
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot.window(xlim=ext[1:2], ylim=ext[3:4], xaxs="i",yaxs="i")

## Finally, plot the (sub-)raster to it
st<-stack(out2@raster)
names(st)<-c("Spring","Summer","Autumn","Winter")

plot(st,addfun=fun,col=kleur.cols,legend=TRUE)

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
