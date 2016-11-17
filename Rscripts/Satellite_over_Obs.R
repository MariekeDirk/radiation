library(adehabitat)
library(lubridate)
library(data.table)
library(raster)
library(rgdal)
library(rhdf5)
library(SDMTools)

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

starttime<-as.POSIXct("2004-01-19")
stoptime<-as.POSIXct("2016-09-30")

time.seq<-seq(from=starttime,to=stoptime,by="day")

for (i in 1:length(time.seq)){
t<-time.seq[i]
Y<-year(t)
M<-month(t)

if (M<=9) {
  M<-paste0(0,M)
} else {print(M)}

D<-mday(t)

if (D<=9) {
  D<-paste0(0,D)
} else {print(D)}

file.loc<-"/net/pc150398/nobackup_1/users/meirink/siccs_wouter/SICCS/daymean/harmonie_proj/year"
file.loc<-gsub("year",Y,file.loc)
files.in.loc<-list.files(file.loc,pattern=".h5")

d.sat<-gsub("daymean_reproj_","",files.in.loc)
d.sat<-gsub(".h5","",d.sat)
I=which(d.sat==paste0(Y,M,D))
if (length(I)==0){
  print("NO FILE")
  # out<-data.frame(time,"no file")
  # names(out)<-NULL
  # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
  #             append=TRUE,sep=",",row.names=FALSE)
} else{
file.sat<-"/net/pc150398/nobackup_1/users/meirink/siccs_wouter/SICCS/daymean/harmonie_proj/year/daymean_reproj_yyyymmdd.h5"
file.sat<-gsub("year",Y,file.sat)
file.sat<-gsub("yyyymmdd",paste0(Y,M,D),file.sat)

h5ls(file.sat)
data.direct.irradiance<-h5read(file.sat,"direct irradiance")
data.diffuse.irradiance<-h5read(file.sat,"diffuse irradiance")
data.total.irradiance<-data.direct.irradiance+data.diffuse.irradiance
data.total.irradiance<-t(data.total.irradiance)

data.lat<-h5read(file.sat,"/lat") #Latitude
data.lon<-h5read(file.sat,"/lon") #Longitude

data.lat[which(data.lat==-999)]<-NA # replace no data value with NA (this case -999)
data.lon[which(data.lon==-999)]<-NA # replace no data value with NA (this case -999)

r<-raster(data.total.irradiance,crs=WGS84,
xmn=min(data.lon,na.rm=T),
xmx=max(data.lon,na.rm=T),
ymn=min(data.lat,na.rm=T),
ymx=max(data.lat,na.rm=T))
rr<-flip(r,direction='y')
rr<-projectRaster(rr,crs=pro)

r.NED<-crop(rr,extent(12621.630033977,278621.630033977,305583.0457758,620583.0457758))
r.NED.matrix<-as.matrix(r.NED)
if (length(which(is.na(r.NED.matrix)))>2000) {
  print("NO data")
  # out<-data.frame(time,"no data NL")
  # names(out)<-NULL
  # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
  #             append=TRUE,sep=",",row.names=FALSE)
} else {
  
st<-stack(r.NED)  
coords<-readRDS("Rdata/coordsKNMI.rda")

obs<-fread("Rdata/radiation_KNMI_day_v2.csv")
obs$IT_DATETIME<-as.POSIXct(obs$IT_DATETIME,format="%Y%m%d_240000_000000")

obs.subset<-obs[which(IT_DATETIME==t),]
obs.subset$Q<-obs.subset$REH1.Q24*10000/(24*3600)
obs.subset<-merge(obs.subset,coords,by="DS_CODE")
obs.subset<-subset(obs.subset,select=c(IT_DATETIME,DS_CODE,DS_LAT,DS_LON,Q))
obs.subset<-na.omit(obs.subset)

coordinates(obs.subset)<-~DS_LON+DS_LAT
proj4string(obs.subset)<-WGS84
obs.subset<-spTransform(obs.subset,pro)

rASC<-asc.from.raster(r.NED)
spdf<-asc2spixdf(rASC)
proj4string(spdf)<-pro

var.Q<-subset(obs.subset,select=Q)
sat.var<-over(var.Q,spdf)

n<-names(sat.var)
diff<-sat.var[n]-var.Q$Q

sat_over_obs<-cbind(as.data.frame(obs.subset),sat.var,diff)
if (i==1) {
  print(sat_over_obs)
} else {names(sat_over_obs)<-NULL}

write.table(sat_over_obs,file="/nobackup/users/dirksen/radiation/Rdata/sat_over_obs2.csv",append=TRUE,sep=",",row.names = FALSE)
print(t)
}
}
}