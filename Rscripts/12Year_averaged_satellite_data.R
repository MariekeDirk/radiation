#Sum for asc grid output
#Generating yearly and montly data

rm(list=ls(all=TRUE)) # clear the working space

library(sp)
library(adehabitat)
library(lubridate)
library(data.table)
library(raster)
library(rgdal)
library(rhdf5)

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")
starttime<-as.POSIXct("2004-01-19")
stoptime<-as.POSIXct("2016-01-19")

time.seq<-seq(from=starttime,to=stoptime,by="day")


for (i in 1:length(time.seq)){
t<-time.seq[i]
print(t)
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
} else {
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
  names(r.NED)<-"SAT"
  r.NED.matrix<-as.matrix(r.NED)
  if (length(which(is.na(r.NED.matrix)))>2000) {
    print("NO data")
    # out<-data.frame(time,"no data NL")
    # names(out)<-NULL
    # write.table(out,file="/nobackup/users/dirksen/Radiation_Obs_Satellite/output/statistical_summary/NOdata_days.csv",
    #             append=TRUE,sep=",",row.names=FALSE)
  } else {
    

if (i==1){
  GRID_year<-r.NED
  n=1
} else  {GRID_year<-GRID_year+r.NED
        n=n+1}


  }
}
}

GRID_year<-GRID_year/n
# GRID_year<-raster(GRID_year)

GRID_new<-"/nobackup/users/dirksen/radiation/Rdata/Satellite_data/satellite12year.rds"
saveRDS(GRID_year,file=GRID_new)

#############
#############Plotting routines
#############
library(RColorBrewer)
kleur.breaks<-seq(115,200,by=1)
kleur.cols<-colorRampPalette(c("green","yellow","orange"))(length(kleur.breaks-1))
# kleur.cols<-terrain.colors(length(kleur.breaks-1))

#Natural Earth dataset: unprojected shape files
mymap.unpro=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_admin_0_countries',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map

mymap.unpro_lakes=readOGR(dsn='Rdata/NaturalEarthData/ne_10m_lakes',layer="ne_10m_lakes") # Read in (unprojected) map data
mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map

fun <- function() {
  plot(mymap.pro,add=TRUE)
  plot(mymap.pro_lakes,add=TRUE)
}

TestChars <- function(encoding = "ISOLatin1", family = "URWHelvetica")
{
  postscript(file="/nobackup/users/dirksen/radiation/Rdata/Satellite_data/test.ps",
             encoding = encoding, 
             family = family,
             onefile = FALSE,
             horizontal=TRUE)
  
  plot(GRID_year,col=kleur.cols,legend=TRUE,addfun=fun,xlab="x-coordinates",ylab="y-coordinates")
  title(paste("Solar Irradiance Climatology 2004-2016 \n [W/m2]"))
  dev.off()
}

TestChars()
