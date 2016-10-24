library(rhdf5)
library(sp)
library(raster)
library(reshape2)
library(rgdal)

pro <- CRS("+init=epsg:4326")

irradiance<-"Rdata/Tiggelen2014_days/daymean12EURO4M.hdf5"
loc.grid<-"Rdata/meteosat9.euro4m.angles.h5"

h5ls(irradiance)
data.direct.irradiance<-h5read(irradiance,"direct irradiance")
data.diffuse.irradiance<-h5read(irradiance,"diffuse irradiance")
data.total.irradiance<-data.direct.irradiance+data.diffuse.irradiance
#data.total.irradiance<-t(data.total.irradiance)

data.lat<-h5read(loc.grid,"/lat") #Latitude
data.lon<-h5read(loc.grid,"/lon") #Longitude

data.lat[which(data.lat==-999)]<-NA # replace no data value with NA (this case -999)
data.lon[which(data.lon==-999)]<-NA # replace no data value with NA (this case -999)

df = data.frame(lon = as.numeric(data.lon),          
                lat = as.numeric(data.lat),
                irr = as.numeric(data.total.irradiance))
sp = df[complete.cases(df),]
coordinates(sp) = ~lon+lat
proj4string(sp)<-"+proj=geos +lon_0=0.000000 +lat_0=0 +h=35807.414063 +a=6378.169 +b=6356.5838"
### Ik doe hier eerst de transformatie naar wgs84
sp_wgs84 = spTransform(sp, pro)

#nu een raster ervan maken
rast<-raster(ncol = 300, nrow = 300)
extent(rast)<-extent(sp_wgs84)
rast_with_values <- rasterize(sp_wgs84, rast, 'irr', fun = mean)

### Dit ziet er qua output wel aardig uit 
plot(rast_with_values)


