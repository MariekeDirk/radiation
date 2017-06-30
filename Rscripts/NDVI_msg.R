################
################vegetation index ECMWF
library(raster)
library(rgdal)
library(MODIS)

#RD projection
pro=CRS("+init=epsg:28992")

#Extent Netherlands
xlim = c(12621.630033977,278621.630033977)
ylim = c(305583.0457758,620583.0457758)
ext<-extent(xlim,ylim)

loc.grid<-'/nobackup/users/dirksen/landuse_MSG/MSG1-SEVI-MSGNDVD-0100-0100-20170320120000.000000000Z-20170321001357-1211986.h5'
evi.meta<-h5read(loc.grid,"/METADATA")

test<-h5ls("/nobackup/users/dirksen/clouds_ECMWF/CFCmm200710010000300070023201MA.hdf")
msg.grid<-h5read("/nobackup/users/dirksen/clouds_ECMWF/CFCmm200710010000300070023201MA.hdf","/")

NDVI.file<-'/nobackup/users/dirksen/landuse_MSG/MSG1-SEVI-MSGNDVD-0100-0100-20170320120000.000000000Z-20170321001357-1211986.h5'
sds <- getSds('/nobackup/users/dirksen/landuse_MSG/MSG1-SEVI-MSGNDVD-0100-0100-20170320120000.000000000Z-20170321001357-1211986.h5') # Make sure to use full path
evi <- raster(readGDAL(sds$SDS4gdal[3], as.is = TRUE))
extent(evi)<-c(-8887500, 8887500, -8887500 ,8887500)

evi.sp<-as(evi,"SpatialPointsDataFrame")
proj4string(evi.sp)<- "+proj=geos +lon_0=0.000000 +lat_0=0 +h=35807.414063 +a=6378.169 +b=6356.5838"
  #CRS("+init=epsg:4326")
  #"+proj=geos +lon_0=0.000000 +lat_0=0 +h=35807.414063 +a=6378.169 +b=6356.5838"

evi.RD<-spTransform(evi.sp, pro)
crop(evi.RD,ext)

rast<-raster(ncol = 100, nrow = 100)
extent(rast)<-extent(evi.RD)
rast_with_values <- rasterize(evi.RD, rast, fun = mean)
