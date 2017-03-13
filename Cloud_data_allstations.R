#Convert txt to Rdataset

#data is downloaded from: http://kisapp.knmi.nl:8080/servlet/selection/SelectTableElementGroup/
#first rows are skiped, long header

library(data.table)

#clouds
cloudfile<-"/nobackup/users/dirksen/radiation/Rdata/KNMI_20170208.txt"
cloud.df<-fread(cloudfile,
      skip=61,
      col.names=c("STN","DATE","NG"),
      colClasses = c("character","character","interger"),
      na.string=NULL,
      sep=",")

cloud.df$DATE<-as.Date(cloud.df$DATE,format="%Y%m%d") 


I=which(cloud.df$NG=="")
cloud.df<-cloud.df[!I,]

saveRDS(cloud.df,"Rdata/cloudcover.rda")

#Snow cover
#from the snow cover dataset no data is stored in snowdepth!

snowcoverfile<-"/nobackup/users/dirksen/radiation/Rdata/dirksen_ren1_sx.dat"
meta<-fread("/nobackup/users/dirksen/radiation/Rdata/meta_snow.csv",colClasses = "character")
meta<-subset(meta,select=c("DS_CODE","DS_NAME","DS_LAT","DS_LON"))
meta$DS_CODE<-gsub("_N","",meta$DS_CODE)

snowsun.df<-fread(snowcoverfile,
                header=FALSE,
                colClasses = "character",
                na.string=NULL,
                sep=",")
names(snowsun.df)<-c("DS_CODE","DATE","SNOW")

snow.df<-merge(meta,snowsun.df,by.x="DS_CODE",by.y="DS_CODE")


sun<-which(snowsun.df$REH1.SQ24=="")
snow<-which(snowsun.df$BGN.Q_S=="") #entire data row is empty!

snow.df<-snow.df[!snow,]


sun.df<-snowsun.df[!sun,]
sun.df<-subset(sun.df,select=c("IT_DATETIME","DS_CODE","REH1.SQ24"))
sun.df$IT_DATETIME<-as.Date(sun.df$IT_DATETIME,format="%Y%m%d_240000_000000")
sun.df$DS_CODE<-gsub("_H","",sun.df$DS_CODE)

saveRDS(sun.df,"Rdata/sunshine.rda")

#compare with maximum duration of light from: https://cran.r-project.org/web/packages/insol/insol.pdf
#function: daylength
library(insol)
?daylength
