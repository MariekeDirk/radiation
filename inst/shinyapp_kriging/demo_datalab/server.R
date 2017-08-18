.libPaths("/nobackup/users/dirksen/R/x86_64-redhat-linux-gnu-library/3.3/")
#runApp("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/demo_datalab/", launch.browser = TRUE)
library(leaflet)
library(shiny)
library(data.table)
library(raster)
library(rgdal)
library(rgeos)

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

xmin<-3.041776
xmax<-7.444776
ymin<-50.536674
ymax<-53.737374

mainpath<-"/nobackup/users/dirksen/radiation/data"

output.sum<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".txt",full.names = T)
# output.distshore.sum<-list.files(paste0(mainpath,"/Kriging/Daily_distsea/"),pattern = ".txt",full.names = T)
# r.kriging<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda",full.names = T)
r.kriging.predictions<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".grd",full.names = T)

r.kriging.dates<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda")
r.kriging.dates<-as.Date(r.kriging.dates,format="ked_exp_model_%Y-%m-%d.rda")
# satellitegrids<-list.files(paste0(mainpath,"/Satellite_data/temp/"),pattern=".grd",full.names = T)

output.sum<-fread(output.sum)

coordsRD<-readRDS(paste0(mainpath,"/coordsRDknmi.rda"))

output.sum<-merge(output.sum,coordsRD,by.x=c("x","y"),by.y=c("x","y"))

output.sum<-subset(output.sum,select=c("date","SAT","DS_CODE","x","y","observed","residual","var1.pred","r2","rmse","rmse_sd","me"))

daily.statistics<-fread("/nobackup/users/dirksen/radiation/data/Kriging/statistics_daily.txt")
daily.statistics$t<-as.Date(daily.statistics$t)

server<-function(input,output,server){
  #Observation data for input date
  selectedDate <- reactive({
    out.sum<-output.sum[which(output.sum$date==input$date)]
  })
  
  #Kriging model output for input date
  # kriging.raster <- reactive({
  #   r.k<-readRDS(r.kriging[which(r.kriging.dates==input$date)])
  # })
  
  #Kriging prediction for input date
  kriging.prediction<-reactive({
    r.p<-raster(r.kriging.predictions[which(r.kriging.dates==input$date)])
  })
  
  #Satellite grid for input date
  # satellite.grid<-reactive({
  #   r.s<-raster(satellitegrids[which(r.kriging.dates==input$date)])
  # })
  
  spatialpoints.data<- reactive({
    out.sum.sp<-output.sum[which(output.sum$date==input$date)]
    coordinates(out.sum.sp)<-~x+y
    proj4string(out.sum.sp)<-pro
    out.sum.sp<-spTransform(out.sum.sp,WGS84)
    out.sum.sp<-data.frame(out.sum.sp)
    # names(out.sum.sp)<-names(output.sum)
  })
  
  output$map<-renderLeaflet({
    r<-kriging.prediction()
    sp<-spatialpoints.data()
    
    sp.range<-range(sp$var1.pred)
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), c(values(r),sp.range),
                        na.color = "transparent")
    
    
    leaflet() %>% 
      fitBounds(xmin,ymin,xmax,ymax) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
     
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addRasterImage(r,colors=pal,opacity=0.6) %>%
      addLegend(pal=pal,values=values(r),position = "bottomright",title="Solar Irradiance") %>%
      addCircleMarkers(data=sp,lng=~x,lat=~y,label=~DS_CODE,color=~pal(observed),opacity=1,fillColor = ~pal(observed),fillOpacity = 0.8)
    
  })

  
}