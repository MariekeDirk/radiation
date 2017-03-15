#Kriging visualization
#Run Application
#runApp("/nobackup/users/dirksen/radiation/Rdata/Kriging/shinyapp_kriging/")

library(shiny)
library(data.table)
library(ggplot2)
library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(graphics)
library(automap)
##################################################
# Loading the maps
##################################################
pro=CRS("+init=epsg:28992")
mymap.unpro=readOGR(dsn='/nobackup/users/dirksen/radiation/Rdata/NaturalEarthData/ne_10m_admin_0_countries/',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map

mymap.unpro_lakes=readOGR(dsn='/nobackup/users/dirksen/radiation/Rdata/NaturalEarthData/ne_10m_lakes/',layer="ne_10m_lakes") # Read in (unprojected) map data

mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map
##################################################
##################################################

##################################################
# Folders were the data from radiation is stored
##################################################
mainpath<-"/nobackup/users/dirksen/radiation/Rdata"

output.sum<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".txt",full.names = T)
r.kriging<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda",full.names = T)
r.kriging.predictions<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".grd",full.names = T)
r.kriging.dates<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda")
r.kriging.dates<-as.Date(r.kriging.dates,format="ked_exp_model_%Y-%m-%d.rda")
satellitegrids<-list.files(paste0(mainpath,"/Satellite_data/temp/"),pattern=".grd",full.names = T)
# coords.knmi<-readRDS("/nobackup/users/dirksen/radiation/Rdata/coordsKNMI.rda")

output.sum<-fread(output.sum)

coordsRD<-readRDS(paste0(mainpath,"/coordsRDknmi.rda"))

output.sum<-merge(output.sum,coordsRD,by.x=c("x","y"),by.y=c("x","y"))

output.sum<-subset(output.sum,select=c("date","SAT","DS_CODE","x","y","observed","residual","var1.pred","r2","rmse","rmse_sd","me"))
##################################################
##################################################

##################################################
# Locations were the Temperature data is stored

#To do: rerun the analysis and store in the same way as radiation!
r.kriging<-list.files("/run/media/dirksen/Elements/Final_R_scripts/R_scripts_HARMONIE/output_HARMONIE/output_int_day/ked_exp_prediction/",pattern=".asc")

r.harmonie<-list.files("/nobackup/users/dirksen/Temperature/Temperature/Data/HARMONIE/",pattern=".grd",full.names = T)


r.harmonie.dates<-list.files("/nobackup/users/dirksen/Temperature/Temperature/Data/HARMONIE/",pattern=".grd",full.names = F)
r.harmonie.dates<-as.Date(r.harmonie.dates,format="file%Y%m%dT123000Z.grd")
#Test case
# t<-r.kriging.dates[2]
# out.sum<-output.sum[which(output.sum$date==t)]
# r.k<-readRDS(r.kriging[which(r.kriging.dates==t)])
# r.p<-raster(r.kriging.predictions[which(r.kriging.dates==t)])
# r.s<-raster(satellitegrids[which(r.kriging.dates==t)])
# 
# coordinates(out.sum)<-~x+y
# proj4string(out.sum)<-pro
# # out.sum<-data.frame(out.sum)
# # 
# # gplot(r.p) + geom_tile(aes(fill = value)) +
# #   # geom_polygon(data=mymap.pro,colour="black",size=1)
# #   facet_wrap(~ variable) +
# #   scale_fill_distiller(palette = "YlGnBu",direction=1) +
# #   # scale_fill_gradient(low = 'white', high = 'blue') +
# #   coord_equal()+
# #   geom_point(data=out.sum,aes(x=x,y=y),color="grey20")+
# #   coord_cartesian(xlim = c(12621.630033977,278621.630033977), ylim = c(305583.0457758,620583.0457758))+
# #   geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
# #   geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)
# x<-12621.630033977
# y<-305583.0457758
# click<-SpatialPoints(coords=data.frame(x,y),proj4string = pro)
# d<-gDistance(click,out.sum,byid=T)
# dmin<-min(d)

function(input, output) {
  
  ##################################################
  ###################INPUT##########################
  ##################################################
   #Observation data for input date
  selectedDate <- reactive({
    out.sum<-output.sum[which(output.sum$date==input$date)]
  })
  
  #Kriging model output for input date
  kriging.raster <- reactive({
    r.k<-readRDS(r.kriging[which(r.kriging.dates==input$date)])
  })
  
  #Kriging prediction for input date
  kriging.prediction<-reactive({
    r.p<-raster(r.kriging.predictions[which(r.kriging.dates==input$date)])
  })
  
  #Satellite grid for input date
  satellite.grid<-reactive({
    r.s<-raster(satellitegrids[which(r.kriging.dates==input$date)])
  })
  
  #create spatial points and data frame for observation for input date ggplot
  spatialpoints.data<- reactive({
    out.sum.sp<-output.sum[which(output.sum$date==input$date)]
    coordinates(out.sum.sp)<-~x+y
    proj4string(out.sum.sp)<-pro
    out.sum.sp<-data.frame(out.sum.sp)
  })
  
  ##################################################
  ####################OUTPUT########################
  ##################################################
  # All the plots are linked with the axis, if one zooms, all zoom
  # The zoomplot and satellite are also linked with the click function, if you click on one information on both plots is shown
  
  # Define starting values for xlim and ylim
  ranges <- reactiveValues(x = c(12621.630033977,278621.630033977), y = c(305583.0457758,620583.0457758))
  
  #Plotting the prediction, error and variogram fit
  output$krigeplot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(kriging.raster(),xlim=ranges$x, ylim = ranges$y,
         col=do.call(input$color,list(n=50)))
  })
  
  #
  output$zoomplot <- renderPlot({
    gplot(kriging.prediction()) + geom_tile(aes(fill = value)) +
      labs(title="Kriging Prediction",x="x-coords",y="y-coord")+
      facet_wrap(~ variable) +
      scale_fill_gradientn(colours=do.call(input$color,list(n=50))) +
      coord_equal()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)+
      geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
      geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)+
    
    geom_point(data=spatialpoints.data(),aes(x=x,y=y,fill=observed),pch=21,size=2,color="grey20")  
      
   
      })
  
  output$satellite<-renderPlot({
    gplot(satellite.grid()) + geom_tile(aes(fill = value)) +
      labs(title="SICCS",x="x-coords",y="y-coord")+
      facet_wrap(~ variable) +
      scale_fill_gradientn(colours=do.call(input$color,list(n=50))) +
      coord_equal()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)+
      geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
      geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)+
      
      geom_point(data=spatialpoints.data(),aes(x=x,y=y,fill=observed),pch=21,size=2,color="grey20")  
      
    
  })
  #First plot with kriging prediction has the following text box after click:
  output$info <- renderText({
    if(is.null(input$plot_click$x)) return(NULL)
    click         <- SpatialPoints(coords=data.frame(input$plot_click$x, input$plot_click$y),
                                   proj4string = pro)
    
    sgdf.k<-as(kriging.prediction(),"SpatialGridDataFrame")
    sgdf.s<-as(satellite.grid(),"SpatialGridDataFrame")
    
    values.k<-over(click,sgdf.k)
    values.s<-over(click,sgdf.s)
    paste0("X = ",round(input$plot_click$x,0),
           "\nY = ",round(input$plot_click$y,0),
           "\nValue Kriging = ",round(values.k,1)," W/m2",
           "\nValue SICCS = ",round(values.s,1)," W/m2")
    })
  
  #For the second plot of the satellite image the next text box is generated:
  output$infosat <- renderText({
    if(is.null(input$plot_click$x)) return(NULL)
    click         <- SpatialPoints(coords=data.frame(input$plot_click$x, input$plot_click$y)
                                   ,proj4string = pro)
    sp.data<-spatialpoints.data()
    coordinates(sp.data)<-~x+y
    proj4string(sp.data)<-pro
    
    
    d<-pointDistance(click,sp.data,lonlat = F)
    d.km.min<-round(min(d)/1000,0)
    
    lowest<-which.min(d)
    d.value<-data.frame(sp.data[lowest,])
    d.station<-d.value$DS_CODE
    d.value<-d.value$observed
    
    paste0( "Nearest AWS station = ",d.station,
            "\nDistance nearest AWS station = ",d.km.min," km",
           "\nObserved AWS value = ",round(d.value,1)," W/m2")
    
  })
  
  # ranges2 <- reactiveValues(x = c(12621.630033977,278621.630033977), y = c(305583.0457758,620583.0457758))
    
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$zoomplot_dblclick, {
      brush <- input$zoomplot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- c(12621.630033977,278621.630033977)
        ranges$y <- c(305583.0457758,620583.0457758)
      }
    })
  
  ############################################################################################
  ############################################################################################  
  ############################################################################################
    output$summary<-renderTable({
    statistics<-selectedDate()
    statistics<-subset(statistics,select=c("observed","var1.pred","r2","rmse","rmse_sd","me"))
    names(statistics)<-c("obs","pred","R2","RMSE","RMSEsd","ME")
    
    if (input$statistics=="quantile"){
      lapply(statistics,input$statistics,probs=c(0.25,0.50,0.75))
    } else {lapply(statistics,input$statistics)}
    # probs=c(0.25,0.50,0.75)
  })
  
  output$summary.station<-renderTable({
    df<-selectedDate()
    df<-df[which(df$DS_CODE==input$stations),]
    df<-subset(df,select=c("DS_CODE","observed","residual","var1.pred","r2","rmse","rmse_sd","me"))
  })
  ############################################################################################
  ############################################################################################
  ############################################################################################
  #Plotting routine according to the following example
  #http://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  output$obsvsmeas<-renderPlot({
    plot.df<-selectedDate()
    
    # ggplot(plot.df,aes_string(get(input$xvar),get(input$yvar)))+
    #   geom_point(color=input$pointcolor,size=as.numeric(input$pointsize))

    ggplot(plot.df,aes(observed,SAT))+
      geom_point(color=input$pointcolor,size=as.numeric(input$pointsize))
  })
  
  output$plot3<-renderPlot({
   plot.df<-selectedDate()
   ggplot(plot.df,aes(observed,SAT))+
   geom_point(color=input$pointcolor,size=as.numeric(input$pointsize))+
   coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    
  })
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  # output$infopoint<-renderPrint({
  #   if(is.null(input$point_click)) return(NULL)
  #   plot.df<-selectedDate()
  #   plot.df<-subset(plot.df,select=c("DS_CODE","observed","SAT","var1.pred","r2","rmse","rmse_sd","me"))
  #   nearPoints(plot.df,input$point_click)
  # })
  
  output$infobrush<-renderPrint({
    if(is.null(input$plot2_brush)) return(NULL)
    plot.df<-selectedDate()
    plot.df<-subset(plot.df,select=c("DS_CODE","observed","SAT","var1.pred","r2","rmse","rmse_sd","me"))
    brushedPoints(plot.df,input$plot2_brush)
  })
  ############################################################################################
  ############################################################################################
  ############################################################################################
  
  
  
}

