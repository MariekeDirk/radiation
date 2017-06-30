#Kriging visualization
#Run Application
#runApp("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/")

#https://shiny.rstudio.com/articles/deployment-local.html
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
library(grid)
##################################################
# Loading the maps
##################################################
pro=CRS("+init=epsg:28992")
mymap.unpro=readOGR(dsn='/nobackup/users/dirksen/radiation/data/NaturalEarthData/ne_10m_admin_0_countries/',layer="ne_10m_admin_0_countries") # Read in (unprojected) map data
mymap.pro=spTransform(mymap.unpro, pro) # Reproject the map

mymap.unpro_lakes=readOGR(dsn='/nobackup/users/dirksen/radiation/data/NaturalEarthData/ne_10m_lakes/',layer="ne_10m_lakes") # Read in (unprojected) map data

mymap.pro_lakes=spTransform(mymap.unpro_lakes, pro) # Reproject the map
##################################################
##################################################

##################################################
# Folders were the data from radiation is stored
##################################################
mainpath<-"/nobackup/users/dirksen/radiation/data"

output.sum<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".txt",full.names = T)
output.distshore.sum<-list.files(paste0(mainpath,"/Kriging/Daily_distsea/"),pattern = ".txt",full.names = T)
r.kriging<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda",full.names = T)
r.kriging.predictions<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".grd",full.names = T)
r.kriging.dates<-list.files(paste0(mainpath,"/Kriging/Daily/"),pattern = ".rda")
r.kriging.dates<-as.Date(r.kriging.dates,format="ked_exp_model_%Y-%m-%d.rda")
satellitegrids<-list.files(paste0(mainpath,"/Satellite_data/temp/"),pattern=".grd",full.names = T)
# coords.knmi<-readRDS("/nobackup/users/dirksen/radiation/data/coordsKNMI.rda")

output.sum<-fread(output.sum)

coordsRD<-readRDS(paste0(mainpath,"/coordsRDknmi.rda"))

output.sum<-merge(output.sum,coordsRD,by.x=c("x","y"),by.y=c("x","y"))

output.sum<-subset(output.sum,select=c("date","SAT","DS_CODE","x","y","observed","residual","var1.pred","r2","rmse","rmse_sd","me"))

daily.statistics<-fread("/nobackup/users/dirksen/radiation/data/Kriging/statistics_daily.txt")
daily.statistics$t<-as.Date(daily.statistics$t)
#20 year output
##################################################
##################################################
r.siccs.20yearmean<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_siccs/siccs_12year_mean.grd"
r.siccs.20yearsd<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_siccs/siccs_12year_sd.grd"

r.distshore.20yearmean<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_mean_distshore/ked_exp_prediction_distsea.grd"
r.distshore.20yearmean.sum<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_mean_distshore/ked_exp_pointdifference.txt"


r.distshore.20yearsd<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_sd_distshore/ked_exp_prediction_distsea.grd"

r.climatology.20yearmean<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_mean/ked_exp_prediction_siccs.grd"
r.climatology.20yearmean.sum<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_mean/ked_exp_pointdifference.txt"


r.climatology.20yearsd<-"/nobackup/users/dirksen/radiation/data/Kriging/Climatology/12year_sd/ked_exp_prediction_siccs.grd"
##################################################
##################################################

#Quarters
##################################################
##################################################
r.climatology<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarters_mean/",pattern=".grd",full.names=T)
climatology.sum<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarters_mean/",pattern=".txt",full.names = T)
# climatology.sum<-fread(climatology.sum)


r.climatology.sd<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarters_sd/",pattern=".grd",full.names=T)
climatology.sum.sd<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarters_sd/",pattern=".txt",full.names = T)

#Satellite climatology
r.quarters.satellite.mean<-"/nobackup/users/dirksen/radiation/data/Satellite_data/climatology/quarters_mean.rds"
r.quarters.satellite.mean<-readRDS(r.quarters.satellite.mean)

r.quarters.satellite.sd<-"/nobackup/users/dirksen/radiation/data/Satellite_data/climatology/quarters_sd.rds"
r.quarters.satellite.sd<-readRDS(r.quarters.satellite.sd)

#Distsea
r.quarters.distsea.mean<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarters_mean_distshore/",pattern=".grd",full.names=T)
r.quarters.distsea.sd<-list.files("/nobackup/users/dirksen/radiation/data/Kriging/Climatology/Quarter_sd_distshore/",pattern=".grd",full.names=T)
# climatology.sum.sd<-fread(climatology.sum)
##################################################
# Locations were the Temperature data is stored

#To do: rerun the analysis and store in the same way as radiation!
# r.kriging<-list.files("/run/media/dirksen/Elements/Final_R_scripts/R_scripts_HARMONIE/output_HARMONIE/output_int_day/ked_exp_prediction/",pattern=".asc")
#
# r.harmonie<-list.files("/nobackup/users/dirksen/Temperature/Temperature/Data/HARMONIE/",pattern=".grd",full.names = T)
#
#
# r.harmonie.dates<-list.files("/nobackup/users/dirksen/Temperature/Temperature/Data/HARMONIE/",pattern=".grd",full.names = F)
# r.harmonie.dates<-as.Date(r.harmonie.dates,format="file%Y%m%dT123000Z.grd")

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

#Time difference ground obs siccs
library(mgcv)
data.obs<-fread("/nobackup/users/dirksen/radiation/data/sat_over_obs2.csv")
names(data.obs)<-c("IT_DATETIME","DS_CODE","Q","DS_LON","DS_LAT","SAT","DIFF")
data.obs$IT_DATETIME<-as.Date(data.obs$IT_DATETIME)
data.obs$day<-yday(data.obs$IT_DATETIME)
data.obs$month<-month(data.obs$IT_DATETIME)

setkey(data.obs, day)

# summary_by_stationday <-data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME),day=day,DS_CODE)]
summary_by_year_day <- data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME), day)]
summary_by_year_month <- data.obs[, mean(DIFF), by = .(year = year(IT_DATETIME), month)]


#fitting routines
# fit1<-gam(V1 ~ s(year) + s(day, bs = "cc"), data=summary_by_stationday)
# plot(fit1)
fit1 <- gam(V1 ~ s(year) + s(day, bs = "cc"), data = summary_by_year_day)
# plot(fit1)

fit2 <- gam(V1 ~ s(year) + s(month, bs = "cc"), data = summary_by_year_month)
# plot(fit2)

fit <- gam(DIFF ~ s(day, bs="cc"), data = data.obs)
#95% interval
p<-predict(fit,data.obs,se.fit=TRUE)
upr<-p$fit+(2*p$se.fit)
lwr<-p$fit-(2*p$se.fit)
fitted<-data.frame(IT_DATETIME=data.obs$IT_DATETIME,DIFF=fit$fitted.values) # for ggplot

#######


########
function(input, output) {

  ##################################################
  ###################INPUT##########################
  ##################################################
  #20 year data SICCS
  r.siccs.20yr.mean<-reactive({
  r.siccs.20y.m<-raster(r.siccs.20yearmean)
  })

  r.siccs.20yr.sd<-reactive({
    r.siccs.20y.sd<-raster(r.siccs.20yearsd)
  })

  #20 year data interpolation distsea
  r.distsea.20year.mean<-reactive({
    r.dist.20yr.m<-raster(r.distshore.20yearmean)
  })

  r.distsea.20year.sd<-reactive({
    r.dist.20yr.sd<-raster(r.distshore.20yearsd)
  })

  #20 year data interpolation siccs
  r.climatology_20yrm<-reactive({
    r.clim.20yrm<-raster(r.climatology.20yearmean)

  })

  r.climatology_20yrsd<-reactive({
    r.clim.20yrsd<-raster(r.climatology.20yearsd)
  })


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

  #Climatology grid
  climatology.grid<-reactive({
    r.clim<-raster(r.climatology[as.numeric(input$season)])
  })

  climatology.grid.sd<-reactive({
  r.clim.sd<-raster(r.climatology.sd[as.numeric(input$season)])
  })
  #Satellite data for quarters
  siccs.quarters.mean<-reactive({
    siccs.q.m<-r.quarters.satellite.mean[[as.numeric(input$season)]]
  })

  siccs.quarters.sd<-reactive({
    siccs.q.sd<-r.quarters.satellite.sd[[as.numeric(input$season)]]
  })
  #KED with distsea for quarters
  distsea.quarters.mean<-reactive({
    distsea.q.m<-raster(r.quarters.distsea.mean[as.numeric(input$season)])
  })

  distsea.quarters.sd<-reactive({
    distsea.q.sd<-raster(r.quarters.distsea.sd[as.numeric(input$season)])
  })
  # climatology.summary<-reactive({
  #   climatology.sum[climatology.summary[which(climatology.summary$quantile==input$season),]]
  # })
  #
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
  plot.routine<-reactive({
    function(grid,title="no title",color=input$color,rangesx=ranges$x,rangesy=ranges$y){
             # ,lim1=min(siccs.quarters.mean()-10),lim2=max(siccs.quarters.mean)+10){
      gplot(grid) + geom_tile(aes(fill = value)) +
      labs(title=title,x="x-coords",y="y-coord")+
      facet_wrap(~ variable) +
      scale_fill_gradientn(name="W/m2",colours=do.call(color,list(n=50))) + #,limits=c(lim1,lim2)
      # coord_equal()+
      coord_fixed(xlim = rangesx, ylim = rangesy)+
      # coord_cartesian(xlim = ranges$x, ylim = ranges$y)+
      geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
        theme(plot.margin=unit(c(0,0,0,0),"mm"))+
        geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)
    }

  })

  plot.routine.limits.12yr.mean<-reactive({
    function(grid,title="no title",color=input$color,rangesx=ranges$x,rangesy=ranges$y){
      # ,lim1=min(siccs.quarters.mean()-10),lim2=max(siccs.quarters.mean)+10){
      gplot(grid) + geom_tile(aes(fill = value)) +
        labs(title=title,x="x-coords",y="y-coord")+
        facet_wrap(~ variable) +
        scale_fill_gradientn(name="W/m2",colours=do.call(color,list(n=50)),limits=c(110,132)) + #,limits=c(lim1,lim2)
        # coord_equal()+
        coord_fixed(xlim = rangesx, ylim = rangesy)+
        # coord_cartesian(xlim = ranges$x, ylim = ranges$y)+
        geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
        theme(plot.margin=unit(c(0,0,0,0),"mm"))+
        geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)
    }

  })


  # Define starting values for xlim and ylim
  ranges <- reactiveValues(x = c(12621.630033977,278621.630033977), y = c(305583.0457758,620583.0457758))

  #Plotting the prediction, error and variogram fit
  output$krigeplot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(kriging.raster(),xlim=ranges$x, ylim = ranges$y,
         col=do.call(input$color,list(n=50)))
  })
  #########################################
  #########################################
  plot.siccs.clim12yr.mean<-reactive({
    p<-plot.routine.limits.12yr.mean()
    p(r.siccs.20yr.mean(),title="SICCS 12 year mean")
  })

  output$clim12yearmean_siccs<-renderPlot({
    plot.siccs.clim12yr.mean()
  })
  #########################################
  #########################################
  plot.siccs.clim12yr.sd<-reactive({
    p<-plot.routine()
    p(r.siccs.20yr.sd(),title="SICCS 12 year sd")
  })

  output$clim12yearsd_siccs<-renderPlot({
    plot.siccs.clim12yr.sd()
  })

  #########################################
  #########################################

  plot.distsea.clim12yr.m<-reactive({
    p<-plot.routine.limits.12yr.mean()
    p(r.distsea.20year.mean(),title="KED distsea 12 year mean")
  })

  output$clim12year_distsea_mean<-renderPlot({
    plot.distsea.clim12yr.m()
  })
  #########################################
  #########################################
  plot.distsea.clim12yr.sd<-reactive({
    p<-plot.routine()
    p(r.distsea.20year.mean(),title="KED distsea 12 year sd")
  })

  output$clim12year_distsea_sd<-renderPlot({
    plot.distsea.clim12yr.sd()
  })
  #########################################
  #########################################
  plot.clim.12year.mean<-reactive({
    p<-plot.routine.limits.12yr.mean()
    p(r.climatology_20yrm(),title="KED siccs 12 year mean")
  })
  output$clim12yearmean_ked_siccs<-renderPlot({
    plot.clim.12year.mean()
  })
  #########################################
  #########################################
  plot.clim.12year.sd<-reactive({
  p<-plot.routine()
  p(r.climatology_20yrsd(),title="KED siccs 12 year sd")
  })

  output$clim12yearsd_ked_siccs<-renderPlot({
    plot.clim.12year.sd()
  })
  #########################################
  #########################################
  observeEvent(input$saveclim,{
    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/siccs_clim12year_mean.png",
                       plot=plot.siccs.clim12yr.mean())

    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/siccs_clim12year_sd.png"
                       ,plot=plot.siccs.clim12yr.sd())

    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/ked_distshore_clim12year_mean.png",
           plot=plot.distsea.clim12yr.m())

    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/ked_distshore_clim12year_sd.png"
           ,plot=plot.distsea.clim12yr.sd())

    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/ked_siccs_clim12year_mean.png",
           plot=plot.clim.12year.mean())

    ggsave(file="/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/ked_siccs_clim12year_sd.png"
           ,plot=plot.clim.12year.sd())
  })
  #########################################
  #########################################
  plot.seasons.mean.distshore<-reactive({
    p<-plot.routine()
    p(distsea.quarters.mean(),title=paste0("KED distsea mean Q",input$season))
  })

  output$seasons.mean.dist<-renderPlot({
    plot.seasons.mean.distshore()
  })
  #########################################
  #########################################
  plot.seasons.sd.distshore<-reactive({
    p<-plot.routine()
    p(distsea.quarters.sd(),title=paste0("KED distsea sd for Q",input$season))
  })

  output$seasons.sd.dist<-renderPlot({
    plot.seasons.sd.distshore()
  })
  #########################################
  #########################################
  plot.seasons.mean<-reactive({
    p<-plot.routine()
    p(climatology.grid(),title=paste0("KED SICCS mean for Q",input$season))
   })
  output$seasons<- renderPlot({
    plot.seasons.mean()
    })
  #########################################
  #########################################
   plot.seasons.sd<-reactive({
    p<-plot.routine()
    p(climatology.grid.sd(),title=paste0("KED SICCS sd for Q",input$season))
    })

  output$seasons.sd<- renderPlot({
    plot.seasons.sd()
    })
  #########################################
  #########################################
  plot.siccs.seasons.sd<-reactive({
    p<-plot.routine()
    p(grid=siccs.quarters.sd(),title=paste0("SICCS sd for Q",input$season))
  })

  output$seasons.sd.siccs<- renderPlot({
    plot.siccs.seasons.sd()
  })
  #########################################
  #########################################
  plot.siccs.seasons.mean<-reactive({
    p<-plot.routine()
    p(siccs.quarters.mean(),title=paste0("SICCS mean for Q",input$season))
    })
  output$seasons.siccs<- renderPlot({
    plot.siccs.seasons.mean()
  })
  #########################################
  #########################################
  observeEvent(input$savemean,{
    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_distshore_mean_Q",
                       input$season,".png"),plot=plot.seasons.mean.distshore())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_distshore_sd_Q",
                       input$season,".png"),plot=plot.seasons.sd.distshore())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_mean_Q",
                       input$season,".png"),plot=plot.seasons.mean())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_sd_Q",
                       input$season,".png"),plot=plot.seasons.sd())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_siccs_sd_Q",
                       input$season,".png"),plot=plot.siccs.seasons.sd())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/seasons_mean_Q",
                       input$season,".png"),plot=plot.siccs.seasons.mean())
  })
  #########################################
  #########################################
  output$slider <- renderUI({
    sliderInput("slider","slider", min   = 10, 
                max   = 150,
                value = c(10,250)
    )})
  
  # zranges<-reactiveValues(zmin=10,zmax=150)
  # 
  # zranges<-reactive({
  #   if(!is.null(input$slider)){
  #   zranges$zmin<-input$slider[1]
  #   zranges$zmax<-input$slider[2]
  #   } else {
  #     zranges$zmin=10
  #     zranges$zmax=150
  #   }
  #   
  # })
  
  plot.daily.mean<-reactive({
    function(grid,title){
      gplot(grid) + geom_tile(aes(fill = value)) +
      labs(title=title,x="x-coords",y="y-coord")+
      facet_wrap(~ variable) +
      scale_fill_gradientn(name="W/m2",colours=do.call(input$color,list(n=50)),limits=c(input$slider[1],input$slider[2])) +
      # coord_equal()+
      coord_fixed(xlim = ranges$x, ylim = ranges$y)+
      geom_polygon(data=mymap.pro[mymap.pro$NAME_SORT  %in% c('Netherlands'),],aes(x=long,y=lat,group=group),color="blue",alpha=0)+
      geom_polygon(data=mymap.pro_lakes,aes(x=long,y=lat,group=group),color="blue",alpha=0)+
      theme(plot.margin=unit(c(0,0,0,0),"mm"))+
      geom_point(data=spatialpoints.data(),aes(x=x,y=y,fill=observed),pch=21,size=2,color="grey20")
    }
  })
  #########################################
  #########################################
  plot.kriging.day<-reactive({
    p<-plot.daily.mean()
    p(kriging.prediction(),title="Kriging Prediction")
  })

  output$zoomplot <- renderPlot({
    plot.kriging.day()
      })
  #########################################
  #########################################
  plot.siccs.day<-reactive({
    p<-plot.daily.mean()
    p(satellite.grid(),title="SICCS")
  })
  output$satellite<-renderPlot({
    plot.siccs.day()
    })
  #########################################
  #########################################
  observeEvent(input$saveday,{
    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/kriging_prediction",
                       input$date,".png"),plot=plot.kriging.day())

    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/siccs",
                       input$date,".png"),plot=plot.siccs.day())
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
  ranges3 <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$zoomplot_dblclick2,{
    brush <- input$plot3_brush
    if (!is.null(brush)) {
      ranges3$x <- c(as.Date(brush$xmin,origin = "1970-01-01"), as.Date(brush$xmax, origin = "1970-01-01"))
      ranges3$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges3$x <- NULL
      ranges3$y <- NULL
    }
  })

  output$infobrush2<-renderPrint({
    if(is.null(input$plot3_brush)) return(NULL)
    brushedPoints(data.obs,input$plot3_brush)
  })

  plot.timediff<-reactive({
    ggplot(data.obs,aes(IT_DATETIME,DIFF))+
      geom_point(size=1)+
      geom_point(data=fitted,colour="yellow",size=1)+
      labs(x="time [years]",y="SICCS - ground-based observations solar irradiance [W/m2]")+
      coord_cartesian(xlim = ranges3$x, ylim = ranges3$y)
  })


ranges4 <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$zoomplot_dblclick3,{
    brush <- input$plot4_brush
    if (!is.null(brush)) {
      ranges4$x <- c(as.Date(brush$xmin,origin = "1970-01-01"), as.Date(brush$xmax, origin = "1970-01-01"))
      ranges4$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges4$x <- NULL
      ranges4$y <- NULL
    }
  })
rmse.sea<-data.frame(t=daily.statistics$t,rmse.sat=daily.statistics$rmse.kedsea) # for ggplot
rmse.siccs<-data.frame(t=daily.statistics$t,rmse.sat=daily.statistics$rmse.kedsat) # for ggplot

plot.statistics<-reactive({
    ggplot(daily.statistics,aes(t,rmse.sat))+
      geom_point(colour="green",size=1,aes(label="SICCS"))+
      geom_point(data=rmse.sea,colour="blue",size=1,aes(label="KED sea"))+
      geom_point(data=rmse.siccs,colour="red",size=1,aes(label="KED SICCS"))+
      labs(x="time [years]",y="RMSE")+
    coord_cartesian(xlim = ranges4$x, ylim = ranges4$y)
  })

output$statistics<-renderPlot({
  plot.statistics()
})

  output$timediff<-renderPlot({
plot.timediff()

  })

  observeEvent(input$savetimediff,{
    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/difference_time",
                       ranges3$x[1],"_",ranges3$x[2],".png"),plot=plot.timediff())
    ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/RMSE_time",
                       ranges4$x[1],"_",ranges4$x[2],".png"),plot=plot.statistics())
    # ggsave(file=paste0("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/fig/difference_time_",
    #                    ranges3$x,"_",ranges3$y,".png"),plot=plot.timediff())
  })

}

