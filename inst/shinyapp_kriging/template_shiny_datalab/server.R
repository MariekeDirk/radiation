#.libPaths("/nobackup/users/dirksen/R/x86_64-redhat-linux-gnu-library/3.3/")
#runApp("./inst/shinyapp_kriging/template_shiny_datalab/", launch.browser = TRUE)
library(leaflet)
library(shiny)

xmin<-3.041776
xmax<-7.444776
ymin<-50.536674
ymax<-53.737374

server<-function(input,output,server){

  
  output$map<-renderLeaflet({
    leaflet() %>% 
      fitBounds(xmin,ymin,xmax,ymax) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas
                       ) 
  })

  
}