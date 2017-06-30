# runApp("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/demo_datalab/", launch.browser = TRUE)
.libPaths("/nobackup/users/dirksen/R/x86_64-redhat-linux-gnu-library/3.3/")
library(leaflet)
library(shinydashboard)
library(shiny)
library(DT)


ui<-dashboardPage(  
dashboardHeader(
  title = div(img(src="datalab_logo.jpg",height=50,class="pull-left"),"Solar Radiation")
  
                        
                
                ),
dashboardSidebar(
  h3("Get Started!"),
  p("Solar radiation is measured at 32 AWS locations in the Netherlands, find out what they measure for each day by changing the date. 
    Can you find differences between the grid and the measurements?"),
  img(src="pyranometer_field.jpg",height=150),
  h3("The Grid"),
  p("The layer in the 
    background is a ground-measurement kriging interpolation with a satellite product (SICCS from MSG) as external drift."),
  img(src="MSG_Auto22.jpeg",height=145)
  
  
),
dashboardBody(
  
  fluidPage(
    fluidRow(
    # class = "greedy",
    # div(class = "col-xs-4",style="background-color: blue",body="height:1500}",
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map"),
        absolutePanel(top = 100, right = 50,
                      wellPanel(
                        dateInput('date',"Enter Date (yyyy-mm-dd)",
                                  min=as.Date("2004-01-19"),max=as.Date("2016-08-08"),
                                  value=as.Date("2015-07-07"))))
  
 
  ))#)
# fluidRow(wellPanel(
#   dateInput('date',"Enter Date (yyyy-mm-dd)",
#             min=as.Date("2004-01-19"),max=as.Date("2016-08-08"),
#             value=as.Date("2015-07-07"))
# )),
# fluidRow(
#   leafletOutput("map")
# )
  )

)


