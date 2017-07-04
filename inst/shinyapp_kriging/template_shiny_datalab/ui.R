# runApp("/nobackup/users/dirksen/radiation/inst/shinyapp_kriging/demo_datalab/", launch.browser = TRUE)
#.libPaths("/nobackup/users/dirksen/R/x86_64-redhat-linux-gnu-library/3.3/")
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
  p("Add a description of your app and what you can change. You can add figures with img(scr='name.jpg'). The 
    figures should be stored in a seperate folder named www"),
  img(src="pyranometer_field.jpg",height=150),

  sidebarMenu(
  menuItem("Plots",tabName="Plots",icon = icon("dashboard")
  ),
  menuItem("Background",tabName="Background",icon = icon("th"),
           menuSubItem("Introduction",tabName = "Intro"),
           menuSubItem("Details",tabName = "Details")
  ))
 
),
dashboardBody(
  
  tabItems(
    tabItem(tabName="Plots",
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
  
 
  ))
  ),
  tabItem(tabName="Intro",
          h3("Introduction to your research"),
          p("You can add for example a Markdown file. Add to the header of your markdown 'runtime: shiny'
            ."),
          includeMarkdown("example.Rmd")
          ),
  tabItem(tabName = "Details",
          h3("Details"),
          wellPanel(helpText(a("Everything you need to know about shiny dashboard",href="https://rstudio.github.io/shinydashboard/index.html",target="_blank")))
          
          )
  ))

)


