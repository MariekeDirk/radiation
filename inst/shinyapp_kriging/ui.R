library(shiny)
coordsRD<-readRDS("/nobackup/users/dirksen/radiation/data/coordsRDknmi.rda")

#Up next: change the page into different tabs
#example: https://shiny.rstudio.com/gallery/datatables-options.html 
#navbarPage("Solar Radiation explorer",
#tabPanel("Interactive Maps",fluidRow,...)
#tabPanel("")

#Old page settings
#fluidPage(
#  titlePanel('Solar Radiation [W/m2]'),
############################################## 
############################################## 
############################################## 

navbarPage("Solar Radiation explorer",
           
##############################################     
############################################## 
############################################## 
tabPanel("Climatology grids",
         fluidRow(
           actionButton('saveclim',"save all figures")
         ),
         fluidRow(
           column(6,plotOutput("clim12yearmean_ked_siccs")),
           column(6,plotOutput("clim12yearsd_ked_siccs"))
         ),
         fluidRow(
           column(6,plotOutput("clim12yearmean_siccs")),
           column(6,plotOutput("clim12yearsd_siccs"))
         ),
         fluidRow(
           column(6,plotOutput("clim12year_distsea_mean")),
           column(6,plotOutput("clim12year_distsea_sd"))
         )
         ),      

tabPanel("Climatology Quarters",
               fluidRow(
                 column(4,
               selectInput('season','Season',choices = c('Q1'="1",
                                                          'Q2'="2",
                                                          'Q3'="3",
                                                          'Q4'="4"))
                 ),
               column(4,
                      actionButton('savemean',"save all figures")
                    
                      )),
                         
              fluidRow(
                  column(6,plotOutput("seasons")),
                  column(6,plotOutput("seasons.sd"))
                      ),
              fluidRow(
                column(6,plotOutput("seasons.siccs")),
                column(6,plotOutput("seasons.sd.siccs"))
              ),
              fluidRow(
                column(6,plotOutput("seasons.mean.dist")),
                column(6,plotOutput("seasons.sd.dist"))
              )
               
      ),
      tabPanel("Interactive Maps",          
  fluidRow(
  column(4,
         #dateInput('date', 'date', min=as.Date("2004-01-19"),max=as.Date("2016-08-08"),value=as.Date("2015-07-07"))
         wellPanel(
           dateInput('date',"Enter Date (yyyy-mm-dd)",
                     min=as.Date("2004-01-19"),max=as.Date("2016-08-08"),
                     value=as.Date("2015-07-07"))
         )),
          
  column(4,       
  selectInput('color','Color Palette',
                        choices = c('Terrain'="terrain.colors",
                                    'Topo'="topo.colors",
                                    'Heat'="heat.colors",
                                    'Rainbow'="rainbow",
                                    'cm'="cm.colors"))
  ),
  column(4,
         actionButton('saveday',"save the day"),
         sliderInput("slider","slider",min=10,max=150,value=c(10,250))
         )

         ),

  fluidRow(
    column(6,
           h4("Kriging Prediction"),
           plotOutput('zoomplot', click = "plot_click",dblclick = 'zoomplot_dblclick',
                      brush = brushOpts(
                        id = "zoomplot_brush",
                        resetOnNew = TRUE
                      )),
           verbatimTextOutput("info")),
    # ),
    # br(),
    column(6,
           h4("SICCS"),
           plotOutput('satellite',click = "plot_click",dblclick = 'zoomplot_dblclick',
                      brush = brushOpts(
                        id = "zoomplot_brush",
                        resetOnNew = TRUE
                      )),
          verbatimTextOutput("infosat"))
    )
           ), 


############################################## 
############################################## 
############################################## 

  tabPanel("Data explorer",   
    fluidRow(
      # column(4,
      #        selectInput('xvar','x-variable',
      #                    list("observed"="observed",
      #                         "satellite"="SAT",
      #                         "predicted"="var1.pred",
      #                         "R2"="r2",
      #                         "RMSE"="RMSE")),
      #        selectInput('yvar','y-variable',
      #                    list("satellite"="SAT",
      #                         "observed"="observed",
      #                         "predicted"="var1.pred",
      #                         "R2"="r2",
      #                         "RMSE"="RMSE"))),
      column(3,
             selectInput('pointcolor','color',
                         choices = c("blue"="blue","black"="black","red"="red","yellow"="yellow")),
    
             selectInput('pointsize','size',
                         choices=c("1"="1","2"="2","3"="3","4"="4"))
      ),
      column(4,selectInput('statistics',"Summary all stations",
                  choices=c('Mean'="mean",
                            "Standard Deviation"="sd",
                            "Quantiles"="quantile")),
             selectInput('stations',"Select Station",
                                choices = coordsRD$DS_CODE)
                    
             )
      ),

      
    
           
    fluidRow(
             column(6,
           h4("Ground observation vs. SICCS"),
           plotOutput("obsvsmeas",
                        brush = brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE))),
           column(6,h4("Brush the left plot for zoom"),
           plotOutput("plot3"))
           # plotOutput("obsvsmeas",click="point_click",
           #            brush = brushOpts(
           #              id = "point_brush"))
           
           ),
           fluidRow(
    column(12,
           # h4("Point info"),
           # verbatimTextOutput("infopoint"),
           h4("Brushed info"),
           verbatimTextOutput("infobrush")
    )
    ),
    fluidRow(
      column(12,
             h4("Summary all stations"),
             tableOutput('summary'),       
             h4("Summary Station"),
             tableOutput('summary.station'))
    )
  ),
############################################## 
############################################## 
############################################## 
tabPanel("Data time explorer",
         fluidRow(
           actionButton("savetimediff","save plot")
         ),
         fluidRow(
           column(12,
                  h4("Difference between two products with time"),
                  plotOutput("timediff",dblclick = 'zoomplot_dblclick2',brush = brushOpts(
                    id = "plot3_brush",
                    resetOnNew = TRUE)),
                  verbatimTextOutput("infobrush2")
                  ),
           column(12,
                  h4("Difference between two products with time"),
                  plotOutput("statistics",dblclick = 'zoomplot_dblclick3',brush = brushOpts(
                    id = "plot4_brush",
                    resetOnNew = TRUE))
                  #verbatimTextOutput("infobrush2")
           )
         )
         ),
tabPanel("Variogram fit",
         fluidRow(
           column(6,
                  h4("Kriging Interpolation"),
                  plotOutput('krigeplot')
           ))
)
)


    # h4("summary"),
    # tableoutput("summary")
    # 









#plot with the kriging prediction and zoom function? https://shiny.rstudio.com/gallery/plot-interaction-zoom.html

#another interactive example: http://davesteps.com/geoExploreR/

#Point click: https://shiny.rstudio.com/articles/plot-interaction.html 
# ui <- basicPage(
#   plotOutput("plot1", click = "plot_click"),
#   verbatimTextOutput("info")
# )
# 
# server <- function(input, output) {
#   output$plot1 <- renderPlot({
#     plot(mtcars$wt, mtcars$mpg)
#   })
#   
#   output$info <- renderText({
#     paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
#   })
# }