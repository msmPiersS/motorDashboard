###############################################################################
## Interactive Shiny based app for exploring MSM car insurance channel performance 
## 
## front end : ui.r
##
## See server.R code for more info
##  
## ps December 2015
###############################################################################

###############################################################################
## todo
## see server.R
## 
###############################################################################



###############################################################################
## setup

library(shiny)

## end setup
###############################################################################


###############################################################################
## UI

shinyUI(
  # tab style UI
  navbarPage("Interactive Car Insurance Trends dashboard - MSM",  
            inverse = TRUE,
            header = 
              tags$head(
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'http://fonts.googleapis.com/css?family=Yanone+Kaffeesatz:400,700')
                #tags$link(rel = 'stylesheet', type = 'text/css', href = 'http://www.moneysupermarket.com/content-management/css/base-framework.css')
              ),
  
  # First Tab
  tabPanel("Top Line Trends",
           
    # Application title again
    titlePanel(
      HTML(
        '<div id="stats_header">
        Interactive dashboard to explore performance of car insurance channel at MSM
        <a href="https://www.moneysupermarket.com/car-insurance/" target="_blank">
        <img id="stats_logo" align="right" alt="SNAP Logo" src="http://www.googledrive.com/host/0B8-urh68sOqfc0xieXVnNDBTdDQ" />
        </a>
        </div>'
      )
    ),
    
    div("Interactive car insurance dashboard", style = "font-size:140%"),
    div("Use first graph to explore top-line trends: use list on left to select metrics to view", style = "font-size:120%"),
    div("Explore trends in customer mix using second graph", style = "font-size:120%"),
    div("Identify key micro segments using third chart", style = "font-size:120%"),
    br(),
  
    fluidPage(  
        fluidRow(
          column(4,
                 fluidRow(
                   column(6,
                          h5(' Select metrics to display '),
                          checkboxGroupInput("displayMetric", "Display Metrics:",
                                             c("Total Market" = "ravg_carMarket",
                                               "Total Site Sessions" = "ravg_totalSessions", 
                                               "Car Chanel Sessions" = "ravg_carSessions",
                                               "Car Enquiries" = "ravg_carEnquries",
                                               "Car Clickers" = "ravg_carClickers",
                                               "Car Buyers" = "ravg_carBuyers", 
                                               "Car Revenue" = "ravg_carRev"), 
                                             selected = "ravg_carMarket")),
                  column(6,
                          h5(' Months to use in averaging '),
                          sliderInput("rollM", "Number of months in avg:", 3, min = 1, max = 12, step = 1, ticks = FALSE))
                 )
          ),
          column(8,
                 h5(' Top Line Trends '),
                 plotOutput("topLinePlot")
          )
        ),
        br(),
        hr()
        #helpText(htmlOutput("timeHelp")),
    )
  ),
  # Second Tab
  tabPanel("Explore Changes",
           
           # Application title again
           titlePanel(
             HTML(
               '<div id="stats_header">
               Interactive dashboard to explore performance of car insurance channel at MSM
               <a href="https://www.moneysupermarket.com/car-insurance/" target="_blank">
               <img id="stats_logo" align="right" alt="SNAP Logo" src="http://www.googledrive.com/host/0B8-urh68sOqfc0xieXVnNDBTdDQ" />
               </a>
               </div>'
             )
             ),
           
           div("Interactive car insurance dashboard", style = "font-size:140%"),
           div("Explore trends in customer mix using second graph", style = "font-size:120%"),
           br(),
           
           fluidPage(  
             fluidRow(
               column(4,
                      fluidRow(
                        column(12,
                               h5(' Dimension to explore '),
                               uiOutput("chooseDim"))
                      )
               ),
               column(12,
                      h5(' Exploring trend drivers '),
                      plotOutput("dimensionPlot")
                      
               )
             ),
             br(),
             hr()
             #helpText(htmlOutput("timeHelp")),
           )
    ) , 
  # Third Tab
  tabPanel("Age/Gender Segments",
           
           # Application title again
           titlePanel(
             HTML(
               '<div id="stats_header">
               Interactive dashboard to explore performance of car insurance channel at MSM
               <a href="https://www.moneysupermarket.com/car-insurance/" target="_blank">
               <img id="stats_logo" align="right" alt="SNAP Logo" src="http://www.googledrive.com/host/0B8-urh68sOqfc0xieXVnNDBTdDQ" />
               </a>
               </div>'
             )
             ),
           
           div("Interactive car insurance dashboard", style = "font-size:140%"),
           div("Explore drivers of change in age/gender segments", style = "font-size:120%"),
           br(),
           
           fluidPage(  
             fluidRow(
               column(12,
                      h5(' Top Level - overall view '),
                      plotOutput("waterfallPlotA")
               ),
               column(12,
                      h5(' Age/Gender segments - overall view '),
                      plotOutput("waterfallPlotD")
                      
               ),
               column(12,
                      h5(' Top Level - change view '),
                      plotOutput("waterfallPlotAC")
               ), 
               column(12,
                      h5(' Age/Gender segments - change view '),
                      plotOutput("waterfallPlotDC")
                      
               )
             ),
             br(),
             hr()
             #helpText(htmlOutput("timeHelp")),
           )
          ) , 
  # Fourth Tab
  tabPanel("Explore Micro Segments",
           
           # Application title again
           titlePanel(
             HTML(
               '<div id="stats_header">
               Interactive dashboard to explore performance of car insurance channel at MSM
               <a href="https://www.moneysupermarket.com/car-insurance/" target="_blank">
               </a>
               </div>'
             )
             ),
           
           div("Interactive car insurance dashboard", style = "font-size:140%"),
           div("Explore micro segments", style = "font-size:120%"),
           br(),
           
           fluidPage(  
             fluidRow(
               includeHTML("parcor/parcorCar.html")
               ),
             br(),
             hr()
             #helpText(htmlOutput("timeHelp")),
           )
    ) 
))


## end UI
###############################################################################
