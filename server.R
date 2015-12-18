###############################################################################
## Interactive Shiny based app for exploring MSM car insurance channel performance  
## 
## shiny server : server.r
##
## Simple interactive dashboard to explore car insurance performance
##
## Data Sources:
## For Topline analysis- 
## ebenchmarkers for total market
## product team/google analytics for total site sessions and car channel sessions
## ADM (ADM_VEH.ADM_VEH_ENQUIRY) for total buyers and enquiries
## finance for revenue 
##
## For segment analysis
## ADM (ADM_VEH.ADM_VEH_ENQUIRY) - simple monthly aggregatation script used (proc sql)
##
## Data loaded from saved r data files- pre-procesed using motorProcess.r script
##
## ps Dec 2015
###############################################################################

#############################
## todo
## 
##
#############################



###############################################################################
## setup

  library(data.table)
  library(ggplot2)
  library(zoo)
  #library(RColorBrewer)
  #library(shiny)

  #setwd("C:/Piers/git/r_abm")
  options(shiny.trace=FALSE) # set to True to debug
  #options(shiny.error=browser) # set error to browser
  options(shiny.maxRequestSize=100*1024^2) # set max input file size to 100MB

  #load data
  load(file='topLine.rdata')
  
  
## end setup
###############################################################################


###############################################################################
## recursive section

shinyServer(function(input, output, session) {
 
  #do everything in an observer
  obs = observe({

      print(head(overall))
    
      #get target metrics from input
      displayMetricClean <- reactive({
        if (is.null(input$displayMetric))
          "carMarket"
        else
          input$displayMetric
      })
      

        
      #get target rolling average from input
      rollMClean <- reactive({
        if (is.null(input$rollM))
          3
        else
          input$rollM
      })
      

      
      #create rolling means
      #first identify nas for future
      naIdx = is.na(overall)
      #then fill in nas with nearest value
      overall = as.data.table(na.fill(overall, "extend"))
      overall[, ym := as.factor(ym)]
      overallNames = setdiff(colnames(overall), 'ym')
      
       
      #rolling periods
      rollM = rollMClean()
      print(overallNames)
      print(displayMetricClean())
      overall[, paste0("ravg_", overallNames) := lapply(.SD, rollapply, width = rollM, mean, fill = NA, align ='right'), .SDcols = overallNames]

       
      overallIdx = 100*overall[, !"ym", with=FALSE] / overall[rep(which(overall[, ym] == '201301'), nrow(overall)), !"ym", with=FALSE]
      overallIdx[, ym := overall[, ym]]
      
      #set up for plotting
      overallNamesAll = setdiff(colnames(overallIdx), 'ym')
      overallNamesAvg = overallNamesAll[grep('ravg_', overallNamesAll)]
      overallNamesRaw = overallNamesAll[grep('ravg_', overallNamesAll,invert = TRUE)]
      overallMonths = levels(overallIdx[, ym])
      
      overallPlot = melt(overallIdx, id = 'ym', measure = overallNamesAll)
      cc =data.table(colours =  scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(overallNamesRaw))))
      cc =data.table(colours =  overallNamesRaw)
      cc[, variable := overallNamesRaw]
      cc = rbindlist(list(cc, cc))
      cc[ (nrow(cc)-length(overallNamesRaw)+1):nrow(cc), variable:=overallNamesAvg]
      setkey(cc, variable)
      setkey(overallPlot, variable)
      overallPlot = overallPlot[cc]
      
      output$topLinePlot <- renderPlot({
        p = ggplot(overallPlot, aes(x=ym, y=value, group = variable, colour = colours)) +
          geom_line(data = overallPlot[variable %in% displayMetricClean(), ], size = 0.5) + 
          scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
          scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        print(p)
      })
      
      
      
      
      
      #isolate ({
      #output$seqInput <- renderUI({
      #  sliderInput("seqId", "Sequence Id:", seqIdClean(), min = 1, max = sessionLength, step=1, ticks = FALSE, 
      #              animate = animationOptions(loop = FALSE, interval = 500))
      #})
      #
      #}) # end isolate  
      
      
      #get target number of recommendations from inpu
      #nRecClean <- reactive({
      #  if (is.null(input$nRec))
      #    1
      #  else
      #    input$nRec
      #}) 
      

    
    #load pages and recommendations
    #i=seqIdClean()
    #tgtUrl = paste("http://",pageLookup[pId %in% tgtPages[i], pagePath], sep="")
    #output$currentPage<-renderUI({tgtUrl})
    #output$pageView<-renderUI({
    #  x <- tgtUrl 
    #  getPage(tgtUrl)
    #})
    
    #inPages = tgtPages[1:i]
    #inPageNames = pageLookup[pId %in% inPages, pagePath]
    #recs = getNRecs(cleanDedup, inPages, pageLookup, nRecClean(), 10)
    #
    #output$recPages<-renderUI({ paste(recs[, pagePath], sep=" ", collapse = "\n") })
    #output$topPages<-renderUI({ paste(pageLookup[!(pId %in% unique(c(inPages, recs[, pId]))), ] [1:nRecClean(), pagePath], sep=" ", collapse = "\n") })
    #output$currentPages<-renderUI({ paste(inPageNames, sep=" ", collapse = "\n") })
    
      
  }) #end observe
}) #end

  



## end recursive section
###############################################################################
