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
## tidy up and make pretty
## remove outliers and put limits on charts
## allow for different index month
## make top line inputs reactive
##
## parcor
## join up to third tab
## limit/round
## normalise/cap outliers - fix colours
## figure out table
## rotate labels
##
#############################



###############################################################################
## setup

  library(data.table)
  library(ggplot2)
  library(gridExtra)
  library(zoo)
  library(reshape2)
  library(scales)
  #library(RColorBrewer)
  #library(shiny)

  #setwd("C:/Piers/git/r_abm")
  options(shiny.trace=FALSE) # set to True to debug
  #options(shiny.error=browser) # set error to browser
  options(shiny.maxRequestSize=100*1024^2) # set max input file size to 100MB

  #load data
  load(file='carDashboard.rdata')
  load(file='carParcor.rdata')
  
  tgtMnth = '201301'
  excludeMonths = c('201512')
  parCorQ1 = '201304'
  parCorQ2 = '201504'
  
## end setup
###############################################################################


###############################################################################
## recursive section

shinyServer(function(input, output, session) {
 
  #do everything in an observer
  obs = observe({

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
      

    #### Top Line Section
      
      #create rolling means
      #first identify nas for future
      naIdx = is.na(overall)
      #then fill in nas with nearest value
      overall = as.data.table(na.fill(overall, "extend"))
      overall[, ym := as.factor(ym)]
      overallNames = setdiff(colnames(overall), 'ym')
      
      #rolling periods
      rollM = rollMClean()
      #print(overallNames)
      #print(displayMetricClean())
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
      
      
    #### Explore Dimension section
      #pull in m1 for each series for indexing
      tmp = agg[ym==tgtMnth, .(var, totEnq)]
      setkey(tmp, var)
      setkey(agg, var)
      aggPlot = agg[tmp]
      setnames(aggPlot, "i.totEnq", paste("totEnq",tgtMnth, sep=""))
      #calcualte index
      aggPlot[, enqIndex:= 100*totEnq/aggPlot[, paste("totEnq",tgtMnth, sep=""), with=FALSE]]
      
      #pull in month total for each series for share
      tmp = aggPlot[, .(totVol = sum(totEnq)), by = .(ym, type)]
      setkeyv(tmp, c('ym', 'type'))
      setkeyv(aggPlot, c('ym', 'type'))
      aggPlot = aggPlot[tmp]
      #calculate share
      aggPlot[, enqShare:= totEnq/totVol]
      #clean up
      aggPlot[, ym:=as.factor(ym)]
      aggPlot[ym!=excludeMonths, ]
      #aggPlot = aggPlot[ym!='201512', ]
      #now have volume index, share and conversion figures to look at
      
      #get target dimension from input
      tgtDimClean <- reactive({
        if (is.null(input$dimensions))
          typeList[2]
        else
          input$dimensions
      })
      
      #create input selections
      typeList = unique(aggPlot[, type])
      output$chooseDim <- renderUI({
          
          # Create the checkboxes and select them all by default
          checkboxGroupInput("dimensions", "Choose Dimensions", 
                             choices  = typeList,
                             selected = tgtDimClean(), 
                             inline = TRUE,
                             width = '100%')
      })
      
      ##dimension plots  
      #tgtType = "drivers"
      #tgtType = tgtDimClean()
      tgtType = input$dimensions
      
      output$dimensionPlot <- renderPlot({
          
          #market and sales
          p1 = ggplot(overallPlot[ variable %in% c('ravg_carMarket', 'ravg_carRev'), ], aes(x=ym, y=value, group = variable, colour = variable)) +
                  geom_line(data = overallPlot[variable %in% c('ravg_carMarket', 'ravg_carRev'), ], size = 0.5) + 
                  scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
                  scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  ggtitle(paste("Overall Trend: base = ", tgtMnth, sep=""))
              
          #volume
          p2 = ggplot(aggPlot[type==tgtType, ], aes(x=ym, y=enqIndex, group = var, colour = var)) +
                  geom_line() +
                  scale_y_continuous(name = "enquiry index", breaks = c(seq(0,200,10))) + 
                  scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  ggtitle(paste("Volume Index trend: base = ", tgtMnth, sep=""))
                
          #share
          p3 = ggplot(aggPlot[type==tgtType, ], aes(x=ym, y=enqShare, group = var, colour = var)) +
                  geom_line() +
                  scale_y_continuous(name = "enquiry share", breaks = c(seq(0,1,0.05)), labels = percent) + 
                  scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  ggtitle("Volume Share trend")
                
          
          #overall conversion
          p4 = ggplot(aggPlot[type==tgtType, ], aes(x=ym, y=enqToSale, group = var, colour = var)) +
                  geom_line() +
                  scale_y_continuous(name = "enquiry to sale", breaks = c(seq(0,0.2,0.01)), labels = percent) + 
                  scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  ggtitle("Enquiry to Sale Conversion trend")
                
          
          #enquiry to click
          p5 = ggplot(aggPlot[type==tgtType, ], aes(x=ym, y=enqToClick, group = var, colour = var)) +
                  geom_line() +
                  scale_y_continuous(name = "enquiry to click", breaks = c(seq(0,0.2,0.01)), labels = percent) + 
                  scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  ggtitle("Enquiry to Click Conversion trend")
                
          #click to sale
          p6 = ggplot(aggPlot[type==tgtType, ], aes(x=ym, y=clickToSale, group = var, colour = var)) +
                geom_line() +
                scale_y_continuous(name = "click to sale", breaks = c(seq(0,0.2,0.01)), labels = percent) + 
                scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                ggtitle("Click to Sale Conversion trend")
              
          pFinal = grid.arrange(p1,p2,p3,p4,p5,p6)
          
          
          print(pFinal)
      })
      
      
    #### Parcor section
      #clean up
      finalQ[ is.nan(transPerSale), transPerSale:=1]
      finalQ[ is.nan(avgMonthlyEnq), avgMonthlyEnq:=0]
      finalQ[ is.nan(enqToSale), enqToSale:=0]
      finalQ[ is.nan(enqToClick), enqToClick:=0]
      finalQ[ is.nan(clicksPerCLicker), clicksPerCLicker:=0]
      finalQ[ is.nan(clickToSale), clickToSale:=0]
      
      parcorData1 = finalQ[ yq %in% c(parCorQ1), ]
      parcorData2 = finalQ[ yq %in% c(parCorQ2), ]
      setkey(parcorData1, segId)
      setkey(parcorData2, segId)
      parcorOut = parcorData2[parcorData1]
      parcorOut[, 'PctCh-Enq':=avgMonthlyEnq/i.avgMonthlyEnq - 1, with=FALSE]
      parcorOut[, 'Ch-enqToSale':= enqToSale - i.enqToSale, with=FALSE]
      parcorOut[, 'Ch-enqToClick':= enqToClick - i.enqToClick, with=FALSE]
      parcorOut[, 'Ch-ClicksPerClicker':= clicksPerCLicker - i.clicksPerCLicker, with=FALSE]
      parcorOut[, 'Ch-clickToSale' := clickToSale - i.clickToSale, with=FALSE]
      parcorOut[, 'Ch-okResults' := pctOkResults - i.pctOkResults, with=FALSE]
      setnames(parcorOut, finalMetricsList, paste(parCorQ2,"-",finalMetricsList, sep=""))
      parcorOut[, colnames(parcorOut)[grep("i\\.", colnames(parcorOut))]:=NULL]  
      
      outCols = setdiff(colnames(parcorOut), c('yq'))
      outCols = outCols[grep('-transPerSale',outCols, invert=TRUE)]
      outMetrics = outCols[grep('-',outCols)]
      volMetrics = outMetrics[1]
      pctMetrics = setdiff(outMetrics, volMetrics)
      parcorOut[,(volMetrics) := round(.SD,0), .SDcols=volMetrics]
      parcorOut[,(pctMetrics) := round(.SD,4), .SDcols=pctMetrics]
      
      
      write.table(parcorOut[, outCols, with=FALSE], file="parcor/data/carParcorData.csv", sep=",", row.names = F, col.names = T)
      #write.table(parcorOut[sample(1:nrow(parcorOut),100), outCols, with=FALSE], file="parcor/data/carParcorDataSample.csv", sep=",", row.names = F, col.names = T)
      #write.table(parcorData[total>10, ], file="ccScores.csv", sep=",", row.names = F, col.names = T)
      
      
  }) #end observe
}) #end

  



## end recursive section
###############################################################################
