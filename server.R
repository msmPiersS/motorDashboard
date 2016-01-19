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
## allow for different index month- input from front end
## bug on explore- select multiple inputs gets stuck in loop
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
  load(file='carParcorAGAll.rdata')
  
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
      #rollM = 3
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
      
    #### waterfall plot section - age and gender
      
      waterfallData1 = finalAGQ[yq == parCorQ1, .(oldQ = yq, segIdAG, oldAvgMnthEnq = totEnq/months, oldEnqToSale = totSaleEnq/totEnq, oldAvgMnthSaleEnq = totSaleEnq/months)]
      setkey(waterfallData1, segIdAG)
      waterfallData2 = finalAGQ[yq == parCorQ2, .(newQ = yq, segIdAG, mainAge, gender, newAvgMnthEnq = totEnq/months, newEnqToSale = totSaleEnq/totEnq, newAvgMnthSaleEnq = totSaleEnq/months)]
      setkey(waterfallData2, segIdAG)
      waterfallData = waterfallData2[waterfallData1]
      tmp1 = waterfallData[, list(mainAge = 'All', 
                           newAvgMnthEnq = sum(newAvgMnthEnq), newAvgMnthSaleEnq = sum(newAvgMnthSaleEnq), 
                           newEnqToSale = sum(newAvgMnthSaleEnq)/sum(newAvgMnthEnq),
                           oldAvgMnthEnq = sum(oldAvgMnthEnq), oldAvgMnthSaleEnq = sum(oldAvgMnthSaleEnq), 
                           oldEnqToSale = sum(oldAvgMnthSaleEnq)/sum(oldAvgMnthEnq)), 
                    by = list(newQ, oldQ, gender)]
      tmp1[, segIdAG:=seq(97,98,1)]
      tmp2 = waterfallData[, list(segIdAG = 99, mainAge = 'All', gender = 'All',  
                                  newAvgMnthEnq = sum(newAvgMnthEnq), newAvgMnthSaleEnq = sum(newAvgMnthSaleEnq), 
                                  newEnqToSale = sum(newAvgMnthSaleEnq)/sum(newAvgMnthEnq),
                                  oldAvgMnthEnq = sum(oldAvgMnthEnq), oldAvgMnthSaleEnq = sum(oldAvgMnthSaleEnq), 
                                  oldEnqToSale = sum(oldAvgMnthSaleEnq)/sum(oldAvgMnthEnq)), 
                           by = list(newQ, oldQ)]
      waterfallData = rbindlist(list(waterfallData, tmp1, tmp2), use.names = TRUE)
      waterfallData[, newSales_sameConv := newAvgMnthEnq * oldEnqToSale]
      waterfallData[, volImpact := newSales_sameConv - oldAvgMnthSaleEnq]
      waterfallData[, convImpact := newAvgMnthSaleEnq - newSales_sameConv]
      waterfallData[, change := newAvgMnthSaleEnq - oldAvgMnthSaleEnq]
      
      test = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 1)
      test[, desc:='Start Q']
      test[, type:='orgSales']
      test[, start:=0]
      test[, end:=waterfallData[, oldAvgMnthSaleEnq]]
      test[, amount:=waterfallData[, oldAvgMnthSaleEnq]]
      
      test2 = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 2)
      test2[, desc:='Vol. Impact']
      test2[, type:='volIMpact']
      test2[, start:=waterfallData[, oldAvgMnthSaleEnq]]
      test2[, end:=waterfallData[, newSales_sameConv]]
      test2[, amount:=waterfallData[, volImpact]]
      
      test3 = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 3)
      test3[, desc:='Conv. Impact']
      test3[, type:='convIMpact']
      test3[, start:=waterfallData[, newSales_sameConv]]
      test3[, end:=waterfallData[, newAvgMnthSaleEnq]]
      test3[, amount:=waterfallData[, convImpact]]
      
      test4 = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 4)
      test4[, desc:='End Q']
      test4[, type:='newSale']
      test4[, start:=waterfallData[, 0]]
      test4[, end:=waterfallData[, newAvgMnthSaleEnq]]
      test4[, amount:=waterfallData[, newAvgMnthSaleEnq]]
      
      waterfallPlot = rbindlist(list(test,test2,test3,test4), use.names = TRUE)
      waterfallPlot[, dir:= 'blue']
      waterfallPlot[id %in% c(2,3) & amount<0, dir:= 'red']
      waterfallPlot[id %in% c(2,3) & amount>=0, dir:= 'green']
      waterfallPlot[, genderLabel:= "Male"]
      waterfallPlot[gender==2, genderLabel:= "Female"]
      waterfallPlot[gender=="All", genderLabel:= "All"]
      waterfallPlot[, titleLabel:= paste(mainAge," - ", genderLabel, sep="")]
      
      setkey(waterfallPlot, segIdAG, id)
      #waterfallPlot[, desc:=as.factor(desc)]
      #waterfallPlot[, type:=as.factor(type)]
      waterfallPlot$dir = factor(waterfallPlot$dir, c("blue", "red", "green"))
      #manualColours = c("#9999CC", "#CC6666", "#66CC99") 
      manualColours = c("#4532ED", "#DB1A47", "#288C04") 
      
      ##waterfall plots  
      #descLabels = factor(waterfallPlot[segIdAG==99, desc], levels = waterfallPlot[segIdAG==99, desc])
      waterfallPlot$desc = factor(waterfallPlot$desc, levels = waterfallPlot[segIdAG==99, desc])
      waterfallPlot$titleLabel = factor(waterfallPlot$titleLabel, levels = waterfallPlot[id==1, titleLabel])
      
      output$waterfallPlotA <- renderPlot({
        
        wA = ggplot(waterfallPlot[segIdAG>=97, ], aes(fill = dir)) + 
          geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, 
                        ymin = end,ymax = start)) +
          scale_y_continuous(name = "avg. monthly sales") + 
          scale_x_discrete(name = "", breaks = levels(descLabels)) +
          scale_fill_manual(guide = "none", values = manualColours) + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          facet_wrap(~ titleLabel)
        
        print(wA)
      })
      
      output$waterfallPlotD <- renderPlot({
        
        wD = ggplot(waterfallPlot[segIdAG<97, ], aes(fill = dir)) + 
          geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, 
                        ymin = end,ymax = start)) +
          scale_y_continuous(name = "avg. monthly sales") + 
          scale_x_discrete(name = "", breaks = levels(descLabels)) +
          scale_fill_manual(guide = "none", values = manualColours) + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          facet_grid(genderLabel ~ mainAge)
        print(wD)
      })
      
      #do this based on changes rather than overall
      
      testC = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 1)
      testC[, desc:='Vol. Impact']
      testC[, type:='volIMpact']
      testC[, start:=0]
      testC[, end:=waterfallData[, volImpact]]
      testC[, amount:=waterfallData[, volImpact]]
      
      test2C = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 2)
      test2C[, desc:='Conv. Impact']
      test2C[, type:='convIMpact']
      test2C[, start:=waterfallData[, volImpact]]
      test2C[, end:=waterfallData[, change]]
      test2C[, amount:=waterfallData[, convImpact]]
      
      test3C = data.table(segIdAG = waterfallData[, segIdAG], gender = waterfallData[, gender], mainAge = waterfallData[, mainAge], id = 3)
      test3C[, desc:='Sales Change']
      test3C[, type:='salesChange']
      test3C[, start:=0]
      test3C[, end:=waterfallData[, change]]
      test3C[, amount:=waterfallData[, change]]
      
      waterfallPlotC = rbindlist(list(testC,test2C,test3C), use.names = TRUE)
      waterfallPlotC[, dir:= 'blue']
      waterfallPlotC[amount<0, dir:= 'red']
      waterfallPlotC[amount>=0, dir:= 'green']
      waterfallPlotC[, genderLabel:= "Male"]
      waterfallPlotC[gender==2, genderLabel:= "Female"]
      waterfallPlotC[gender=="All", genderLabel:= "All"]
      waterfallPlotC[, titleLabel:= paste(mainAge," - ", genderLabel, sep="")]
      
      setkey(waterfallPlotC, segIdAG, id)
      #waterfallPlot[, desc:=as.factor(desc)]
      #waterfallPlot[, type:=as.factor(type)]
      waterfallPlotC$dir = factor(waterfallPlotC$dir, c("blue", "red", "green"))
      #manualColours = c("#9999CC", "#CC6666", "#66CC99") 
      #manualColoursC = c("#4532ED", "#DB1A47", "#288C04") 
      
      manualColoursC = c("#DB1A47", "#288C04") 
      
      ##waterfall plots  
      #descLabels = factor(waterfallPlot[segIdAG==99, desc], levels = waterfallPlot[segIdAG==99, desc])
      waterfallPlotC$desc = factor(waterfallPlotC$desc, levels = waterfallPlotC[segIdAG==99, desc])
      waterfallPlotC$titleLabel = factor(waterfallPlotC$titleLabel, levels = waterfallPlotC[id==1, titleLabel])
      
      
      output$waterfallPlotAC <- renderPlot({
        
          wAC = ggplot(waterfallPlotC[segIdAG>=97, ], aes(fill = dir)) + 
            geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, 
                          ymin = end,ymax = start)) +
            geom_hline(yintercept=0) +
            scale_y_continuous(name = "avg. monthly sales") + 
            scale_x_discrete(name = "") +
            scale_fill_manual(guide = "none", values = manualColoursC) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            facet_wrap(~ titleLabel)
          print(wAC)
      })    
      
      output$waterfallPlotDC <- renderPlot({
        
#           wDC = ggplot(waterfallPlotC[segIdAG<97, ], aes(fill = dir)) + 
#             geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, 
#                           ymin = end,ymax = start)) +
#             geom_hline(yintercept=0) +
#             scale_y_continuous(name = "avg. monthly sales") + 
#             scale_x_discrete(name = "") +
#             scale_fill_manual(guide = "none", values = manualColoursC) + 
#             theme_minimal() +
#             theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#             facet_wrap(~ titleLabel, ncol=2)
#           
          wDC = ggplot(waterfallPlotC[segIdAG<97, ], aes(fill = dir)) + 
            geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, 
                          ymin = end,ymax = start)) +
            geom_hline(yintercept=0) +
            scale_y_continuous(name = "avg. monthly sales") + 
            scale_x_discrete(name = "") +
            scale_fill_manual(guide = "none", values = manualColoursC) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            facet_grid(genderLabel ~ mainAge)
          
          print(wDC)
      })
    
      
#       #clean up
#       finalAGQ[, c('totMonths', 'minEnq', 'okMonths', 'excludeFlag') :=NULL] 
#       finalAGQ[ is.nan(transPerSale), transPerSale:=1]
#       finalAGQ[ is.nan(avgMonthlyEnq), avgMonthlyEnq:=0]
#       finalAGQ[ is.nan(enqToSale), enqToSale:=0]
#       finalAGQ[ is.nan(enqToClick), enqToClick:=0]
#       finalAGQ[ is.nan(clicksPerCLicker), clicksPerCLicker:=0]
#       finalAGQ[ is.nan(clickToSale), clickToSale:=0]
#       finalAGQ[ is.nan(pctZeroNCB), pctZeroNCB:=0]
#       finalAGQ[ is.nan(pct1Driver), pct1Driver:=0]
#       finalAGQ[ is.nan(pct1CoverType), pct1CoverType:=0]
#       finalAGQ[ is.nan(pctLessThan5yrs), pctLessThan5yrs:=0]
#       finalAGQ[ is.nan(pctOneCar), pctOneCar:=0]
#       finalAGQ[ is.nan(pctLessThan5k), pctLessThan5k:=0]
#       finalAGQ[ is.nan(pct1plusClaims), pct1plusClaims:=0]
#       finalAGQ[ is.nan(pct1plusOffences), pct1plusOffences:=0]
#       finalAGQ[ is.nan(pctLessThan1w), pctLessThan1w:=0]
#       
#       parcorDataAG1 = finalAGQ[ yq %in% c(parCorQ1), ]
#       parcorDataAG2 = finalAGQ[ yq %in% c(parCorQ2), ]
#       setkey(parcorDataAG1, segIdAG)
#       setkey(parcorDataAG2, segIdAG)
#       parcorAGOut = parcorDataAG2[parcorDataAG1]
#       parcorAGOut[, 'Ch_avgMonthlyEnq':=avgMonthlyEnq - i.avgMonthlyEnq, with=FALSE]
#       parcorAGOut[, 'Ch_ZeroNCB':= pctZeroNCB - i.pctZeroNCB, with=FALSE]
#       parcorAGOut[, 'Ch_1Driver':= pct1Driver - i.pct1Driver, with=FALSE]
#       parcorAGOut[, 'Ch_1CoverType':= pct1CoverType - i.pct1CoverType, with=FALSE]
#       parcorAGOut[, 'Ch_enqToSale':= enqToSale - i.enqToSale, with=FALSE]
#       parcorAGOut[, 'Ch_enqToClick':= enqToClick - i.enqToClick, with=FALSE]
#       parcorAGOut[, 'Ch_ClicksPerClicker':= clicksPerCLicker - i.clicksPerCLicker, with=FALSE]
#       parcorAGOut[, 'Ch_clickToSale' := clickToSale - i.clickToSale, with=FALSE]
#       parcorAGOut[, 'Ch_okResults' := pctOkResults - i.pctOkResults, with=FALSE]
#       #setnames(parcorAGOut, finalMetricsListAG, paste(parCorQ2,"-",finalMetricsListAG, sep=""))
#       setnames(parcorAGOut, finalMetricsListAG, paste("tgtQ_",finalMetricsListAG, sep=""))
#       parcorAGOut[, colnames(parcorAGOut)[grep("i\\.", colnames(parcorAGOut))]:=NULL]  
#       
#       outColsAG = setdiff(colnames(parcorAGOut), c('yq'))
#       outColsAG = outColsAG[grep('_transPerSale',outColsAG, invert=TRUE)]
#       outMetricsAG = outColsAG[grep('_',outColsAG)]
#       volMetricsAG = outMetricsAG[grep("avgMonthlyEnq", outMetricsAG)]
#       pctMetricsAG = setdiff(outMetricsAG, volMetricsAG)
#       parcorAGOut[,(volMetricsAG) := round(.SD,0), .SDcols=volMetricsAG]
#       parcorAGOut[,(pctMetricsAG) := round(.SD,4), .SDcols=pctMetricsAG]
#       
#       write.table(parcorAGOut[, outColsAG, with=FALSE], file="parcor/data/carParcorDataAG.csv", sep=",", row.names = F, col.names = T)
#       
#       AGplot1 = ggplot(data = parcorAGOut, aes(x= gender, y= mainAge, size = tgtQ_avgMonthlyEnq, colour = tgtQ_enqToSale)) + 
#         geom_point() +
#         scale_x_discrete(name = "gender") + 
#         scale_y_discrete(name = "Age of Main Driver") +
#         scale_size_continuous(guide="none", range = c(5,25)) +
#         scale_colour_gradient(name = "Enq to Sale Conversion", low="red", high="green", label = percent) +
#         theme_minimal() +
#         ggtitle("Performance by Age and Gender - target Q")
#       
#       
#       AGplot2 = ggplot(data = parcorAGOut, aes(x= gender, y= mainAge, size = Ch_avgMonthlyEnq, colour = Ch_enqToSale)) + 
#         geom_point() +
#         scale_x_discrete(name = "gender") + 
#         scale_y_discrete(name = "Age of Main Driver") +
#         scale_size_continuous(guide="none", range = c(5,25)) +
#         scale_colour_gradient(name = "Change in Enq to Sale Conversion", low="red", high="green", label = percent) +
#         theme_minimal() +
#         ggtitle("Performance by Age and Gender - Change")
#       
#       
#       AGplot3 = ggplot(data = parcorAGOut, aes(x= gender, y= mainAge, fill = Ch_avgMonthlyEnq)) + 
#         geom_tile() +
#         scale_x_discrete(name = "gender") + 
#         scale_y_discrete(name = "Age of Main Driver") +
#         scale_fill_gradient(name = "Change in Enq to Sale Conversion", low="red", high="green") +
#         theme_minimal() +
#         ggtitle("Performance by Age and Gender - Change")
#       
#       AGplot4 = ggplot(data = parcorAGOut, aes(x= gender, y= mainAge, fill = Ch_enqToSale)) + 
#         geom_tile() +
#         scale_x_discrete(name = "gender") + 
#         scale_y_discrete(name = "Age of Main Driver") +
#         scale_fill_gradient(name = "Change in Enq to Sale Conversion", low="red", high="green", label=percent) +
#         theme_minimal() +
#         ggtitle("Performance by Age and Gender - Change")
#       
      
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
      
      
      
      #### Parcor section 2
#       #clean up
#       final3Q[, c('totMonths', 'minEnq', 'okMonths', 'excludeFlag') :=NULL]  
#       final3Q[ is.nan(transPerSale), transPerSale:=1]
#       final3Q[ is.nan(avgMonthlyEnq), avgMonthlyEnq:=0]
#       final3Q[ is.nan(enqToSale), enqToSale:=0]
#       final3Q[ is.nan(enqToClick), enqToClick:=0]
#       final3Q[ is.nan(clicksPerCLicker), clicksPerCLicker:=0]
#       final3Q[ is.nan(clickToSale), clickToSale:=0]
#       final3Q[ is.nan(pctLessThan5yrs), pctLessThan5yrs:=0]
#       final3Q[ is.nan(pctOneCar), pctOneCar:=0]
#       final3Q[ is.nan(pctLessThan5k), pctLessThan5k:=0]
#       final3Q[ is.nan(pct1plusClaims), pct1plusClaims:=0]
#       final3Q[ is.nan(pct1plusOffences), pct1plusOffences:=0]
#       final3Q[ is.nan(pctLessThan1w), pctLessThan1w:=0]
#       
#       
#       
#       parcorData31 = final3Q[ yq %in% c(parCorQ1), ]
#       parcorData32 = final3Q[ yq %in% c(parCorQ2), ]
#       setkey(parcorData31, segId3)
#       setkey(parcorData32, segId3)
#       parcor3Out = parcorData32[parcorData31]
#       parcor3Out[, 'Ch-LessThan5yrs':= pctLessThan5yrs - i.pctLessThan5yrs, with=FALSE]
#       parcor3Out[, 'Ch-OneCar':= pctOneCar - i.pctOneCar, with=FALSE]
#       parcor3Out[, 'Ch-LessThan5k':= pctLessThan5k - i.pctLessThan5k, with=FALSE]
#       parcor3Out[, 'Ch-1plusClaims':= pct1plusClaims - i.pct1plusClaims, with=FALSE]
#       parcor3Out[, 'Ch-1plusOffences':= pct1plusOffences - i.pct1plusOffences, with=FALSE]
#       parcor3Out[, 'Ch-LessThan1w':= pctLessThan1w - i.pctLessThan1w, with=FALSE]
#       parcor3Out[, 'PctCh-Enq':=avgMonthlyEnq/i.avgMonthlyEnq - 1, with=FALSE]
#       parcor3Out[, 'Ch-enqToSale':= enqToSale - i.enqToSale, with=FALSE]
#       parcor3Out[, 'Ch-enqToClick':= enqToClick - i.enqToClick, with=FALSE]
#       parcor3Out[, 'Ch-ClicksPerClicker':= clicksPerCLicker - i.clicksPerCLicker, with=FALSE]
#       parcor3Out[, 'Ch-clickToSale' := clickToSale - i.clickToSale, with=FALSE]
#       parcor3Out[, 'Ch-okResults' := pctOkResults - i.pctOkResults, with=FALSE]
#       setnames(parcor3Out, finalMetricsList3, paste(parCorQ2,"-",finalMetricsList3, sep=""))
#       parcor3Out[, colnames(parcor3Out)[grep("i\\.", colnames(parcor3Out))]:=NULL]  
#       
#       outCols3 = setdiff(colnames(parcor3Out), c('yq'))
#       outCols3 = outCols3[grep('-transPerSale',outCols3, invert=TRUE)]
#       outMetrics3 = outCols3[grep('-',outCols3)]
#       volMetrics3 = outMetrics3[grep('avgMonthlyEnq',outMetrics3)]
#       pctMetrics3 = setdiff(outMetrics3, volMetrics3)
#       parcor3Out[,(volMetrics3) := round(.SD,0), .SDcols=volMetrics3]
#       parcor3Out[,(pctMetrics3) := round(.SD,4), .SDcols=pctMetrics3]
#       
#       
#       write.table(parcor3Out[, outCols3, with=FALSE], file="parcor/data/carParcorData3.csv", sep=",", row.names = F, col.names = T)
#       #write.table(parcor3Out[sample(1:nrow(parcor3Out),100), outCols3, with=FALSE], file="parcor/data/carParcorDataSample3.csv", sep=",", row.names = F, col.names = T)
#       #write.table(parcorData[total>10, ], file="ccScores.csv", sep=",", row.names = F, col.names = T)
#       
#       
      
      
  }) #end observe
}) #end

  



## end recursive section
###############################################################################
