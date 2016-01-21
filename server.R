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
  options(shiny.trace=TRUE) # set to True to debug
  #options(shiny.error=browser) # set error to browser
  options(shiny.maxRequestSize=100*1024^2) # set max input file size to 100MB

  #load data
  load(file='carDashboard.rdata')
  load(file='carParcor.rdata')
  load(file='carParcorAGAll.rdata')
  
  tgtMnth = '201301'
  excludeMonths = c('201201','201202','201203','201204','201205','201206',
                    '201207','201208','201209','201210','201211','201212',
                    #'201301','201302','201303','201304','201305','201306',
                    #'201307','201308','201309','201310','201311','201312',
                    '201512')
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

      overallIdx = 100*overall[, !"ym", with=FALSE] / overall[rep(which(overall[, ym] == tgtMnth), nrow(overall)), !"ym", with=FALSE]
      overallIdx[, ym := overall[, ym]]
      #remove excluded months
      overallIdx = overallIdx[!(ym %in% excludeMonths), ]
      
      
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
      
      endData = overallPlot[ym == '201511', ]
      startData = overallPlot[ym == '201411', ]
      setkey(endData,variable)
      setkey(startData,variable)
      trendData = endData[startData]
      trendData[, change:= value/i.value-1]  
      trendVarList = unique(trendData[, variable])
      trendFinalList = trendVarList[grep('ravg', trendVarList)]    
      
      output$trendPlot <- renderPlot({  
        tp = ggplot(trendData[variable %in% trendFinalList, ], aes(x=variable, y=change, fill = variable)) + 
          geom_bar(stat='identity') +
          geom_hline(yintercept=0) +
          scale_y_continuous(name = "pct change", label = percent) + 
          scale_x_discrete(name = "", labels = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      
        print(tp)
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
      
      #get target quarters from input
      #parCorQ1 = '201404'
      #parCorQ2 = '201504'
      qList = as.character(sort(unique(finalAGQ[, yq])))
      
      q1InClean <- reactive({
        if (is.null(input$q1In))
          qList[length(qList)-4]
        else
          input$q1In
      })
      q2InClean <- reactive({
        if (is.null(input$q2In))
          qList[length(qList)]
        else
          input$q2In
      })
      
      
      #create input selections
      output$chooseQ1 <- renderUI({
        
        # Create the checkboxes and select them all by default
        selectInput("q1In", "Choose start quarter:",
                    qList,
                    selected = q1InClean())
      })
      output$chooseQ2 <- renderUI({
        # Create the checkboxes and select them all by default
        selectInput("q2In", "Choose end quarter:",
                    qList,
                    selected = q2InClean())
      })
      
      #print(input$q1In)
      #print(input$q2In)
      
      parCorQ1 = q1InClean()
      parCorQ2 = q2InClean()
      
      
      waterfallData1 = finalAGQ[yq == parCorQ1, .(oldQ = yq, segIdAG, 
                                                  oldAvgMnthEnq = totEnq/months, oldEnqToSale = totSaleEnq/totEnq, oldAvgMnthSaleEnq = totSaleEnq/months,
                                                  oldpctZeroNCB = pctZeroNCB, oldpct1Driver = pct1Driver, oldpct1CoverType = pct1CoverType, 
                                                  oldpctLessThan5yrs = pctLessThan5yrs, oldpctOneCar = pctOneCar, oldpctLessThan5k = pctLessThan5k,
                                                  oldpct1plusClaims = pct1plusClaims, oldpct1plusOffences = pct1plusOffences, oldpctLessThan1w = pctLessThan1w)]
      setkey(waterfallData1, segIdAG)
      waterfallData2 = finalAGQ[yq == parCorQ2, .(newQ = yq, segIdAG, mainAge, gender, 
                                                  newAvgMnthEnq = totEnq/months, newEnqToSale = totSaleEnq/totEnq, newAvgMnthSaleEnq = totSaleEnq/months,
                                                  newpctZeroNCB = pctZeroNCB, newpct1Driver = pct1Driver, newpct1CoverType = pct1CoverType, 
                                                  newpctLessThan5yrs = pctLessThan5yrs, newpctOneCar = pctOneCar, newpctLessThan5k = pctLessThan5k,
                                                  newpct1plusClaims = pct1plusClaims, newpct1plusOffences = pct1plusOffences, newpctLessThan1w = pctLessThan1w)]
                                                  
      setkey(waterfallData2, segIdAG)
      waterfallData = waterfallData2[waterfallData1]
      tmp1 = waterfallData[, list(mainAge = 'All', 
                           newAvgMnthEnq = sum(newAvgMnthEnq), newAvgMnthSaleEnq = sum(newAvgMnthSaleEnq), 
                           newEnqToSale = sum(newAvgMnthSaleEnq)/sum(newAvgMnthEnq),
                           oldAvgMnthEnq = sum(oldAvgMnthEnq), oldAvgMnthSaleEnq = sum(oldAvgMnthSaleEnq), 
                           oldEnqToSale = sum(oldAvgMnthSaleEnq)/sum(oldAvgMnthEnq),
                           oldpctZeroNCB = sum(oldpctZeroNCB*oldAvgMnthEnq)/sum(oldAvgMnthEnq), 
                           oldpct1Driver = sum(oldpct1Driver*oldAvgMnthEnq)/sum(oldAvgMnthEnq), 
                           oldpct1CoverType = sum(oldpct1CoverType*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpctLessThan5yrs = sum(oldpctLessThan5yrs*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpctOneCar = sum(oldpctOneCar*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpctLessThan5k = sum(oldpctLessThan5k*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpct1plusClaims = sum(oldpct1plusClaims*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpct1plusOffences = sum(oldpct1plusOffences*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           oldpctLessThan1w = sum(oldpctLessThan1w*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                           newpctZeroNCB = sum(newpctZeroNCB*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpct1Driver = sum(newpct1Driver*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpct1CoverType = sum(newpct1CoverType*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpctLessThan5yrs = sum(newpctLessThan5yrs*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpctOneCar = sum(newpctOneCar*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpctLessThan5k = sum(newpctLessThan5k*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpct1plusClaims = sum(newpct1plusClaims*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpct1plusOffences = sum(newpct1plusOffences*newAvgMnthEnq)/sum(newAvgMnthEnq),
                           newpctLessThan1w = sum(newpctLessThan1w*newAvgMnthEnq)/sum(newAvgMnthEnq)),
                    by = list(newQ, oldQ, gender)]
      tmp1[, segIdAG:=seq(97,98,1)]
      tmp2 = waterfallData[, list(segIdAG = 99, mainAge = 'All', gender = 'All',  
                                  newAvgMnthEnq = sum(newAvgMnthEnq), newAvgMnthSaleEnq = sum(newAvgMnthSaleEnq), 
                                  newEnqToSale = sum(newAvgMnthSaleEnq)/sum(newAvgMnthEnq),
                                  oldAvgMnthEnq = sum(oldAvgMnthEnq), oldAvgMnthSaleEnq = sum(oldAvgMnthSaleEnq), 
                                  oldEnqToSale = sum(oldAvgMnthSaleEnq)/sum(oldAvgMnthEnq),
                                  oldpctZeroNCB = sum(oldpctZeroNCB*oldAvgMnthEnq)/sum(oldAvgMnthEnq), 
                                  oldpct1Driver = sum(oldpct1Driver*oldAvgMnthEnq)/sum(oldAvgMnthEnq), 
                                  oldpct1CoverType = sum(oldpct1CoverType*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpctLessThan5yrs = sum(oldpctLessThan5yrs*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpctOneCar = sum(oldpctOneCar*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpctLessThan5k = sum(oldpctLessThan5k*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpct1plusClaims = sum(oldpct1plusClaims*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpct1plusOffences = sum(oldpct1plusOffences*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  oldpctLessThan1w = sum(oldpctLessThan1w*oldAvgMnthEnq)/sum(oldAvgMnthEnq),
                                  newpctZeroNCB = sum(newpctZeroNCB*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpct1Driver = sum(newpct1Driver*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpct1CoverType = sum(newpct1CoverType*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpctLessThan5yrs = sum(newpctLessThan5yrs*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpctOneCar = sum(newpctOneCar*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpctLessThan5k = sum(newpctLessThan5k*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpct1plusClaims = sum(newpct1plusClaims*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpct1plusOffences = sum(newpct1plusOffences*newAvgMnthEnq)/sum(newAvgMnthEnq),
                                  newpctLessThan1w = sum(newpctLessThan1w*newAvgMnthEnq)/sum(newAvgMnthEnq)), 
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
    
      
     
      
    #### graphs of change in characteristics
      changePlot = waterfallData[, list(segIdAG, mainAge, gender, volImpact, convImpact,
                                        pctZeroNCBdiff = newpctZeroNCB - oldpctZeroNCB,
                                        pct1Driverdiff = newpct1Driver - oldpct1Driver,
                                        pct1CoverTypediff = newpct1CoverType - oldpct1CoverType,
                                        pctLessThan5yrsdiff = newpctLessThan5yrs - oldpctLessThan5yrs,
                                        pctOneCardiff = newpctOneCar - oldpctOneCar,
                                        pctLessThan5kdiff = newpctLessThan5k - oldpctLessThan5k,
                                        pct1plusClaimsdiff = newpct1plusClaims - oldpct1plusClaims,
                                        pct1plusOffencesdiff = newpct1plusOffences - oldpct1plusOffences,
                                        pctLessThan1wdiff = newpctLessThan1w - oldpctLessThan1w
                                        )]
      changePlot[, genderLabel:= "Male"]
      changePlot[gender==2, genderLabel:= "Female"]
      changePlot[gender=="All", genderLabel:= "All"]
      changePlot[, titleLabel:= paste(mainAge," - ", genderLabel, sep="")]
      
      changeVars = names(changePlot)
      meltIdsVars = c("segIdAG", "mainAge", "gender", "genderLabel","titleLabel")
      meltMeasureVars = setdiff(changeVars, meltIdsVars)
      
      changePlotFinal = melt(changePlot, id.vars = meltIdsVars, 
                            measure.vars = meltMeasureVars)
      
      
      
      output$changePlotA <- renderPlot({    
        cPA = ggplot(changePlotFinal[segIdAG>=97 & !(variable %in% c('volImpact', 'convImpact')), ], aes(x=variable, y=value, fill = variable)) + 
          geom_bar(stat='identity') +
          geom_hline(yintercept=0) +
          scale_y_continuous(name = "pct difference", label = percent) + 
          scale_x_discrete(name = "", labels = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          facet_wrap(~ titleLabel)
      
        print(cPA)
      })  
        
      output$changePlotD <- renderPlot({  
        cPD = ggplot(changePlotFinal[segIdAG<97 & !(variable %in% c('volImpact', 'convImpact')), ], aes(x=variable, y=value, fill = variable)) + 
          geom_bar(stat='identity') +
          geom_hline(yintercept=0) +
          scale_y_continuous(name = "pct difference", label = percent) + 
          scale_x_discrete(name = "", labels = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          facet_grid(genderLabel ~ mainAge)   
        print(cPD)
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
      outCols = outCols[grep('-enqToClick',outCols, invert=TRUE)]
      outCols = outCols[grep('-clicksPerCLicker',outCols, invert=TRUE)]
      outCols = outCols[grep('-clickToSale',outCols, invert=TRUE)]
      outCols = outCols[grep('-pctOkResults',outCols, invert=TRUE)]
      
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
