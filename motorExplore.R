##################################################################################
## Motor Insurance dashboard - micro segments over time
## ps December 2015
##
## Data used- 
## Monthly aggregate generated from proc sql from ADM_VEH.ADM_VEH_ENQUIRY
##
#################################################################################

#################################################################################
## set up

  ## options
  options(scipen=999)

  ## locations
  homeDir = '/Users/piers.stobbs/Documents/piers/Box Sync/datascience/eligibility/'
  dataDir = '/Volumes/safebox/sandbox/'
  
  #setwd = homeDir
  homeDir = getwd()
  #dataDir = '/data/'
  
  ## libraries
  library(data.table)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(zoo)
  library(reshape2)
  #library(lubridate)
  #detach("package:lubridate", unload=TRUE)
  
  ## data files
  rawFile = 'enquiry_agg.csv'
  revFile = 'trafficRevClean.csv'
  
## end set up
#################################################################################
  
  
  
#################################################################################
## prepare and manipulate data
  #load data
  
  #rev
  rev = fread(paste(dataDir, revFile, sep=""))
  str(rev)
  
  #raw
  raw = fread(paste(dataDir, rawFile, sep=""))
  str(raw)
  #raw2 = fread(paste(dataDir, "enquiry_agg_20160125.csv", sep=""))
  #str(raw)
  
  # remove NAs
  raw = raw[!is.na(raw[ ,month]), ]
  
  #clean up fields
  raw[, ym := as.numeric(paste(as.character(raw[, year]), str_sub(paste("00",as.character(raw[, month]), sep=""), start=-2), sep=""))]
  raw[, mainAge:= as.factor(mainAge)]
  raw[, licenseHeld:= as.factor(licenseHeld)]
  raw[, noClaims:= as.factor(noClaims)]
  raw[, gender:= as.factor(gender)]
  raw[, drivers:= as.factor(drivers)]
  raw[, numCars:= as.factor(numCars)]
  raw[, coverType:= as.factor(coverType)]
  raw[, persMiles:= as.factor(persMiles)]
  raw[, numClaims:= as.factor(numClaims)]
  raw[, numOffences:= as.factor(numOffences)]
  raw[, leadTime:= as.factor(leadTime)]
  raw[, validProps:= as.factor(validProps)]

  #clean up dates - remove any partial months
  excludeM = '201512'
  raw = raw[ym!=excludeM, ]
  
  raw$mainAge = factor(raw$mainAge, levels = 
                         c("0: <17", "1: 17-18", "2: 19-20", "3: 21-22", "4: 23-25", 
                           "5: 26-30", "6: 31-40", "7: 41-50", "8: 51-60", "9: 61-80", "10: 81+"))
  
  varList = c("mainAge", "licenseHeld", "noClaims", "gender", "drivers", "numCars", 
              "coverType", "persMiles", "numClaims", "numOffences", "leadTime",
              "validProps")
  
  
  
  
  #deal with props
  fullList = c('ym', 'month', 'year', 'mainAge', 'licenseHeld', 'noClaims', 'gender', 'drivers', 
               'numCars', 'coverType', 'persMiles', 'numClaims', 'numOffences', 'leadTime')
  raw2 = raw[, list(ym, month, year, mainAge, licenseHeld, noClaims, gender, drivers, numCars, 
                    coverType, persMiles, numClaims, numOffences, leadTime, 
                    totEnq = sum(totEnquiries), totClickEnq = sum(totClickEnquiries), 
                    totCl = sum(totClicks), totSaleEnq = sum(totSaleEnquiries), totS = sum(totSales)
                    ), by = list(ym, month, year, mainAge, licenseHeld, noClaims, gender, drivers, numCars, 
                                 coverType, persMiles, numClaims, numOffences, leadTime)]
  setkeyv(raw2, fullList)
  tmpOK = raw[validProps=='2: okProps', c(fullList, 'totEnquiries'), with=FALSE]
  setnames(tmpOK, 'totEnquiries', 'okResults')
  setkeyv(tmpOK, fullList)
  metricList = c('totEnq', 'totClickEnq','totCl', 'totSaleEnq', 'totS', 'okResults')
  raw2 = tmpOK[raw2][, c(fullList, metricList), with=FALSE]
  raw2[ is.na(okResults), okResults := 0]
  raw2[, c('enqToSale','enqToClick','clickToSale','pctOkResults') := 
         list(totSaleEnq/totEnq, totClickEnq/totEnq, totSaleEnq/totClickEnq, okResults/totEnq)]
  raw2[is.na(raw2)] = 0
  minEnq = 100
  raw2[, okFlag:=totEnq>=minEnq]
  

  
  raw3 = copy(raw2)
  rawAG = copy(raw2)
  
  microSegSum = raw2[, list(mainAge, licenseHeld, noClaims, gender, drivers, numCars, 
                    coverType, persMiles, numClaims, numOffences, leadTime, 
                    totMonths = .N, minEnq = min(totEnq), okMonths = sum(okFlag)), 
                    by = list(mainAge, licenseHeld, noClaims, gender, drivers, numCars, 
               coverType, persMiles, numClaims, numOffences, leadTime)]
  microSegSum[, excludeFlag:=0]
  
  microSegSum[substr(mainAge,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(licenseHeld,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(noClaims,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(gender,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(drivers,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(numCars,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(coverType,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(persMiles,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(numClaims,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(numOffences,1,3) == "0: ", excludeFlag:=1]
  microSegSum[substr(leadTime,1,3) == "0: ", excludeFlag:=1]
  nrow(microSegSum[ excludeFlag==0, ])
  
  setkeyv(microSegSum, c('mainAge', 'licenseHeld', 'noClaims', 'gender', 'drivers', 'numCars', 
                           'coverType', 'persMiles', 'numClaims', 'numOffences', 'leadTime'))
  
  microSegSum[, segId:=seq(1,nrow(microSegSum),1)]
  
  #nrow(microSegSum[totMonths > 30 & minEnq > 10 & excludeFlag ==0, ])
  okSegId = microSegSum[totMonths > 30 & minEnq > 10 & excludeFlag ==0, segId]
  
  
  
  #join segid back into raw2
  setkeyv(raw2, c('mainAge', 'licenseHeld', 'noClaims', 'gender', 'drivers', 'numCars', 
                         'coverType', 'persMiles', 'numClaims', 'numOffences', 'leadTime'))
  raw2 = raw2[microSegSum][, c('segId', fullList, metricList, 'pctOkResults', 'totMonths', 'okMonths', 'excludeFlag'), with=FALSE]
  
  #add in a quarter flag
  raw2[, qtr:=0]
  raw2[month %in% c(1,2,3), qtr:=1]
  raw2[month %in% c(4,5,6), qtr:=2]
  raw2[month %in% c(7,8,9), qtr:=3]
  raw2[month %in% c(10,11,12), qtr:=4]
  
  #add in yq 
  raw2[, yq := as.numeric(paste(as.character(raw2[, year]), paste("0",as.character(raw2[, qtr]), sep=""), sep=""))]
  raw2[, yq:=as.factor(yq)]
  
  
  #condense down to qtr view, taking avg monthlies - remove December
  rawQ = raw2[, list(yq, segId, totEnq = sum(totEnq), totClickEnq = sum(totClickEnq),
                     totCl = sum(totCl), totSaleEnq = sum(totSaleEnq), totS = sum(totS), 
                     okResults = sum(okResults), months=.N), by = list(yq, segId)][, 
                    list(yq, segId, totEnq, totClickEnq, totCl, totSaleEnq, totS, okResults, months)]
  
  setkeyv(rawQ,c('segId', 'yq'))
  
  finalQ = rawQ[segId %in% okSegId, list(segId, yq, totEnq, totClickEnq, totCl, totSaleEnq, totS, okResults, months)]
  
  finalQ[, 
         c('avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults') 
         := list(totEnq/months, totSaleEnq/totEnq, totSaleEnq/totClickEnq, totCl/totClickEnq, totSaleEnq/totClickEnq, totS/totSaleEnq, okResults/totEnq)]
  
  finalQ[, c('totEnq', 'totClickEnq', 'totCl', 'totSaleEnq', 'totS', 'okResults', 'months') :=NULL]
  
  #join in segment details
  setkey(finalQ, segId)
  setkey(microSegSum, segId)
  finalQ = microSegSum[finalQ][, c('yq','segId', 'mainAge', 'licenseHeld', 'noClaims', 'gender', 'drivers', 'numCars', 
                          'coverType', 'persMiles', 'numClaims', 'numOffences', 'leadTime', 
                          'avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults'), with=FALSE]
                      
  finalVarList = c('mainAge', 'licenseHeld', 'noClaims', 'gender', 'drivers', 'numCars', 
                   'coverType', 'persMiles', 'numClaims', 'numOffences', 'leadTime')
  finalMetricsList = c('avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults')
    
  save(finalQ, okSegId, finalVarList, finalMetricsList, file='carParcor.rdata')
  
  ## additional consolidation
  ## final required features and measured
  ## features: yq, mainAge, gender, noclaims, drivers, coverType
  ## metrics: avg licenseHeld, avg numCars, avg persMiles, avg numClaims, avg numOffences, avg leadTime
  
  ## do it again but with additional consolidation
  
  raw3 = raw3[, list(enqLessThan5yrs = sum((licenseHeld == "1: 0-4")*totEnq), 
                     enqOneCar = sum((numCars == "1: 1")*totEnq), 
                     enqLessThan5k = sum((persMiles== "1: 0-5k")*totEnq), 
                     enq1plusClaims = sum((numClaims== "2: 1+")*totEnq), 
                     enq1plusOffences = sum((numOffences== "2: 1+")*totEnq), 
                     enqLessThan1w = sum((leadTime== "1: 0-6")*totEnq), 
                       totEnq = sum(totEnq), totClickEnq = sum(totClickEnq), totCl = sum(totCl), 
                            totSaleEnq = sum(totSaleEnq), totS = sum(totS), okResults = sum(okResults)), 
                     by = list(ym, month, year, mainAge, gender, noClaims, drivers, coverType)]
  raw3[, okFlag:=totEnq>=minEnq]
  
  microSegSum3 = raw3[, list(totMonths = .N, minEnq = min(totEnq), okMonths = sum(okFlag)), 
                     by = list(mainAge, gender, noClaims, drivers, coverType)]
  
  microSegSum3[, excludeFlag:=0]
  microSegSum3[substr(mainAge,1,3) == "0: ", excludeFlag:=1]
  microSegSum3[substr(noClaims,1,3) == "0: ", excludeFlag:=1]
  microSegSum3[substr(gender,1,3) == "0: ", excludeFlag:=1]
  microSegSum3[substr(drivers,1,3) == "0: ", excludeFlag:=1]
  microSegSum3[substr(coverType,1,3) == "0: ", excludeFlag:=1]
  nrow(microSegSum3[ excludeFlag==0, ])
  
  setkeyv(microSegSum3, c('mainAge', 'noClaims', 'gender', 'drivers', 'coverType'))
          
  microSegSum3[, segId3:=seq(1,nrow(microSegSum3),1)]
  
  #nrow(microSegSum[totMonths > 30 & minEnq > 10 & excludeFlag ==0, ])
  okSegId3 = microSegSum3[totMonths > 30 & minEnq > 10 & excludeFlag ==0, segId3]
  
  #join segid back into raw2
  setkeyv(raw3, c('mainAge', 'noClaims', 'gender', 'drivers', 'coverType'))
  raw3 = raw3[microSegSum3]
  
  #add in a quarter flag
  raw3[, qtr:=0]
  raw3[month %in% c(1,2,3), qtr:=1]
  raw3[month %in% c(4,5,6), qtr:=2]
  raw3[month %in% c(7,8,9), qtr:=3]
  raw3[month %in% c(10,11,12), qtr:=4]
  
  #add in yq 
  raw3[, yq := as.numeric(paste(as.character(raw3[, year]), paste("0",as.character(raw3[, qtr]), sep=""), sep=""))]
  raw3[, yq:=as.factor(yq)]
  
  
  #condense down to qtr view, taking avg monthlies - remove December
  raw3Q = raw3[, list(enqLessThan5yrs = sum(enqLessThan5yrs), 
                      enqOneCar = sum(enqOneCar), 
                      enqLessThan5k = sum(enqLessThan5k), 
                      enq1plusClaims = sum(enq1plusClaims), 
                      enq1plusOffences = sum(enq1plusOffences), 
                      enqLessThan1w = sum(enqLessThan1w), 
                      totEnq = sum(totEnq), totClickEnq = sum(totClickEnq),
                      totCl = sum(totCl), totSaleEnq = sum(totSaleEnq), totS = sum(totS), 
                      okResults = sum(okResults), months=.N), by = list(yq, segId3)]
  
  setkeyv(raw3Q,c('segId3', 'yq'))
  
  final3Q = raw3Q[segId3 %in% okSegId3, ] 
  final3Q[, 
         c('pctLessThan5yrs', 'pctOneCar' ,'pctLessThan5k', 'pct1plusClaims', 'pct1plusOffences', 'pctLessThan1w' 
           ,'avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults') 
         := list(enqLessThan5yrs/totEnq, enqOneCar/totEnq, enqLessThan5k/totEnq, enq1plusClaims/totEnq, enq1plusOffences/totEnq, enqLessThan1w/totEnq, 
                 totEnq/months, totSaleEnq/totEnq, totSaleEnq/totClickEnq, totCl/totClickEnq, totSaleEnq/totClickEnq, totS/totSaleEnq, okResults/totEnq)]
  
  final3Q[, c('enqLessThan5yrs', 'enqOneCar' ,'enqLessThan5k', 'enq1plusClaims', 'enq1plusOffences', 'enqLessThan1w', 
              'totEnq', 'totClickEnq', 'totCl', 'totSaleEnq', 'totS', 'okResults', 'months') :=NULL]
  
  #join in segment details
  setkey(final3Q, segId3)
  setkey(microSegSum3, segId3)
  final3Q = microSegSum3[final3Q]
  
  finalVarList3 = c('mainAge', 'noClaims', 'gender', 'drivers', 'coverType')
  finalMetricsList3 = c('pctLessThan5yrs', 'pctOneCar' ,'pctLessThan5k', 'pct1plusClaims', 'pct1plusOffences', 'pctLessThan1w' 
                       ,'avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults')
  
  
  save(final3Q, okSegId3, finalVarList3, finalMetricsList3, file='carParcor3.rdata')
  
  
  ## age gender consolidation
  ## final required features and measured
  ## features: yq, mainAge, gender
  ## metrics: avg licenseHeld, avg numCars, avg persMiles, avg numClaims, avg numOffences, avg leadTime
  ##          avg noclaims, avg drivers, covertype==1 
  
  
  ## do it again but with additional consolidation
  rawAG = rawAG[, list(enqZeroNCB = sum((noClaims == "1: 0")*totEnq),
                       enq1Driver = sum((drivers == "1: 1")*totEnq),
                       enq1CoverType = sum((coverType == "1")*totEnq),
                       enqLessThan5yrs = sum((licenseHeld == "1: 0-4")*totEnq), 
                     enqOneCar = sum((numCars == "1: 1")*totEnq), 
                     enqLessThan5k = sum((persMiles== "1: 0-5k")*totEnq), 
                     enq1plusClaims = sum((numClaims== "2: 1+")*totEnq), 
                     enq1plusOffences = sum((numOffences== "2: 1+")*totEnq), 
                     enqLessThan1w = sum((leadTime== "1: 0-6")*totEnq), 
                     totEnq = sum(totEnq), totClickEnq = sum(totClickEnq), totCl = sum(totCl), 
                     totSaleEnq = sum(totSaleEnq), totS = sum(totS), okResults = sum(okResults)), 
              by = list(ym, month, year, mainAge, gender)]
  rawAG[, okFlag:=totEnq>=minEnq]
  
  microSegSumAG = rawAG[, list(totMonths = .N, minEnq = min(totEnq), okMonths = sum(okFlag)), 
                      by = list(mainAge, gender)]
  
  microSegSumAG[, excludeFlag:=0]
  microSegSumAG[substr(mainAge,1,3) == "0: ", excludeFlag:=1]
  microSegSumAG[substr(gender,1,3) == "0: ", excludeFlag:=1]
  nrow(microSegSumAG[ excludeFlag==0, ])
  
  setkeyv(microSegSumAG, c('mainAge', 'gender'))
  
  microSegSumAG[, segIdAG:=seq(1,nrow(microSegSumAG),1)]
  
  #nrow(microSegSum[totMonths > 30 & minEnq > 10 & excludeFlag ==0, ])
  okSegIdAG = microSegSumAG[totMonths > 30 & minEnq > 10 & excludeFlag ==0, segIdAG]
  
  #join segid back into raw2
  setkeyv(rawAG, c('mainAge', 'gender'))
  rawAG = rawAG[microSegSumAG]
  
  #add in a quarter flag
  rawAG[, qtr:=0]
  rawAG[month %in% c(1,2,3), qtr:=1]
  rawAG[month %in% c(4,5,6), qtr:=2]
  rawAG[month %in% c(7,8,9), qtr:=3]
  rawAG[month %in% c(10,11,12), qtr:=4]
  
  #add in yq 
  rawAG[, yq := as.numeric(paste(as.character(rawAG[, year]), paste("0",as.character(rawAG[, qtr]), sep=""), sep=""))]
  rawAG[, yq:=as.factor(yq)]
  
  
  #condense down to qtr view, taking avg monthlies - remove December
  rawAGQ = rawAG[, list(enqZeroNCB = sum(enqZeroNCB),
                        enq1Driver = sum(enq1Driver),
                        enq1CoverType = sum(enq1CoverType),
                        enqLessThan5yrs = sum(enqLessThan5yrs), 
                      enqOneCar = sum(enqOneCar), 
                      enqLessThan5k = sum(enqLessThan5k), 
                      enq1plusClaims = sum(enq1plusClaims), 
                      enq1plusOffences = sum(enq1plusOffences), 
                      enqLessThan1w = sum(enqLessThan1w), 
                      totEnq = sum(totEnq), totClickEnq = sum(totClickEnq),
                      totCl = sum(totCl), totSaleEnq = sum(totSaleEnq), totS = sum(totS), 
                      okResults = sum(okResults), months=.N), by = list(yq, segIdAG)]
  
  setkeyv(rawAGQ,c('segIdAG', 'yq'))
  
  finalAGQ = rawAGQ[segIdAG %in% okSegIdAG, ] 
  finalAGQ[, 
          c('pctZeroNCB','pct1Driver','pct1CoverType','pctLessThan5yrs', 'pctOneCar' ,'pctLessThan5k', 'pct1plusClaims', 'pct1plusOffences', 'pctLessThan1w' 
            ,'avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults') 
          := list(enqZeroNCB/totEnq, enq1Driver/totEnq, enq1CoverType/totEnq, enqLessThan5yrs/totEnq, enqOneCar/totEnq, enqLessThan5k/totEnq, enq1plusClaims/totEnq, enq1plusOffences/totEnq, enqLessThan1w/totEnq, 
                  totEnq/months, totSaleEnq/totEnq, totSaleEnq/totClickEnq, totCl/totClickEnq, totSaleEnq/totClickEnq, totS/totSaleEnq, okResults/totEnq)]
  
  #finalAGQ[, c('enqZeroNCB', 'enq1Driver', 'enq1CoverType', 'enqLessThan5yrs', 'enqOneCar' ,'enqLessThan5k', 'enq1plusClaims', 'enq1plusOffences', 'enqLessThan1w', 
  #            'totEnq', 'totClickEnq', 'totCl', 'totSaleEnq', 'totS', 'okResults', 'months') :=NULL]
  
  #join in segment details
  setkey(finalAGQ, segIdAG)
  setkey(microSegSumAG, segIdAG)
  finalAGQ = microSegSumAG[finalAGQ]
  
  #finalVarListAG = c('mainAge', 'gender')
  #finalMetricsListAG = c('pctZeroNCB', 'pct1Driver', 'pct1CoverType', 'pctLessThan5yrs', 'pctOneCar' ,'pctLessThan5k', 'pct1plusClaims', 'pct1plusOffences', 'pctLessThan1w' 
  #                      ,'avgMonthlyEnq', 'enqToSale','enqToClick', 'clicksPerCLicker', 'clickToSale', 'transPerSale', 'pctOkResults')
  
  
  #save(finalAGQ, okSegIdAG, finalVarListAG, finalMetricsListAG, file='carParcorAG.rdata')
  save(finalAGQ, okSegIdAG, file='carParcorAGAll.rdata')
  
  
  
  #### create single feature summaries
  str(raw)
  agg = raw[, list(totEnq = sum(totEnquiries), totCLickEnq = sum(totClickEnquiries), 
             totSaleEnq = sum(totSaleEnquiries), totSales = sum(totSales), 
             wavgMeanCheapestPrice = sum(meanCheapestPrice * totEnquiries) / sum(totEnquiries), 
             wavgPropCount = sum(meanPropCount * totEnquiries) / sum(totEnquiries)), 
             by = list(ym, mainAge)]
  
  agg[, var := as.factor(paste("mainAge", mainAge, sep = "--"))] 
  agg[, type:="mainAge"]
  agg[, mainAge:=NULL]
  
  for (v in 2:length(varList)) {
    #v=2
    b <- parse(text = paste("list(ym, ",varList[v],")", sep=""))
    
    tmp = raw[, list(totEnq = sum(totEnquiries), totCLickEnq = sum(totClickEnquiries), 
               totSaleEnq = sum(totSaleEnquiries), totSales = sum(totSales), 
               wavgMeanCheapestPrice = sum(meanCheapestPrice * totEnquiries) / sum(totEnquiries), 
               wavgPropCount = sum(meanPropCount * totEnquiries) / sum(totEnquiries)), 
        by = eval(b)]
    
    tmp[, var:=as.factor(paste(varList[v], tmp[[varList[v]]], sep = "--")) ]   
    tmp[, type:=varList[v]]
    tmp[[varList[v]]] = NULL
    
    agg = rbindlist(list(agg,tmp), use.names = TRUE)
    
  }
  
  
  #create relevant metrics
  agg[, enqToClick := totCLickEnq/totEnq]
  agg[, clickToSale := totSaleEnq/totCLickEnq]
  agg[, enqToSale := totSaleEnq/totEnq]
  
  
  
  
  #pull out for plotting
  aggPlot = agg[, list(ym, var, type)]
  aggPlot[, volume:= agg[, totEnq]]
  aggPlot[, conv1:= agg[, enqToSale]]

  #pull in rev stuff
  #revClean = rev[, list(ym, totalSessions, carIns_ga, carRev)]
  # want to show total market trend, total visitor trend, car channel trend, car enquiry trend, car buyer trend, revenue trend
  #overall = rev[, .(ym, aggCarAllKnown, totalSessions, carIns_ga, carRev_eb)]
  overall = rev[, .(ym, aggCarAllKnown, totalSessions, CarTrafficExForeign, carRev_eb)]
  overall = overall[ ym!='201512', ]
  # need totenquiries, tot clickers, tot buyers
  overallEnq = raw[, .(totEnq = sum(totEnquiries), totClickers = sum(totClickEnquiries), totBuyers = sum(totSaleEnquiries)), 
        by = .(ym)]
  overallEnq = overallEnq[ ym!='201512', ]
  setkey(overall, ym)
  setkey(overallEnq, ym)
  overall = overall[overallEnq, ]
  #setnames(overall, c('carIns_ga', 'totEnq', 'totClickers', 'totBuyers', 'aggCarAllKnown', 'carRev_eb') , c('carSessions', 'carEnquries', 'carClickers', 'carBuyers', 'carMarket', 'carRev'))
  setnames(overall, c('CarTrafficExForeign', 'totEnq', 'totClickers', 'totBuyers', 'aggCarAllKnown', 'carRev_eb') , c('carSessions', 'carEnquries', 'carClickers', 'carBuyers', 'carMarket', 'carRev'))
  overallNames = setdiff(colnames(overall), 'ym')
  save(overall, file='topLine.rdata')
  save(overall, agg, file='carDashboard.rdata')
  #load(file='topLine.rdata')
  
  #create rolling means
  #first identify nas for future
  naIdx = is.na(overall)
  #then fill in nas with nearest value
  overall = as.data.table(na.fill(overall, "extend"))
  overall[, ym := as.factor(ym)]
  
  
  #rolling periods
  rollM = 1
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

  overallPlot[variable %in% c('carBuyers', 'ravg_carBuyers'), ]
    
  overallPlot[, size := 0.1]
  overallPlot[variable %in% overallNamesAvg, size := 0.2]
  
  #overallPlot[, ym:=as.factor(ym)]
  
  #plot volume trends
  ggplot(overallPlot, aes(x=ym, y=value, group = variable, colour = colours)) +
    geom_line(data = overallPlot[variable %in% overallNamesAvg, ], size = 0.5) + 
    scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
    scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
  
    geom_line(overallPlot[variable %in% overallNamesRaw, ], 
              aes(x=ym, y=value, group = variable, colour = variable, size = 0.1)) +
    geom_line(overallPlot[variable %in% overallNamesAvg, ], aes(x=ym, y=value, group = variable, colour = variable, size = 0.1)) +
    scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
    scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
  
  
  
  
  
  
  
  test = copy(overallIdx)
  test[, paste0("ravg_", overallNames) := lapply(.SD, rollapply, width = 3, mean, fill = NA, align ='right'), .SDcols = overallNames]
  
  allnewNames = colnames(test)
  avgNames = allnewNames[grep("ravg", allnewNames)]
  testPlot = melt(test[, c('ym',avgNames), with=FALSE], id = 'ym', measure = avgNames)
  testPlot[, ym:=as.factor(ym)]
  
  
  
  x = c(120,105,118,140,142,141,135,152,154,138,125,132,131,120)
  plot(x, type="l")
  
  #overallIdx[, paste0("ravg_", overallNames) := lapply(.SD, rollmean, k = 3, na.pad = TRUE), .SDcols = overallNames]
  
  
  overallNames = setdiff(colnames(overallIdx), 'ym')
  overallMonths = levels(overallPlot[, ym])
  overallPlot = melt(overallIdx, id = 'ym', measure = overallNames)
  overallPlot[, ym:=as.factor(ym)]
  
  #volume
  ggplot(overallPlot, aes(x=ym, y=value, group = variable, colour = variable)) +
    geom_line(size = 0.1) +
    geom_smooth(size = 1, method = 'loess', degree=0, 
                span=0.1, se=T, alpha = 0.2) +
    scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
    scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
  ggplot(testPlot, aes(x=ym, y=value, group = variable, colour = variable)) +
    geom_line(size = 1) +
    scale_y_continuous(name = "index", breaks = c(80,90,100,110,120)) + 
    scale_x_discrete(name = "month", breaks = c(overallMonths[seq(1,length(overallMonths),4)])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
  
  str(agg)
  
  revClean = data.table(ym = rev[, ym], var = "totalSessions", type = "Overall", volume = rev[, totalSessions])
  revClean = rbindlist(list(revClean, 
                            data.table(ym = rev[, ym], var = "carSessions", type = "Overall", volume = rev[, carIns_ga])),
                       use.names = TRUE)
  revClean = rbindlist(list(revClean, 
                            data.table(ym = rev[, ym], var = "carRev", type = "Overall", volume = rev[, carRev])),
                       use.names = TRUE)
  revClean = revClean[!is.na(volume), ]
  revClean[, conv1 := 0.01] 
  
  aggPlot = rbindlist(list(aggPlot, revClean), use.names = TRUE)
  
  aggPlot[, ym:=as.factor(ym)]
  aggPlot = aggPlot[ym!="201512", ]
  
  #pull in m1 for each series for indexing
  tmp = aggPlot[ym=='201301', .(var, volume)]
  setkey(tmp, var)
  setkey(aggPlot, var)
  aggPlot = aggPlot[tmp]
  setnames(aggPlot, "i.volume", "vol201301")
  #calcualte index
  aggPlot[, volIndex:= 100*volume/vol201301]
  
  #pull in month total for each series for share
  tmp = aggPlot[, .(totVol = sum(volume)), by = .(ym, type)]
  setkeyv(tmp, c('ym', 'type'))
  setkeyv(aggPlot, c('ym', 'type'))
  aggPlot = aggPlot[tmp]
  #fix the overall share figures to 100
  aggPlot[ type=='Overall', totVol:=volume]
  #calculate share
  aggPlot[, volShare:= 100*volume/totVol]
  
  
  aggPlot2 = aggPlot[, .(ym, type, var, value=volIndex, metric='volumeIndex')]
  aggPlot2 = rbindlist(list(aggPlot2, aggPlot[, .(ym, type, var, value=volShare, metric='volumeShare')]), 
                       use.names = T, fill = F)
  aggPlot2 = rbindlist(list(aggPlot2, aggPlot[, .(ym, type, var, value=100*conv1, metric='buyerConversion')]), 
                       use.names = T, fill = F)
  
  typeList = unique(aggPlot[, type])
  
  #plots
  #volume
  ggplot(aggPlot, aes(x=ym, y=volume, group = var, colour = var, size = log(1+conv1))) +
    geom_line() +
    facet_wrap(~type) + 
    theme_minimal() 
  
  
  #index
  ggplot(aggPlot, aes(x=ym, y=volIndex, group = var, colour = var)) +
    geom_line() +
    scale_y_continuous(limits = c(0,200)) +
    facet_wrap(~type) + 
    theme_minimal() 
  
  
  tgtType = 2
  ggplot(aggPlot2[ type == typeList[tgtType], ], aes(x=ym, y=value, group = var, colour = var)) +
    geom_line() +
    scale_y_continuous(limits = c(0,200)) +
    facet_wrap(~metric) + 
    theme_minimal() 
  
  
  aggPlot2[type=='mainAge' & ym == '201511', ]
  #### note- things to remove- gener--0
  #### takeaways - 
  ## cover type 2 fallen away
  ## drivers 2 or more fallen away
  ## shorter lead times - 0-6 up
  ## licenceHeld for less time: 0-4
  ## younger drivers up : 17-18
  ## extremes on no claims 0 and 10+
  ## more than 1 car fallen away
  ## claims up a little
  ## but offences down
  
  #share
  ggplot(aggPlot, aes(x=ym, y=volShare, group = var, colour = var)) +
    geom_line() +
    facet_wrap(~type) + 
    theme_minimal() 
  
  
  
  ## to do
  ## pull in share data
  ## standard kpi assessment
  ## calculated estimated rev from each segment if held same- diff based on volume and conversion
  
  ## work on micro segents - 
  ## naming/ids
  ## indexing and share
  ## parcor?
  
  
  
  tmpDate = as.POSIXct(ccUsersRaw[, DateOfSubmission], "%Y-%m-%d %H:%M:%S", tz = "")
  ccUsersRaw[, DateOfSubmission:=as.IDate(tmpDate)]
  ccUsersRaw[, TimeOfSubmission:=as.ITime(tmpDate)]
  
  ccUsersRaw[, SubmissionType:=as.factor(SubmissionType)]
  ccUsersRaw[, PostcodeSector:=as.factor(PostcodeSector)]
  setnames(ccUsersRaw, "YEAR(DateOfBirth)", "BirthYear")
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "0", TimeAtCurrentAddress:=  "0 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "1", TimeAtCurrentAddress:= "1 year"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "1 years", TimeAtCurrentAddress:= "1 year"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "2", TimeAtCurrentAddress:= "2 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "3", TimeAtCurrentAddress:= "3 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "4", TimeAtCurrentAddress:= "4 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "5", TimeAtCurrentAddress:= "5 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "6", TimeAtCurrentAddress:=  "6 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "7", TimeAtCurrentAddress:= "7 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "8", TimeAtCurrentAddress:= "8 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "9", TimeAtCurrentAddress:= "9 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "10", TimeAtCurrentAddress:= "10 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "11", TimeAtCurrentAddress:= "11 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "12", TimeAtCurrentAddress:= "12 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "13", TimeAtCurrentAddress:= "13 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "14", TimeAtCurrentAddress:= "14 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "15", TimeAtCurrentAddress:= "15 years"]
  ccUsersRaw[ccUsersRaw[ , TimeAtCurrentAddress] == "16+", TimeAtCurrentAddress:= "16+ years"]
  
  
  ccUsersRaw[, TimeAtCurrentAddress:=as.factor(TimeAtCurrentAddress)]
  levels(ccUsersRaw$TimeAtCurrentAddress) = c("0 years", "1 year", "2 years" , "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", 
                                                 "9 years" , "10 years", "11 years", "12 years", "13 years", "14 years", "15 years", "16+ years")
  
  ccUsersRaw[, CurrentBank:=as.factor(CurrentBank)]
  ccUsersRaw[, ResidentialStatus:=as.factor(ResidentialStatus)]
  ccUsersRaw[, EmploymentStatus:=as.factor(EmploymentStatus)]
  ccUsersRaw[, ProductType:=as.factor(ProductType)]
  ccUsersRaw[, LeadProduct:=as.factor(LeadProduct)]
  ccUsersRaw[, Result:=as.factor(Result)]
  ccUsersRaw[, Source:=as.factor(Source)]
  ccUsersRaw[, Age:=2016-BirthYear]
  
  br = seq(0,200000,10000)
  lab = paste(br/1000, "k", sep="")
  ccUsersRaw[, IncomeRange:=cut(ccUsersRaw[, Income], br, lab[2:(length(lab))])]
  
  #postcodes
  ccUsersRaw[, PostcodeSector]
  rexp <- "^(\\w+)\\s?(.*)$"
  ccUsersRaw[, PostcodeArea:=as.factor(toupper(sub(rexp,"\\1", ccUsersRaw[, PostcodeSector])))]
  #try and clean up
  #trim to 4 characters
  ccUsersRaw[, PostcodeArea:=substr(PostcodeArea,1,4)]
  #trim to 3 where 4th character is not numeric
  ccUsersRaw[grepl("[^0-9]", substr(ccUsersRaw[, PostcodeArea],4,4)), PostcodeArea:=substr(PostcodeArea,1,3)]
  ccUsersRaw[, PostcodeArea:=as.factor(PostcodeArea)]
  tabulate(ccUsersRaw[, PostcodeArea])
  
  tmp = table(ccUsersRaw[, PostcodeArea])
  length(tmp[tmp>10])
  
  str(ccUsersRaw)
  summary(ccUsersRaw)
  
  #filters to use:
  # Result == Success
  # !(is.na(IncomeRange)
  cleanIdx = (ccUsersRaw[, Result] == "Success") & (!is.na(ccUsersRaw[, IncomeRange]))
  ccUsersClean = ccUsersRaw[cleanIdx, ]
  levels(ccUsersClean[, PostcodeArea])
  
  #final list
  varFactorList = c("PostcodeArea", "TimeAtCurrentAddress", "CurrentBank", 
              "ResidentialStatus", "EmploymentStatus", "ProductType", "IncomeRange")
  varNumericList = c("Income", "BirthYear", "Age")
  
    
  #### distributions
  ggplot(ccUsersClean, aes(x=TimeAtCurrentAddress)) +
    geom_histogram(binwidth=.5, colour="black", fill="white") 
  
  ggplot(ccUsersClean, aes_string(x=varFactorList[7])) +
    geom_histogram(binwidth=.5, colour="black", fill="white") 
  
  ggplot(ccUsersClean, aes_string(x=varNumericList[3])) +
    geom_density() 
  
  
  
  
  
  
## end explore users
#################################################################################
  
  
  
  
#################################################################################
## explore products
  #load data
  ccProductsRaw = fread(paste(dataDir, ccProductsFile, sep=""))
  str(ccProductsRaw)
  summary(ccProductsRaw)
  
  #clean up fields
  #clean up fields
  ccProductsRaw[, Product:=as.factor(Product)]
  ccProductsRaw[, ProductType:=as.factor(ProductType)]
  ccProductsRaw[, ProductSet:=as.factor(ProductSet)]
  ccProductsRaw[, Likelihood:=as.factor(Likelihood)]
  
  levels(ccProductsRaw$Likelihood) = c(
    "95%", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", 
    "Low", "Hard fail", "Not rated"
  )

  ccProductsRaw[, LikelihoodScore:=0]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "10%", LikelihoodScore:=0.1]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "20%", LikelihoodScore:=0.2]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "30%", LikelihoodScore:=0.3]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "40%", LikelihoodScore:=0.4]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "50%", LikelihoodScore:=0.5]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "60%", LikelihoodScore:=0.6]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "70%", LikelihoodScore:=0.7]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "80%", LikelihoodScore:=0.8]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "90%", LikelihoodScore:=0.9]
  ccProductsRaw[ccProductsRaw[, Likelihood] == "95%", LikelihoodScore:=0.95]
  
  sum(ccProductsRaw[, LikelihoodScore]==0)
  table(ccProductsRaw[, LikelihoodScore])

  ccProductsRaw
  levels(ccProductsRaw[, ProductType])

  # remove not rated
  sum(ccProductsRaw[, Likelihood] == "Not rated")
  sum(ccProductsRaw[, Likelihood] == "Low")
  sum(ccProductsRaw[, Likelihood] == "Hard fail")
  ccProductsRaw = ccProductsRaw[ccProductsRaw[, Likelihood] != "Not rated", ]
  
    
  #create agg by submissionid
  str(ccProductsRaw)
  ccProductsAgg = ccProductsRaw[, list(count = .N, 
                       med = median(LikelihoodScore), 
                       avg = mean(LikelihoodScore), 
                       min = min(LikelihoodScore), 
                       max = max(LikelihoodScore),
                       pct50 = length(LikelihoodScore[LikelihoodScore>=0.5])/.N,
                       pct80 = length(LikelihoodScore[LikelihoodScore>=0.8])/.N,
                       pct90 = length(LikelihoodScore[LikelihoodScore>=0.9])/.N,
                       leadProductCount = length(LikelihoodScore[ProductSet=="Lead Product"]),
                       leadProductAvg = sum(LikelihoodScore[ProductSet=="Lead Product"])/length(LikelihoodScore[ProductSet=="Lead Product"]),
                       zeroPctCount = length(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctAvg = sum(LikelihoodScore[ProductType=="0% Spending"])/length(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctMax = max(LikelihoodScore[ProductType=="0% Spending"]),
                       zeroPctMin = min(LikelihoodScore[ProductType=="0% Spending"]),
                       airlineCount = length(LikelihoodScore[ProductType=="Airline"]),
                       airlineAvg = sum(LikelihoodScore[ProductType=="Airline"])/length(LikelihoodScore[ProductType=="Airline"]),
                       airlineMax = max(LikelihoodScore[ProductType=="Airline"]),
                       airlineMin = min(LikelihoodScore[ProductType=="Airline"]),
                       allroundCount = length(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundAvg = sum(LikelihoodScore[ProductType=="All-Rounder"])/length(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundMax = max(LikelihoodScore[ProductType=="All-Rounder"]),
                       allroundMin = min(LikelihoodScore[ProductType=="All-Rounder"]),
                       badcreditCount = length(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditAvg = sum(LikelihoodScore[ProductType=="Bad Credit"])/length(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditMax = max(LikelihoodScore[ProductType=="Bad Credit"]),
                       badcreditMin = min(LikelihoodScore[ProductType=="Bad Credit"]),
                       balanceTransCount = length(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransAvg = sum(LikelihoodScore[ProductType=="Balance Transfer"])/length(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransMax = max(LikelihoodScore[ProductType=="Balance Transfer"]),
                       balanceTransMin = min(LikelihoodScore[ProductType=="Balance Transfer"]),
                       cashbackCount = length(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackAvg = sum(LikelihoodScore[ProductType=="Cashback"])/length(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackMax = max(LikelihoodScore[ProductType=="Cashback"]),
                       cashbackMin = min(LikelihoodScore[ProductType=="Cashback"]),
                       moneyTransCount = length(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransAvg = sum(LikelihoodScore[ProductType=="Money Transfer"])/length(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransMax = max(LikelihoodScore[ProductType=="Money Transfer"]),
                       moneyTransMin = min(LikelihoodScore[ProductType=="Money Transfer"]),
                       rewardsCount = length(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsAvg = sum(LikelihoodScore[ProductType=="Rewards"])/length(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsMax = max(LikelihoodScore[ProductType=="Rewards"]),
                       rewardsMin = min(LikelihoodScore[ProductType=="Rewards"]),
                       travelCount = length(LikelihoodScore[ProductType=="Travel Money"]),
                       travelMax = max(LikelihoodScore[ProductType=="Travel Money"]),
                       travelMin = min(LikelihoodScore[ProductType=="Travel Money"]),
                       travelAvg = sum(LikelihoodScore[ProductType=="Travel Money"])/length(LikelihoodScore[ProductType=="Travel Money"])
  ), by = list(SubmissionID)]
  
  
  
  
  
  #figure out the join - key both tables on SubmissionID
  nrow(ccProductsAgg)
  nrow(ccUsersClean)
  setkey(ccProductsAgg, SubmissionID)
  setkey(ccUsersClean, SubmissionID)

  ccJoin = ccProductsAgg[ccUsersClean, ]

  levels(ccJoin$ProductType)
  levels(ccJoin$LeadProduct)
  
  ccJoin[, requestedNum:=0]
  ccJoin[, requestedAvg:=0]
  tmpIdx = ccJoin[, ProductType] == "0% Spending"
  ccJoin[tmpIdx, requestedNum:=as.numeric(zeroPctCount)]
  ccJoin[tmpIdx, requestedAvg:=zeroPctAvg]
  ccJoin[tmpIdx, requestedMax:=zeroPctMax]
  ccJoin[tmpIdx, requestedMin:=zeroPctMin]
  tmpIdx = ccJoin[, ProductType] == "Airline"
  ccJoin[tmpIdx, requestedNum:=as.numeric(airlineCount)]
  ccJoin[tmpIdx, requestedAvg:=airlineAvg]
  ccJoin[tmpIdx, requestedMax:=airlineMax]
  ccJoin[tmpIdx, requestedMin:=airlineMin]
  tmpIdx = ccJoin[, ProductType] == "All-Rounder"
  ccJoin[tmpIdx, requestedNum:=as.numeric(allroundCount)]
  ccJoin[tmpIdx, requestedAvg:=allroundAvg]
  ccJoin[tmpIdx, requestedMax:=allroundMax]
  ccJoin[tmpIdx, requestedMin:=allroundMin]
  tmpIdx = ccJoin[, ProductType] == "Bad Credit"
  ccJoin[tmpIdx, requestedNum:=as.numeric(badcreditCount)]
  ccJoin[tmpIdx, requestedAvg:=badcreditAvg]
  ccJoin[tmpIdx, requestedMax:=badcreditMax]
  ccJoin[tmpIdx, requestedMin:=badcreditMin]
  tmpIdx = ccJoin[, ProductType] == "Balance Transfer"
  ccJoin[tmpIdx, requestedNum:=as.numeric(balanceTransCount)]
  ccJoin[tmpIdx, requestedAvg:=balanceTransAvg]
  ccJoin[tmpIdx, requestedMax:=balanceTransMax]
  ccJoin[tmpIdx, requestedMin:=balanceTransMin]
  tmpIdx = ccJoin[, ProductType] == "Cashback"
  ccJoin[tmpIdx, requestedNum:=as.numeric(cashbackCount)]
  ccJoin[tmpIdx, requestedAvg:=cashbackAvg]
  ccJoin[tmpIdx, requestedMax:=cashbackMax]
  ccJoin[tmpIdx, requestedMin:=cashbackMin]
  tmpIdx = ccJoin[, ProductType] == "Money Transfer"
  ccJoin[tmpIdx, requestedNum:=as.numeric(moneyTransCount)]
  ccJoin[tmpIdx, requestedAvg:=moneyTransAvg]
  ccJoin[tmpIdx, requestedMax:=moneyTransMax]
  ccJoin[tmpIdx, requestedMin:=moneyTransMin]
  tmpIdx = ccJoin[, ProductType] == "Rewards"
  ccJoin[tmpIdx, requestedNum:=as.numeric(rewardsCount)]
  ccJoin[tmpIdx, requestedAvg:=rewardsAvg]
  ccJoin[tmpIdx, requestedMax:=rewardsMax]
  ccJoin[tmpIdx, requestedMin:=rewardsMin]
  tmpIdx = ccJoin[, ProductType] == "Travel Money"
  ccJoin[tmpIdx, requestedNum:=as.numeric(travelCount)]
  ccJoin[tmpIdx, requestedAvg:=travelAvg]
  ccJoin[tmpIdx, requestedMax:=travelAvg]
  ccJoin[tmpIdx, requestedMin:=travelAvg]
  
  #add in an age range
  #ccJoin[, AgeRange]
  br = seq(0,100,10)
  ccJoin[, AgeRange:=as.factor(cut(ccJoin[, Age], br))]
  ccUsersClean[, AgeRange:=as.factor(cut(ccUsersClean[, Age], br))]
  
  
  #figure out a join to the raw product for age and income
  ccProductJoin = ccProductsRaw[, list(SubmissionID, Product, ProductType, LikelihoodScore)]
  setkey(ccProductJoin, SubmissionID)
  setkey(ccUsersClean, SubmissionID)
  ccProductJoin = ccUsersClean[ccProductJoin][, list(SubmissionID, Product, ProductType, 
                                                     LikelihoodScore, AgeRange, IncomeRange, 
                                                     PostcodeArea, EmploymentStatus, ResidentialStatus,
                                                     CurrentBank, TimeAtCurrentAddress)]
  
  ccProductAgg = ccProductJoin[, list(total = .N,
                                      minScore = min(LikelihoodScore), 
                                      maxScore = max(LikelihoodScore),
                                      medianScore = median(LikelihoodScore), 
                                      meanScore = mean(LikelihoodScore)),
                               by = list(Product, ProductType, AgeRange, IncomeRange)
                               ]
  
  
  #create agg by score/product
  str(ccProductsRaw)
  ccScoreProductAgg = ccProductJoin[, list(total = .N),
                               by = list(ProductType, Product, AgeRange, 
                                         IncomeRange, PostcodeArea, EmploymentStatus, ResidentialStatus,
                                         CurrentBank, TimeAtCurrentAddress, 
                                         LikelihoodScore)]
  
  ccScoreProductAggExPC = ccScoreProductAgg[, list(total = sum(total)),
                                    by = list(ProductType, Product, AgeRange, 
                                              IncomeRange, EmploymentStatus, ResidentialStatus,
                                              CurrentBank, TimeAtCurrentAddress, 
                                              LikelihoodScore)]
  
  
  ccScoreProductAgg3 = ccScoreProductAgg[, list(total = sum(total)),
                                            by = list(ProductType, Product, AgeRange, 
                                                      IncomeRange, EmploymentStatus, ResidentialStatus,
                                                      CurrentBank, 
                                                      LikelihoodScore)]
  
  ccScoreProductAgg4 = ccScoreProductAgg[, list(total = sum(total)),
                                         by = list(ProductType, Product, AgeRange, 
                                                   IncomeRange, EmploymentStatus, ResidentialStatus,
                                                   LikelihoodScore)]
  
  ccScoreProductAgg5 = ccScoreProductAgg[, list(total = sum(total)),
                                         by = list(ProductType, Product, AgeRange, 
                                                   IncomeRange, EmploymentStatus,
                                                   LikelihoodScore)]
  
  
  ccScoreProductAgg6 = ccScoreProductAgg[, list(total = sum(total)),
                                         by = list(ProductType, Product, AgeRange, 
                                                   IncomeRange,
                                                   LikelihoodScore)]
  
  
  
  prodList = ccScoreProductAgg6[, list(total = sum(total)), by = list(ProductType, Product)]
  prodList = prodList[order(-total), ]
  prodList5 = prodList[1:5,]
  
  parcorData = ccScoreProductAgg4[ProductType == "Balance Transfer" & Product %in% prodList5[, Product], ]
  
  parcorData[, age:=0]
  parcorData[AgeRange == "(10,20]", age:=20]
  parcorData[AgeRange == "(20,30]", age:=30]
  parcorData[AgeRange == "(30,40]", age:=40]
  parcorData[AgeRange == "(40,50]", age:=50]
  parcorData[AgeRange == "(50,60]", age:=60]
  parcorData[AgeRange == "(60,70]", age:=70]
  parcorData[AgeRange == "(70,80]", age:=80]
  parcorData[AgeRange == "(80,90]", age:=90]
  parcorData[AgeRange == "(90,100]", age:=100]
  
  table(parcorData[, age])
  
  parcorData[, income:=as.numeric(gsub("k","",parcorData[, IncomeRange]))]
  parcorData[, AgeRange:=NULL]         
  parcorData[, IncomeRange:=NULL]         
  
  setnames(parcorData, c("ProductType", "Product", "EmploymentStatus", "ResidentialStatus", 
                         "LikelihoodScore", "total", "age", "income"), 
                      c("Credit Card Type", "Credit Card", "Employment Status", "Residential Status", 
                        "Likelihood Score", "Volume", "Age Range", "Income Range (k)"))
  
  setcolorder(parcorData,  
           c("Credit Card Type", "Credit Card", "Employment Status", "Residential Status", 
             "Age Range", "Income Range (k)", "Volume", "Likelihood Score"))
  
  
#  setnames(parcorData, c("ProductType", "Product", "EmploymentStatus", "ResidentialStatus", 
#                         "LikelihoodScore", "total", "age", "income"), 
#           c("CreditCardType", "CreditCard", "EmploymentStatus", "ResidentialStatus", 
#             "LikelihoodScore", "Volume", "AgeRange", "IncomeRange(k)"))
  
#  setcolorder(parcorData,  
#              c("CreditCardType", "CreditCard", "EmploymentStatus", "ResidentialStatus", 
#                "AgeRange", "IncomeRange(k)", "Volume", "LikelihoodScore"))
#  

#    setnames(parcorData, c("ProductType", "Product", "EmploymentStatus", "ResidentialStatus", 
#                         "LikelihoodScore", "total", "AgeRange", "IncomeRange"), 
#           c("Credit Card Type", "Credit Card", "Employment Status", "Residential Status", 
#             "Likelihood Score", "Volume", "Age Range", "Income Range (k)"))
  
#  setcolorder(parcorData,  
#              c("Credit Card Type", "Credit Card", "Employment Status", "Residential Status", 
#                "Age Range", "Income Range (k)", "Volume", "Likelihood Score"))
  
  
  outData = parcorData[Volume>10, ][order(-Volume), ]
  write.table(outData, file="ccScores.csv", sep=",", row.names = F, col.names = T)
  #write.table(parcorData[total>10, ], file="ccScores.csv", sep=",", row.names = F, col.names = T)
  
  
  nrow(ccScoreProductAgg[total>10, ])
  summary(ccJoin)
  
  
  ccJoin[ccJoin[, requestedMax]==0, ]
  
  str(ccJoin)
  
  ccProductsRaw[SubmissionID==5508331, ]
  hist(ccJoin[, requestedMax])  
  hist(ccJoin[, max])  
  
  #build uk pop aggs for age and income
  incomeData = ccJoin[, list(Population = .N), by = list(IncomeRange)]
  incomeData[, group:="MSE_CC_Eligibility"]
  incomeData = rbindlist(list(incomeData, ukIncome))
  incomeData[, proportion:=Population/sum(incomeData[group=="UK_Pop", Population])]
  incomeData[group=="MSE_CC_Eligibility", proportion:=Population/sum(incomeData[group=="MSE_CC_Eligibility", Population])]
  
  tmp = ccJoin[requestedMax<0.5, list(Population = .N), by = list(IncomeRange)]
  tmp[, group:="MSE_LowScore"]
  tmp[group=="MSE_LowScore", proportion:=Population/sum(tmp[group=="MSE_LowScore", Population])]
  incomeData = rbindlist(list(incomeData, tmp), use.names = T)
  
  
  ageData = ccJoin[, list(Population = .N), by = list(AgeRange)]
  ageData[, group:="MSE_CC_Eligibility"]
  ageData = rbindlist(list(ageData, ukAge))
  ageData[, proportion:=Population/sum(ageData[group=="UK_Pop", Population])]
  ageData[group=="MSE_CC_Eligibility", proportion:=Population/sum(ageData[group=="MSE_CC_Eligibility", Population])]
  
  
  tmp = ccJoin[requestedMax<0.5, list(Population = .N), by = list(AgeRange)]
  tmp[, group:="MSE_LowScore"]
  tmp[group=="MSE_LowScore", proportion:=Population/sum(tmp[group=="MSE_LowScore", Population])]
  ageData = rbindlist(list(ageData, tmp), use.names = T)
  
  #build product type app
  typeData = ccJoin[, list(Population = .N), by = list(ProductType)]
  typeData[, group:="MSE_CC_Eligibility"]
  typeData[group=="MSE_CC_Eligibility", proportion:=Population/sum(typeData[group=="MSE_CC_Eligibility", Population])]
  
  tmp = ccJoin[requestedMax<0.5, list(Population = .N), by = list(ProductType)]
  tmp[, group:="MSE_LowScore"]
  tmp[group=="MSE_LowScore", proportion:=Population/sum(tmp[group=="MSE_LowScore", Population])]
  typeData = rbindlist(list(typeData, tmp), use.names = T)
  
  
  #creating our reporting agg
  # need brakouts by product type, income and age with alls for everything
  # first do product type
  ccAgg = ccJoin[, list(totCount = .N, 
                            avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                            pct90Requested = sum(requestedMax>=0.9)/.N, 
                            pct80Requested = sum(requestedMax>=0.8)/.N, 
                            pct50Requested = sum(requestedMax>=0.5)/.N,
                            avgAll = sum(count*avg)/sum(count), 
                            pct90All = sum(max>=0.9)/.N, 
                            pct80All = sum(max>=0.8)/.N, 
                            pct50All = sum(max>=0.5)/.N),
                     by = list(ProductType)]
  ccAgg[, AgeRange:="All"]
  ccAgg[, IncomeRange:="All"]
  
  # next do income
  tmp = ccJoin[, list(totCount = .N, 
                        avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                        pct90Requested = sum(requestedMax>=0.9)/.N, 
                        pct80Requested = sum(requestedMax>=0.8)/.N, 
                        pct50Requested = sum(requestedMax>=0.5)/.N,
                        avgAll = sum(count*avg)/sum(count), 
                        pct90All = sum(max>=0.9)/.N, 
                        pct80All = sum(max>=0.8)/.N, 
                        pct50All = sum(max>=0.5)/.N),
                 by = list(IncomeRange)]
  tmp[, AgeRange:="All"]
  tmp[, ProductType:="All"]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  # next do age
  tmp = ccJoin[, list(totCount = .N, 
                      avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                      pct90Requested = sum(requestedMax>=0.9)/.N, 
                      pct80Requested = sum(requestedMax>=0.8)/.N, 
                      pct50Requested = sum(requestedMax>=0.5)/.N,
                      avgAll = sum(count*avg)/sum(count), 
                      pct90All = sum(max>=0.9)/.N, 
                      pct80All = sum(max>=0.8)/.N, 
                      pct50All = sum(max>=0.5)/.N),
               by = list(AgeRange)]
  tmp[, IncomeRange:="All"]
  tmp[, ProductType:="All"]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  
  # next do producttype and age
  tmp = ccJoin[, list(totCount = .N, 
                      avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                      pct90Requested = sum(requestedMax>=0.9)/.N, 
                      pct80Requested = sum(requestedMax>=0.8)/.N, 
                      pct50Requested = sum(requestedMax>=0.5)/.N,
                      avgAll = sum(count*avg)/sum(count), 
                      pct90All = sum(max>=0.9)/.N, 
                      pct80All = sum(max>=0.8)/.N, 
                      pct50All = sum(max>=0.5)/.N),
               by = list(ProductType, AgeRange)]
  tmp[, IncomeRange:="All"]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  # next do producttype and income
  tmp = ccJoin[, list(totCount = .N, 
                      avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                      pct90Requested = sum(requestedMax>=0.9)/.N, 
                      pct80Requested = sum(requestedMax>=0.8)/.N, 
                      pct50Requested = sum(requestedMax>=0.5)/.N,
                      avgAll = sum(count*avg)/sum(count), 
                      pct90All = sum(max>=0.9)/.N, 
                      pct80All = sum(max>=0.8)/.N, 
                      pct50All = sum(max>=0.5)/.N),
               by = list(ProductType, IncomeRange)]
  tmp[, AgeRange:="All"]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  # next do age and income
  tmp = ccJoin[, list(totCount = .N, 
                      avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                      pct90Requested = sum(requestedMax>=0.9)/.N, 
                      pct80Requested = sum(requestedMax>=0.8)/.N, 
                      pct50Requested = sum(requestedMax>=0.5)/.N,
                      avgAll = sum(count*avg)/sum(count), 
                      pct90All = sum(max>=0.9)/.N, 
                      pct80All = sum(max>=0.8)/.N, 
                      pct50All = sum(max>=0.5)/.N),
               by = list(AgeRange, IncomeRange)]
  tmp[, ProductType:="All"]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  # next do producttype age and income
  tmp = ccJoin[, list(totCount = .N, 
                      avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                      pct90Requested = sum(requestedMax>=0.9)/.N, 
                      pct80Requested = sum(requestedMax>=0.8)/.N, 
                      pct50Requested = sum(requestedMax>=0.5)/.N,
                      avgAll = sum(count*avg)/sum(count), 
                      pct90All = sum(max>=0.9)/.N, 
                      pct80All = sum(max>=0.8)/.N, 
                      pct50All = sum(max>=0.5)/.N),
               by = list(ProductType, AgeRange, IncomeRange)]
  ccAgg = rbindlist(list(ccAgg, tmp), use.names = T)
  
  
  ccJoin[1:10, list(DateOfSubmission, TimeOfSubmission)]
  
  hist(ccJoin[, DateOfSubmission], breaks = "days")
  plot(table(ccJoin[, DateOfSubmission]))
  plot(table(hour(ccJoin[, TimeOfSubmission])))
  
  plot(table(as.POSIXlt(ccJoin[, TimeOfSubmission])$hour))
  plot(table(as.POSIXlt(ccJoin[, TimeOfSubmission])$min))
  plot(table(as.POSIXlt(ccJoin[, TimeOfSubmission])$sec))
  
  
  
  library(leaflet)
  
  m6 = leaflet() %>% addTiles('http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',
                              attribution = paste(
                                '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
                                '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
                              )
  ) %>% setView(-6,53.52,  zoom = 5)
  mtest = m6 %>% 
    addCircleMarkers(tmpDateData[1, longitude], tmpDateData[1, latitude], color = "red", radius = 3)
  
  mtest
  
  
  
  # you may need to view the output in a browser- can also create stand alone html files
  
  
  map = leaflet() %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                               attribution = paste(
                                 '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
                                 '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
                               ) ) %>% setView(-6,53.52,  zoom = 5)
  
  mtest = map %>% 
    addCircleMarkers(tmpDateData[1, longitude], tmpDateData[1, latitude], popup = tmpDateData[1, id], color = "white", weight=5, radius = 5)
  
  mtest  
  maxI = 5
  
  
  for (i in 1:maxI) {
    #i=2
    mtest = map %>% 
      addCircleMarkers(tmpDateData[i, longitude], tmpDateData[i, latitude], popup - tmpDateData[i, id], color = "white", weight=5, radius = 5)
    
    mtest  
    Sys.sleep(1)
    
  }
  
  format(Sys.time(), "%S")
  
  
  
  #https://github.com/skeate/Leaflet.timeline/blob/master/examples/earthquakes.html
  #http://zevross.com/blog/2014/09/30/use-the-amazing-d3-library-to-animate-a-path-on-a-leaflet-map/
  
  
#json format  {"type":"Feature",
  #             "properties":{"mag":2.8,
  #                           "time":1448034152340,
  #                           "url":"http://earthquake.usgs.gov/earthquakes/eventpage/us10003zv0",
  #                           "title":"testing M 2.8 - 26km NNW of Fairview, Oklahoma"},
  #             "geometry":{"type":"Point",
  #                         "coordinates":[-6,53]}},
#type = Feature
#properties - mag = requestedMax or requestedAvg
#properties - time = TimeOfSubmission
#properties - url = ?
#properties - title = ProductType
#properties - IncomeRange
#properties - AgeRange
#geometry - type = "Point"
#geometry - coordinates = [longitude, latitude]
  
library(jsonlite)  

  
  
  ## processing data for geojson export
  
  tmpDateData = ccJoin[ DateOfSubmission == "2015-06-24", ]
  setkeyv(tmpDateData, c("DateOfSubmission", "TimeOfSubmission"))
  
  postCodeRaw = fread(postCodeFile)
  setkey(postCodeRaw, outcode)
  
  inPostCodes = sort(unique(tmpDateData[, PostcodeArea]))
  missingPostCodes = inPostCodes[!inPostCodes %in% postCodeRaw[, outcode]]
  
  tmpDateData[PostcodeArea %in% missingPostCodes, list(tot = .N), by = list(PostcodeArea)]
  tmpDateData[, list(tot = .N), by = list(PostcodeArea)][tot>7, ]
  
  setkey(tmpDateData, PostcodeArea)
  tmpDateData = postCodeRaw[tmpDateData, ]
  setkeyv(tmpDateData, c("DateOfSubmission", "TimeOfSubmission"))
  
  prodTypeLookup = data.table(ProductType = sort(unique(tmpDateData[, ProductType])), 
                              prodTypeId = seq(1,length(unique(tmpDateData[, ProductType])),1))
  setkey(prodTypeLookup, ProductType)
  setkey(tmpDateData, ProductType)
  tmpDateData = prodTypeLookup[tmpDateData, ]
  setkeyv(tmpDateData, c("DateOfSubmission", "TimeOfSubmission"))
  
  tmpDateData = tmpDateData[ !is.na(latitude), ]
  tmpDateData = tmpDateData[ !is.na(longitude), ]
  
  jsonOut = data.table(type = "Feature", 
                     mag = tmpDateData[, requestedMax], 
                     time = as.integer(tmpDateData[, TimeOfSubmission]),
                     tod = as.character(tmpDateData[, TimeOfSubmission]),
                     chardate = as.character.Date(tmpDateData[, DateOfSubmission]),
                     url = tmpDateData[, PostcodeSector],
                     prodType = tmpDateData[, prodTypeId],
                     title = paste(as.character(tmpDateData[, TimeOfSubmission]), ' ',
                                   tmpDateData[, ProductType], ' - ', 
                                   tmpDateData[, PostcodeSector], ' : ',
                                   round(tmpDateData[, requestedMax]*100), '%', sep=""),
                     IncomeRange = tmpDateData[, IncomeRange],
                     AgeRange = tmpDateData[, AgeRange],
                     type = "Point",
                     coordinates = paste("[",round(tmpDateData[, longitude]*1000)/1000,",",round(tmpDateData[, latitude]*1000)/1000,"]", sep="")
                     )  
  
    #test = jsonOut[1:10, ]
    #test = jsonOut
    idx = sample(2:(nrow(jsonOut)-1), 5000, replace = FALSE, prob = NULL)
    idx = c(1,sort(idx),nrow(jsonOut))
    test = jsonOut[idx, ]
    properties = test[, list(mag, time, prodType, url, title, tod, chardate)]
    geometry = data.table(type = "Point", coordinates = test[, coordinates])
  
    #create json
    i=1
  
    tmp = paste('{"type":"Feature","properties":',toJSON(properties[i, ], pretty = TRUE),',"geometry":',
            toJSON(geometry[i, ], pretty = TRUE),'}', sep="")
    tmp = gsub("\\:\\[",":",tmp)
    tmp = gsub("\\],",",",tmp)
    tmp = gsub("\\]}","}",tmp)
  
    for (i in 2:nrow(test)) {
      #i=2
      tmpi = paste('{"type":"Feature","properties":',toJSON(properties[i, ], pretty = TRUE),',"geometry":',
                   toJSON(geometry[i, ], pretty = TRUE),'}', sep="")
      tmpi = gsub("\\:\\[",":",tmpi)
      tmpi = gsub("\\],",",",tmpi)
      tmpi = gsub("\\]}","}",tmpi)
      
      tmp = paste(tmp, tmpi, sep=",")    
    }
    
    #clean up square brackets
    tmp = gsub('\\"\\[','\\[',tmp)
    tmp = gsub('\\]\\"','\\]',tmp)
  
    tmp = paste('{"type":"FeatureCollection","features":[',tmp,']}', sep="")
    #validate(tmpi)
    #double check all is ok
    validate(tmp)
  
    #export data json
    sink(paste("ccFinalSamp5k.json", sep=""))
    cat('eqfeed_callback(',tmp,');', sep="")
    sink()
  
  
    
    
    
    
    
  save(ccJoin, ccAgg, ageData, incomeData, typeData, ccProductAgg, ccScoreProductAgg6, file = "eligibility.Rdata")
  load("eligibility.Rdata")
  
  ccScoreProductAgg6[, list(total = sum(total)), by = list(ProductType, Product)]
  
  
  
  
  
  ccJoinAgg = ccJoin[, list(totCount = .N, 
                            totRequestedNum = sum(requestedNum),
                            avgRequestedNum = sum(requestedNum)/.N,
                            avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                            pct90Requested = sum(requestedMax>=0.9)/.N, 
                            pct80Requested = sum(requestedMax>=0.8)/.N, 
                            pct50Requested = sum(requestedMax>=0.5)/.N),
                     by = list(ProductType, AgeRange)]
  
  
  
  
  #agg by product type and age
  ccJoinAgg = ccJoin[, list(totCount = .N, 
                totRequestedNum = sum(requestedNum),
                avgRequestedNum = sum(requestedNum)/.N,
                avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                pct90Requested = sum(requestedMax>=0.9)/.N, 
                pct80Requested = sum(requestedMax>=0.8)/.N, 
                pct50Requested = sum(requestedMax>=0.5)/.N),
          by = list(ProductType, AgeRange)]
  
  ccJoinAgg[, pct90Requested]
  
  ccJoinAgg[ ProductType == "Balance Transfer" & AgeRange == "(10,20]", ]
  ccJoin[ ProductType == "Balance Transfer" & AgeRange == "(10,20]", ]
  sum(ccJoin[ ProductType == "Balance Transfer" & AgeRange == "(10,20]", requestedMax]>=0.9) / 
  nrow(ccJoin[ ProductType == "Balance Transfer" & AgeRange == "(10,20]", ])  
  
  
  ggplot(ccJoinAgg, aes(x=AgeRange, y=ProductType, size = log(totCount+1), colour=pct90Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  ggplot(ccJoinAgg, aes(x=AgeRange, y=ProductType, size = log(totCount+1), colour=pct80Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  ggplot(ccJoinAgg, aes(x=AgeRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  
  ggplot(ccJoinAgg, aes(x=AgeRange, y=ProductType, size = log(totCount+1), colour=avgRequested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  ## double check some figures
  nrow(ccJoin[requestedMax<0.5,]) / nrow(ccJoin)
  nrow(ccProductsAgg[max<.5,]) / nrow(ccProductsAgg)
  
  #agg by product type and income
  ccJoinAgg2 = ccJoin[, list(totCount = .N, 
                            totRequestedNum = sum(requestedNum),
                            avgRequestedNum = sum(requestedNum)/.N,
                            avgRequested = sum(requestedNum*requestedAvg)/sum(requestedNum), 
                            pct90Requested = sum(requestedMax>=0.9)/.N, 
                            pct80Requested = sum(requestedMax>=0.8)/.N, 
                            pct50Requested = sum(requestedMax>=0.5)/.N),
                     by = list(ProductType, IncomeRange)]
  
  
  
  ggplot(ccJoinAgg2, aes(x=IncomeRange, y=ProductType, size = log(totCount+1), colour=pct90Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  ggplot(ccJoinAgg2, aes(x=IncomeRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green")
  
  
  
  ggplot(ccJoin, aes(x=AgeRange)) +
    geom_histogram() 
  
  
  ccJoin[ AgeRange == "(10,20]", ]
  
  ggplot(ccJoin, aes(x=Income, y=requestedAvg)) +
    geom_point() 
  
  ggplot(ccJoin, aes(x=avg)) +
    geom_density() 
  
  ggplot(ccJoin, aes(x=max)) +
    geom_density() 
  
  ggplot(ccJoin, aes(x=max)) +
    geom_histogram(bin = 0.1) 
  
  ggplot(ccJoin, aes(x=requestedAvg)) +
    geom_density() 
  
  ## create plot dt
  library(reshape2)
  
  ccPlot = melt(ccJoin,
                 # ID variables - all the variables to keep but not split apart on
                 id.vars=c("SubmissionID", "ProductType", "IncomeRange"),
                 # The source columns
                 measure.vars=c("max", "avg", "requestedMax", "requestedAvg"),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 #variable.name="submitter",
                 value.name="score"
  )
  
  
  ggplot(ccPlot, aes(x=ProductType, y=IncomeRange)) +
    geom_point()
  
  
  ggplot(ccPlot, aes(x=score, fill=variable)) +
    geom_histogram(binwidth=.05, alpha=.5, position="dodge")
  
  ggplot(ccPlot, aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  
  ggplot(ccPlot[ variable %in% c("avg", "requestedAvg"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5)
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_histogram(binwidth=.1, alpha=.5, position="identity")
  
  
  ggplot(ccPlot[ variable %in% c("max", "requestedMax"), ], aes(x=score, fill=variable)) +
    geom_density(alpha=.5) +
    facet_wrap(ProductType ~ .)
  
  ggplot(ccJoin, aes(x=requestedMax)) +
    geom_histogram(bin=0.1) 
  
  ggplot(ccJoin, aes(x=requestedAvg)) +
    geom_density() 
  
  
  
## end explore products
#################################################################################

#################################################################################
## story

  ## first compare MSE Credit Card searchers (June 2015) to uk population
  ## Finding- similar income (slight over index 20-30k); age younger
  
  #plots of mse cc vs uk pop
  
  incomePlot = ggplot(incomeData[ group!="MSE_LowScore", ],aes(x=IncomeRange, y=proportion, group=group, colour=group)) + 
    geom_line(size=1)
  incomePlot + geom_histogram(aes(x=IncomeRange, y=proportion, fill=group), stat="identity", position="identity", alpha=0.2) +
    theme_minimal() +
    ggtitle("Income distribution of MSE Credit Card Smart Searchers vs UK Adult Population") +
    annotate("text", x = 12, y = 0.3, label = "MSE CC eligibility users pretty similar to UK population- slight over index in 20-30k")
  
  agePlot = ggplot(ageData[ group!="MSE_LowScore", ], aes(x=AgeRange, y=proportion, group=group, colour=group)) + 
    geom_line(size=1)
  agePlot + geom_histogram(aes(x=AgeRange, y=proportion, fill=group), stat="identity", position="identity", alpha=0.2) +
    theme_minimal() +
    ggtitle("Age distribution of MSE Credit Card Smart Searchers vs UK 16+ Population") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 6, y = 0.3, label = "MSE CC eligibility users younger than UK adult population")

  
  ## What are they applying for?
  #ggplot(ccJoin, aes(x=ProductType, fill = ProductType)) +  
  #  geom_bar(aes(y = (..count..)/sum(..count..))) +
  #  scale_y_continuous(labels=percent, name="Percent searches") +
  #  scale_fill_discrete(guide=FALSE) +
  #  theme_minimal() +
  #  ggtitle("Distribution of MSE Credit Card Smart Searchers by product type") +
  #  annotate("text", x = 3, y = 0.4, label = "MSE CC eligibility usrs most likely to search for Balance Transfer cards")
  
  
  ggplot(typeData[group == 'MSE_CC_Eligibility', ], aes(x=ProductType, y=proportion, fill = ProductType)) +  
    geom_bar(stat="identity") +
    scale_y_continuous(labels=percent, name="Percent searches") +
    scale_fill_discrete(guide=FALSE) +
    theme_minimal() +
    ggtitle("Distribution of MSE Credit Card Smart Searchers by product type") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 3, y = 0.4, label = "MSE CC eligibility usrs most likely to search for Balance Transfer cards")
  
  
  ## Are they successful?
  ggplot(ccAgg[AgeRange=="All" & IncomeRange=="All", ], aes(x=ProductType, fill = ProductType)) +  
    geom_bar(aes(y = pct90Requested), stat="identity") +
    scale_y_continuous(labels=percent, name="Pct searches with a requested liklihood >90%") +
    scale_fill_discrete(guide=FALSE) +
    theme_minimal() +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of success by rquested product type") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 3, y = 0.99, label = "MSE CC eligibility users looking for Bad Credit cards are most likely to be accepted")
  
    
  
  ## Could everyone get a card?
  ggplot(ccAgg[AgeRange=="All" & IncomeRange=="All", ], aes(x=ProductType, fill = ProductType)) +  
    geom_bar(aes(y = pct90All), stat="identity") +
    scale_y_continuous(labels=percent, name="Pct searches with any liklihood >90%") +
    scale_fill_discrete(guide=FALSE) +
    theme_minimal() +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of any success by any product type searched for") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 3, y = 1.05, label = "Any MSE CC eligibility user could get a credit card")
  
  
## how does likelihood vary by income
  
  ggplot(ccAgg[AgeRange=="All" , ], aes(x=IncomeRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green",name = "Pct with response >50%") + 
    scale_size(guide=FALSE) +
    theme_minimal() +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of requested acceptance by income") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 8, y = 10.5, label = "Limited direct relationship between Income and likelihood of acceptance")
  
  
  
  ## how does likelihood vary by age
  
  ggplot(ccAgg[IncomeRange=="All" , ], aes(x=AgeRange, y=ProductType, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green",name = "Pct with response >50%") + 
    scale_size(guide=FALSE) +
    theme_minimal() +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of requested acceptance by age") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 5, y = 10.5, label = "Limited direct relationship between Age and likelihood of acceptance")
  
  #overall how does it vary by age and income
  
  ggplot(ccAgg[AgeRange!="All" & IncomeRange!="All", ], aes(x=IncomeRange, y=AgeRange, size = log(totCount+1), colour=pct50Requested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green",name = "Pct with response >50%") + 
    scale_size(guide=FALSE) +
    theme_minimal() +
    facet_wrap(~ProductType) +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of requested acceptance by age, income and product type")
  
  
  ggplot(ccAgg[AgeRange!="All" & IncomeRange!="All", ], aes(x=IncomeRange, y=AgeRange, size = log(totCount+1), colour=avgRequested)) +
    geom_point() + 
    scale_colour_gradient(low = "red", high = "green",name = "Avg requested") + 
    scale_size(guide=FALSE) +
    theme_minimal() +
    facet_wrap(~ProductType) +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    ggtitle("MSE Credit Card Smart Searchers - likelihood of requested acceptance by age, income and product type")
  
  
  
  #Who are those people who are not able to get the card they want?
  
  incomePlot = ggplot(incomeData[ group!="UK_Pop", ],aes(x=IncomeRange, y=proportion, group=group, colour=group)) + 
    geom_line(size=1)
  incomePlot + geom_histogram(aes(x=IncomeRange, y=proportion, fill=group), stat="identity", position="identity", alpha=0.2) +
    theme_minimal() +
    ggtitle("Income distribution of low score MSE Credit Card Smart Searchers vs All MSE Credit Card Smart Searchers") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 12, y = 0.3, label = "Limited income differences between low scorers and average")
  
  agePlot = ggplot(ageData[ group!="UK_Pop", ], aes(x=AgeRange, y=proportion, group=group, colour=group)) + 
    geom_line(size=1)
  agePlot + geom_histogram(aes(x=AgeRange, y=proportion, fill=group), stat="identity", position="identity", alpha=0.2) +
    theme_minimal() +
    ggtitle("Age distribution of low score MSE Credit Card Smart Searchers vs All MSE Credit Card Smart Searchers") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 6, y = 0.3, label = "MSE CC low scorers skew slightly higher on income than average")
  
  typePlot = ggplot(typeData[ group!="UK_Pop", ], aes(x=ProductType, y=proportion, group=group, colour=group)) + 
    geom_line(size=1)
  typePlot + geom_histogram(aes(x=ProductType, y=proportion, fill=group), stat="identity", position="identity", alpha=0.2) +
    theme_minimal() +
    ggtitle("Product Type distribution of low score MSE Credit Card Smart Searchers vs All MSE Credit Card Smart Searchers") +
    theme(plot.title = element_text(lineheight=.9, face="bold")) +
    annotate("text", x = 6, y = 0.5, label = "MSE CC low scorers skew towards Money Tranfer and Travel Money")
  
  
  ggplot(typeData[group == 'MSE_CC_Eligibility', ], aes(x=ProductType, y=proportion, fill = ProductType)) +  
    geom_bar(stat="identity") +
    scale_y_continuous(labels=percent, name="Percent searches") +
    scale_fill_discrete(guide=FALSE) +
    theme_minimal() +
    ggtitle("Distribution of MSE Credit Card Smart Searchers by product type") +
    annotate("text", x = 3, y = 0.4, label = "MSE CC eligibility usrs most likely to search for Balance Transfer cards")
  
  
  ### playing around with brute force parcor
  ccScoreTop1Agg
  
  
  
  
## end story
#################################################################################
  
  
#################################################################################
## model
    
## split into train and test
set.seed(1234)
n = nrow(homeDT)  
trainPct = 0.80 # but keep last 3 months in test as well

trainId = sort(sample(seq(1,n-3,1), floor(trainPct*n), replace=FALSE))
trainDiffId = (trainId-1)[(trainId-1)>0]

trainCar = carDT[trainId, ]
testCar = carDT[-trainId, ]
trainDiffCar = carDiffDT[trainIdDiff, ]
testDiffCar = carDiffDT[-trainIdDiff, ]
trainHome = homeDT[trainId, ]
testHome = homeDT[-trainId, ]
trainDiffHome = homeDiffDT[trainDiffId, ]
testDiffHome = homeDiffDT[-trainDiffId, ]



library(ggplot2)
library(GGally)
library(scales)

# lets explore the correlations and distributions of the variables
# matrix scatter plot

homeVars = setdiff(colnames(trainHome), "date")
carVars = setdiff(colnames(trainCar), "date")
homeVarsTgt = c("homeinsurance", "houseinsurance", "comparehomeinsurance", "sov", "share")
carVarsTgt = c("carinsurance", "cheapcarinsurance", "comparecarinsurance", "sov", "share")

ggpairs(trainHome[, homeVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

ggpairs(trainDiffHome[, homeVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")


ggpairs(trainCar[, carVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

ggpairs(trainDiffCar[, carVarsTgt, with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



ggplot(trainHome[date>'2014-01-01', ], aes(x=homeinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainHome[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffHome[date>'2014-01-01', ], aes(x=homeinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffHome[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()


ggplot(trainCar[date>'2012-01-01', ], aes(x=carinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainCar[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()


ggplot(trainDiffCar[date>'2012-01-01', ], aes(x=carinsurance, y=share, colour = as.factor(date))) +
  geom_point()

ggplot(trainDiffCar[date>'2012-01-01', ], aes(x=sov, y=share, colour = as.factor(date))) +
  geom_point()

#ggpairs(trainHome[, c(colnames(rawData)[grep("bs", colnames(rawData))], "y"), with=FALSE], 
#        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



#### simple modelling
library(caret)

## focus on insample to start with to save out of sample
predictionsCar = data.table(date = carDT[, date], actuals = carDT[, share],
                            mean = mean(carDT[, share]), last = c(0.2, carDT[1:(nrow(carDT)-1), share]))

predictionsCarDiff = data.table(date = carDiffDT[, date], actuals = carDiffDT[, share],
                            mean = mean(carDiffDT[, share]), last = c(0.0, carDiffDT[1:(nrow(carDiffDT)-1), share]))

predictionsHome = data.table(date = homeDT[, date], actuals = homeDT[, share],
                            mean = mean(homeDT[, share]), last = c(0.2, homeDT[1:(nrow(homeDT)-1), share]))

predictionsHomeDiff = data.table(date = homeDiffDT[, date], actuals = homeDiffDT[, share],
                                mean = mean(homeDiffDT[, share]), last = c(0.0, homeDiffDT[1:(nrow(homeDiffDT)-1), share]))


## start with a simple linear regression 
tmpModelglm = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                    preProcess = c("scale", "center"),  
                    method = "glm")
tmp = predict(tmpModelglm, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, glmbase := tmp]
rm(tmpModelglm)

tmpModelglm = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                    preProcess = c("scale", "center"),  
                    method = "glm")
tmp = predict(tmpModelglm , newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, glmbase := tmp]
rm(tmpModelglm)

testCar = genTest(predictionsCar[trainId, !"date", with=FALSE])
testCarDiff = genTest(predictionsCarDiff[trainId, !"date", with=FALSE])

# glmnet and random forest
fitControl <- trainControl(## should nortmally do 10-fold CV, will do 5 for speed
  method = "repeatedcv",
  number = 5,
  ## repeated 2 times
  repeats = 2)

tmpModelglmnet = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'glmnet',
                       trControl = fitControl)
tmp = predict(tmpModelglmnet, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, glmnet := tmp]
rm(tmpModelglmnet)


tmpModelglmnet = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'glmnet',
                       trControl = fitControl)
tmp = predict(tmpModelglmnet, newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, glmnet := tmp]
rm(tmpModelglmnet)


tmpModelrf = train(share ~ ., data = trainCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'rf',
                       trControl = fitControl)
tmp = predict(tmpModelrf, newdata = carDT[, carVars, with=FALSE])
predictionsCar[, rf := tmp]
rm(tmpModelrf)


tmpModelrf = train(share ~ ., data = trainDiffCar[, carVars, with=FALSE],
                       preProcess = c("scale", "center"),  
                       method = 'rf',
                       trControl = fitControl)
tmp = predict(tmpModelrf, newdata = carDiffDT[, carVars, with=FALSE])
predictionsCarDiff[, rf := tmp]
rm(tmpModelrf)


testCar = genTest(predictionsCar[trainId, !"date", with=FALSE])
testCarDiff = genTest(predictionsCarDiff[trainDiffId, !"date", with=FALSE])



testCarOOS = genTest(predictionsCar[-trainId, !"date", with=FALSE])
testCarDiffOOS = genTest(predictionsCarDiff[-trainDiffId, !"date", with=FALSE])

library(reshape2)

tmpPlot = melt(predictionsCar[trainId, ],
               # ID variables - all the variables to keep but not split apart on
               id.vars=c("date"),
               # The source columns
               measure.vars=setdiff(colnames(predictionsCar), "date"),
               # Name of the destination column that will identify the original
               # column that the measurement came from
               variable.name="model",
               value.name="share"
)

ggplot(tmpPlot, aes(x=date, y = share, colour = model)) + geom_line()

## end model
#################################################################################
  
   

#################################################################################
## performance function

genTest = function(pred) {
  
  require(data.table)
  
  #assume input includes results from models including one column "actuals"
  models = setdiff(colnames(pred), c("actuals"))
  nModels = length(models)
  rawAD = abs(pred[, models, with=FALSE] -
                pred[, rep("actuals",nModels), with=FALSE])
  rawSD = as.data.table((pred[, models, with=FALSE] -
             pred[, rep("actuals",nModels), with=FALSE])^2)
  
  summaryDT = data.table(modelName = models)
  summaryDT[, mad:= colMeans(rawAD)]
  summaryDT[, madMin:= lapply(rawAD, min)]
  summaryDT[, madMax:= lapply(rawAD, max)]
  summaryDT[, madsd:= lapply(rawAD, sd)]
  
  summaryDT[, mse:= colMeans(rawSD)]
  summaryDT[, mseMin:= lapply(rawSD, min)]
  summaryDT[, mseMax:= lapply(rawSD, max)]
  summaryDT[, msesd:= lapply(rawSD, sd)]
  
  return(list(summaryDT,rawAD,rawSD))

}

## end performance function
#################################################################################

library(data.table)
tmp = data.table(id = seq(1,10000000,1))
tmp[, a:="char"]
tmp[, b:="char"]
tmp[, c:="char"]
tmp[, d:="char"]
tmp[, e:="char"]
tmp[, f:="char fill 2"]
tmp[, g:=0]
tmp[, h:=1]
tmp[, i:=0]
tmp[, j:=1]
tmp[, k:=0]
tmp[, l:=0]
tmp[, m:=0]
tmp[, n:=40241]
tmp[, o:=10000]

save(tmp, file="test10mrows.Rdata")
write.table(tmp, "test10mrows.txt", sep="\t", row.names=F, col.names = T)

load("test10mrows.Rdd")
tmp2 = fread("test10mrows.txt")

sort( sapply(ls(),function(x){object.size(get(x))}))

