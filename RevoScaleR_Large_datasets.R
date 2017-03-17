#### 
###   Belgium SQL User group
###   R and SQL Server 2016
###   Tomaž Kaštrun; 16.MARCH, 2017
####


setwd('C:/DataTK')

library(RevoScaleR)

#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#s
#
#     LOADING DATA (small sample)
#     178 MB 
#     8,4Mio Rows
#
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set sample dir
rxOptions(sampleDataDir = "C:/Program Files/Microsoft SQL Server/130/R_SERVER/library/RevoScaleR/SampleData")


ptm <- proc.time()
inFile <- file.path(rxGetOption("sampleDataDir"), "airsample.csv")
rxTextToXdf(inFile = inFile, outFile = "airline.xdf",  stringsAsFactors = T, rowsPerRead = 200000, overwrite=TRUE)
proc.time() - ptm
# ~ 25 seconds!
# - 42 Chunks per 200.000 Rows; Total: 8.400.000 Rows


##################################
#   EXPLORING DATA (small sample)
##################################


rxGetInfo(data="airline.xdf", getVarInfo = TRUE, numRows = 5)

#Histograms by day of week
ptm <- proc.time()
rxHistogram( ~ ArrDelay|DayOfWeek, data = "airline.xdf")
proc.time() - ptm
# ~ 1 second!

#summary
rxSummary( ~ ArrDelay, data = "airline.xdf")

#Sorting Data
rxSort(inData="airline.xdf", outFile = "sortFlights.xdf", sortByVars="ArrDelay",  decreasing = TRUE,overwrite=TRUE) 
# ~ 4 Seconds!



######################################
#   Linear Model with ReportProgress!
######################################

# Linear Model using rxLinMod
sampleDataDir <- rxGetOption("sampleDataDir")
airlineDemoSmall <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")

airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 1)
airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 2)
airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 2, blocksPerRead = 3)

help(rxLinMod)

summary(airlineLinMod)

# use multiple dependent variables in model formula
# print and summarize results for comparison

airlineLinMod2 <- rxLinMod(cbind(ArrDelay, CRSDepTime) ~ DayOfWeek,data = airlineDemoSmall)


airlineLinMod2
summary(airlineLinMod2)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#     LOADING DATA (large sample)
#     # 13GB File
#     # 123 Mio rows
#
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# download data: https://packages.revolutionanalytics.com/datasets/
# 

ptm <- proc.time()
airlineDemoBig <- file.path(rxGetOption("sampleDataDir"), "AirlineData87to08.xdf")
#rxTextToXdf(inFile = inFile, outFile = "AirlineData87to08.xdf",  stringsAsFactors = T, rowsPerRead = 200000)
proc.time() - ptm


#calculate simple linear regression
airlineLinMod2 <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoBig, reportProgress = 2, blocksPerRead = 3)
summary(airlineLinMod2)


#plot a histogram
ptm <- proc.time()
#rxHistogram( ~ ArrDelay|DayOfWeek, data = "C:/Program Files/Microsoft SQL Server/130/R_SERVER/library/RevoScaleR/SampleData/AirlineData87to08.xdf")
rxHistogram( ~ ArrDelay|DayOfWeek, data = airlineDemoBig)
proc.time() - ptm
# ~ 44 seconds!
# - 823 Chunks per 150.000 Rows; Total: 123.534.969 Rows

