library(data.table)
source("C:/git/alexm-util/DevInit/R/P20/atkinson.R")

# dat <- read.csv("D:/Documents/Data/PovCal_Increment/years/pcn.2013.csv")
load("D:/Documents/Data/PovCal_Increment/pcn.RData")
keep <- c("country","year","pl","hc")
dat <- data.frame(pcn)[keep]
dat <- dat[complete.cases(dat),]

incomeGivenPercent <- function(perc,plV,hcV){
  minDiff <- 1000
  minIndex <- 0
  for(i in 1:length(hcV)){
    hc <- hcV[i]
    diff <- abs(hc-perc)
    if(diff < minDiff){
      minDiff <- diff
      minIndex <- i
    }
  }
  return(plV[minIndex])
}


countries <- unique(dat$country)
dataList <- list()
dataIndex <- 1
for(ctry in countries){
  message(ctry)
  ss <- subset(dat,country==ctry)
  years <- unique(ss$year)
  for(yr in years){
    message(yr)
    subss <- subset(ss,year==yr)
    incomes <- sapply(seq(1,100),incomeGivenPercent,plV=subss$pl,hcV=subss$hc)
    ai <- atkinson(incomes)
    aiDF <- data.frame(country=ctry,atkinson=ai,year=yr)
    dataList[[dataIndex]] <- aiDF
    dataIndex <- dataIndex + 1
  }
}
data <- rbindlist(dataList)
write.csv(data,"atkinson_over_time.csv",row.names=FALSE)