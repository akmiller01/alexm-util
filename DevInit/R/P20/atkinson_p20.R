library(data.table)
source("C:/git/alexm-util/DevInit/R/P20/atkinson.R")

# df <- data.frame(group=c("a","a","a","b","b","b"),L=c(0.3,0.1,1,0.2,0.4,1))
# dt <- data.table(df)
# setkey(dt,group,L)
# dt[,diff:=c(L[1],diff(L)),by=group]

dat <- read.csv("D:/Documents/Data/PovCal_Increment/years/pcn.2013.csv")
keep <- c("country","pl","hc")
dat <- dat[keep]

incomeGivenPercent <- function(perc,plV,hcV){
  minDiff <- 100
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
  incomes <- sapply(seq(1,100),incomeGivenPercent,plV=ss$pl,hcV=ss$hc)
  ai <- atkinson(incomes)
  aiDF <- data.frame(country=ctry,atkinson=ai,year=2013)
  dataList[[dataIndex]] <- aiDF
  dataIndex <- dataIndex + 1
}
data <- rbindlist(dataList)
write.csv(data,"atkinson.csv",row.names=FALSE)