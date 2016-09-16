####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(WDI)
library(varhandle)
require(zoo)

indicator <- "SI.POV.NAHC"

pov <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

source("C:/git/alexm-util/DevInit/R/P20/povcal_api3.R")

pov <- pov[c("iso3c","year","SI.POV.NAHC")]
names(pov) <- c("iso3","year","hc")
pov$hc <- pov$hc/100
pov <- pov[order(pov$year),]
pov <- pov[order(pov$iso3),]
colname <- "hc"
pov <- ddply(pov,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival 
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(pov) <- c("iso3","year","pl.hc","pl.hc.original")
pov <- pov[c("iso3","year","pl.hc")]
names(pov) <- c("iso3","year","poverty.line")

pov <- subset(pov,year==2012 & !is.na(iso3))
pov$p20 <- sapply(pov$iso3,povcal.p20)
pov$int.pl <- sapply(pov$iso3,povcal.int)
pov$p20.of.int <- sapply(pov$iso3,povcal.mmd)

write.csv(pov,"D:/Documents/Data/compare.pov.csv",row.names=FALSE,na="")


