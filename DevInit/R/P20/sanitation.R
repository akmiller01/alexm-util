library(data.table)
library(plyr)
library(Hmisc)
library(varhandle)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)

load("gcw.RData")

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

gcw$wealth <- gcw$old.wealth
yes <- c("yes",1,"1","Yes","yes, public","yes, other household only","Oui")
no <- c("no",0,"0","No","Non")
missing <- c(9,"9","Missing","Manquant",NA)
gcw$share.toilets <- unfactor(gcw$share.toilets)
gcw$share.toilets[which(gcw$share.toilets %in% yes)] <- TRUE
gcw$share.toilets[which(gcw$share.toilets %in% no)] <- FALSE
gcw$share.toilets[which(gcw$share.toilets %in% missing)] <- NA
gcw$share.toilets[which(!(gcw$share.toilets %in% c(TRUE,FALSE,NA)))] <- TRUE

wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

dataList <- list()
dataIndex <- 1

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

countries <- unique(gcw$filename)
for(i in 1:length(countries)){
  country <- countries[i]
  message(country)
  dat <- subset(gcw,filename==country)
  povcalcut <- subset(povcalcuts,filename==country)$hc
  povperc <- weighted.percentile(dat$wealth,dat$weights,prob=povcalcut)
  dat$p20 <- (dat$wealth < povperc)
  dataList[[dataIndex]] <- dat
  dataIndex = dataIndex + 1
}

gcw.p20 <- rbindlist(dataList)

gcw.p20$water.binary <- NA
gcw.p20$water.binary[which(gcw.p20$ade.water==TRUE)] <- 1
gcw.p20$water.binary[which(gcw.p20$ade.water==FALSE)] <- 0
gcw.p20$toilets.binary <- NA
gcw.p20$toilets.binary[which(gcw.p20$ade.toilets==TRUE)] <- 1
gcw.p20$toilets.binary[which(gcw.p20$ade.toilets==FALSE)] <- 0
gcw.p20$share.toilets.binary <- NA
gcw.p20$share.toilets.binary[which(gcw.p20$share.toilets==TRUE)] <- 1
gcw.p20$share.toilets.binary[which(gcw.p20$share.toilets==FALSE)] <- 0

sani.tab <- gcw.p20[,.(
  ade.water=weighted.mean(water.binary,weights,na.rm=TRUE)
  ,ade.toilets=weighted.mean(toilets.binary,weights,na.rm=TRUE)
  ,share.toilets=weighted.mean(share.toilets.binary,weights,na.rm=TRUE)
  ),by=.(filename,p20)]
write.csv(sani.tab,"sani.tab.csv",row.names=FALSE,na="")

sani.tab <- join(
  sani.tab,
  povcalcuts[c("filename","hc","pop.total")],
  by="filename")

sani.tab <- transform(sani.tab,pop.segment =
                        p20*(hc*pop.total) +
                        (!p20)*((1-hc)*pop.total)
                        )
global.sani <- subset(sani.tab,!is.na(p20))[,.(
                          ade.water=sum(ade.water*pop.segment,na.rm=TRUE)
                          ,inade.water=sum((1-ade.water)*pop.segment,na.rm=TRUE)
                          ,ade.toilets=sum(ade.toilets*pop.segment,na.rm=TRUE)
                          ,inade.toilets=sum((1-ade.toilets)*pop.segment,na.rm=TRUE)
                          ,share.toilets=sum(share.toilets*pop.segment,na.rm=TRUE)
                          ,not.share.toilets=sum((1-share.toilets)*pop.segment,na.rm=TRUE)
                          ),by=.(p20)]
write.csv(global.sani,"global.sani.csv",row.names=FALSE,na="")
