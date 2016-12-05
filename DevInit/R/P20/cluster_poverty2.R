library(foreign)
library(plyr)
library(data.table)
library(varhandle)
library(Hmisc)

wd <- "D:/Documents/Data/DHS map"
setwd(wd)

all.years <- read.csv("D:/Documents/Data/MICSmeta/all.years.csv",as.is=TRUE)
all.years <- subset(all.years,substr(filename,7,8)=="dt")
all.years$DHSCC <- toupper(substr(all.years$filename,1,2))
names(all.years)[which(names(all.years)=="year")] <- "DHSYEAR"
all.years <- ddply(all.years,.(DHSCC,DHSYEAR),function(x)
{
  for(i in 1:length(x$DHSYEAR))
  {
    latestName <- x$filename[i]
  }
  y <- latestName
  return(y)
}
)

names(all.years) <- c("DHSCC","DHSYEAR","filename")

all.years$filename[which(all.years$filename=="ughr6adt")] <- "ughr60dt"
all.years$filename[which(all.years$filename=="drhr6adt")] <- "drhr61dt"
all.years$DHSYEAR[which(all.years$DHSCC=="TG" & all.years$DHSYEAR==2014)] <- 2013
all.years$DHSYEAR[which(all.years$DHSCC=="KE" & all.years$DHSYEAR==2009)] <- 2008
all.years$DHSYEAR[which(all.years$DHSCC=="SN" & all.years$DHSYEAR==2013)] <- 2012
all.years <- rbind(all.years,data.frame(DHSCC="ID",DHSYEAR=2003,filename="idhr42dt"))
all.years <- rbind(all.years,data.frame(DHSCC="CF",DHSYEAR=1994,filename="cfhr31dt"))
all.years$DHSYEAR[which(all.years$DHSCC=="HN" & all.years$DHSYEAR==2012)] <- 2011
all.years <- rbind(all.years,data.frame(DHSCC="NI",DHSYEAR=1998,filename="nihr31dt"))
all.years <- rbind(all.years,data.frame(DHSCC="MA",DHSYEAR=2003,filename="mahr43dt"))
all.years$DHSYEAR[which(all.years$DHSCC=="MD" & all.years$DHSYEAR==2009)] <- 2008

dat <- read.dbf("aggregate_clusters/clusters.dbf")
dat$filename <- NULL
dat$year <- NULL
dat$p20 <- NULL
dat$stunted.children <- NULL
dat$measured.children <- NULL
dat$stunted <- NULL

dat <- join(dat,all.years,by=c("DHSCC","DHSYEAR"))

filenames <- unique(dat[c("filename","DHSYEAR")])
filenames <- filenames[complete.cases(filenames),]

isos <- read.csv("D:/Documents/Data/DHS map/isos.csv",as.is=TRUE)

povcal <- function(dhscc,year){
  library(curl)
  yearDict <- list(
    "1990"=1990
    ,"1991"=1990
    ,"1992"=1993
    ,"1993"=1993
    ,"1994"=1993
    ,"1995"=1996
    ,"1996"=1996
    ,"1997"=1996
    ,"1998"=1999
    ,"1999"=1999
    ,"2000"=1999
    ,"2001"=2002
    ,"2002"=2002
    ,"2003"=2002
    ,"2004"=2005
    ,"2005"=2005
    ,"2006"=2005
    ,"2007"=2008
    ,"2008"=2008
    ,"2009"=2008
    ,"2010"=2011
    ,"2011"=2011
    ,"2012"=2012
    ,"2013"=2013
    ,"2014"=2013
    ,"2015"=2013
    ,"2016"=2013
    ,"2017"=2013
  )
  isos <- read.csv("D:/Documents/Data/DHS map/isos.csv",as.is=TRUE)
  cuts <- read.csv("D:/Documents/Data/DHS map/povcuts.csv",na.strings="",as.is=TRUE)
  sub <- subset(isos,cc==dhscc)
  #   subcuts <- subset(cuts,DHSYEAR==year)
  subcuts <- subset(cuts,DHSYEAR==yearDict[as.character(year)])
  if(nrow(sub)>0){
    iso3 <- sub$iso3[1]
  }else{
    iso3 <- readline(prompt=paste0("Enter ISO3 for ",dhscc,": "))
  }
  if(nrow(subcuts)>0){
    cut <- subcuts$cutpoint[1]
  }else{
    cut <- as.numeric(readline(prompt=paste0("Enter cutpoint for ",year,": ")))
  }
  url <- paste0(
    "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
    ,iso3
    ,"_3"
    ,"&PPP0=0&PL0="
    ,cut
    ,"&Y0="
    ,yearDict[as.character(year)]
    ,"&NumOfCountries=1"
  )
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  closeAllConnections()
  grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
  if(length(grepResults)>0){
    datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
    if(datum>1){
      datum <- datum/100
    }
  }else{
    datum <- NA
  }
  return(datum)
}

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

dataList <- list()
dataIndex <- 1

###One worker assumption for households outside of IR
stunted.children <- function(df){
  stunted.column <- c()
  for(i in 1:nrow(df)){
    stunted = 0
    for(j in 1:99){
      stuntingVar <- paste0("hc5_",j)
      if(typeof(df[[stuntingVar]])!="NULL" & typeof(df[[stuntingVar]])!="character"){
        h4a <- df[[stuntingVar]][i]
        if(!is.na(h4a)){
          h4a <- h4a/100
          if(h4a<=-2){
            stunted = stunted + 1 
          }
        }
      }else{
        break;
      }
    }
    stunted.column = c(stunted.column,stunted)
  }
  return(stunted.column)
}

mean.h4a <- function(df){
  stunted.column <- c()
  for(i in 1:nrow(df)){
    stunted.sum = 0
    n <- 0
    for(j in 1:99){
      stuntingVar <- paste0("hc5_",j)
      if(typeof(df[[stuntingVar]])!="NULL" & typeof(df[[stuntingVar]])!="character"){
        h4a <- df[[stuntingVar]][i]
        if(!is.na(h4a)){
          h4a <- h4a/100
          if(h4a<90){
            stunted.sum = stunted.sum + h4a
            n <- n + 1 
          }
        }
      }else{
        break;
      }
    }
    stunted.column = c(stunted.column,(stunted.sum/n))
  }
  return(stunted.column)
}

calc.stunting.perc <- function(stunted.c,measured.c){
  result.c <- c()
  for(i in 1:length(stunted.c)){
    stunted = stunted.c[i]
    measured = measured.c[i]
    
    if(is.na(measured)){
      result.c <- c(result.c,NA)
    }else if(is.na(stunted)){
      result.c <- c(result.c,NA)
    }else if(measured==0){
      result.c <- c(result.c,0)
    }else{
      result.c <- c(result.c,stunted/measured)
    }
  }
  return(result.c)
}

# cf <- read.csv("D:/Documents/Data/DHSauto/cfhr31dt/CFHR31FL.csv",as.is=TRUE)
# cf.wealth <- read.csv("D:/Documents/Data/DHSauto/cfwi31dt/CFWI31FL.csv",as.is=TRUE)
# names(cf.wealth)[which(names(cf.wealth)=="whhid")] <- "hhid"
# names(cf.wealth)[which(names(cf.wealth)=="wlthindf")] <- "hv271"
# cf.wealth$hv271 <- cf.wealth$hv271*100000
# cf <- join(cf,cf.wealth,by="hhid")
# write.csv(cf,"D:/Documents/Data/DHSauto/cfhr31dt/CFHR31FL.csv",na="",row.names=FALSE)

# ni <- read.csv("D:/Documents/Data/DHSauto/nihr31dt/niHR31FL.csv",as.is=TRUE)
# ni.wealth <- read.csv("D:/Documents/Data/DHSauto/niwi31dt/niWI31FL.csv",as.is=TRUE)
# names(ni.wealth)[which(names(ni.wealth)=="whhid")] <- "hhid"
# names(ni.wealth)[which(names(ni.wealth)=="wlthindf")] <- "hv271"
# ni.wealth$hv271 <- ni.wealth$hv271*100000
# ni <- join(ni,ni.wealth,by="hhid")
# write.csv(ni,"D:/Documents/Data/DHSauto/nihr31dt/niHR31FL.csv",na="",row.names=FALSE)

for(i in 1:nrow(filenames)){
  hrBase <- filenames$filename[i]
  message(hrBase)
  year <- filenames$DHSYEAR[i]
  cc <- toupper(substr(hrBase,1,2))
  dhscc <- tolower(cc)
  phase <- substr(hrBase,5,6)
  hrFile <- paste0(
    "D:/Documents/Data/DHSauto/"
    ,hrBase
    ,"/"
    ,cc
    ,"HR"
    ,phase
    ,"FL"
    ,".csv"
  )
  dhs <- read.csv(hrFile,as.is=TRUE)
  #   iso3 <- subset(isos,cc==dhscc)$iso3[1]
  
  names(dhs)[which(names(dhs)=="hv271")] <- "wealth"
  dhs$wealth <- dhs$wealth/100000
  
  names(dhs)[which(names(dhs)=="hv035")] <- "measured.children"
  dhs$stunted.children <- stunted.children(dhs)
  if(typeof(dhs$measured.children)=="NULL" | length(unique(dhs$measured.children))<=2){dhs$measured.children<-dhs$hv014}
  
  #Rename sample.weights var
  names(dhs)[which(names(dhs)=="hv005")] <- "sample.weights"
  dhs$weights <- dhs$sample.weights/1000000
  
  #Rename cluster/hh var
  names(dhs)[which(names(dhs)=="hv001")] <- "cluster"
  names(dhs)[which(names(dhs)=="hv002")] <- "household"
  
  povcalcut <- povcal(dhscc,year)
  if(is.na(povcalcut) & dhscc=="eg"){
    povcalcut <- 0.0202
  }else if(is.na(povcalcut) & dhscc=="id"){
    povcalcut <- 0.1590
  }
  povcalperc <- weighted.percentile(dhs$wealth,dhs$weights,prob=povcalcut)
  dhs$p20 <- (dhs$wealth < povcalperc)
  dhs.tab <- data.table(dhs)
  cluster.tab <- dhs.tab[,.(p20=weighted.mean(p20,weights),stunted.children=(sum(stunted.children*weights,na.rm=TRUE)/sum(weights,na.rm=TRUE)),measured.children=(sum(measured.children*weights,na.rm=TRUE)/sum(weights,na.rm=TRUE))),by=.(cluster)]
  cluster.tab$DHSCC <- cc
  cluster.tab$year <- year
  dataList[[dataIndex]] <- cluster.tab
  dataIndex <- dataIndex + 1
}

metaClusters <- rbindlist(dataList)
metaClusters$stunted = calc.stunting.perc(metaClusters$stunted.children,metaClusters$measured.children)
metaClusters$stunted.children <- NULL
metaClusters$measured.children <- NULL
write.csv(metaClusters,"D:/Documents/Data/DHS map/all_clusters2.csv",na="",row.names=FALSE)
metaClusters <- read.csv("D:/Documents/Data/DHS map/all_clusters2.csv",na.strings="",as.is=TRUE)
setnames(metaClusters,"cluster","DHSCLUST")
dat <- join(dat,metaClusters,by=c("DHSCC","DHSCLUST"))
dat$p20[which(is.na(dat$p20))] <- -1
dat$stunted[which(is.na(dat$stunted))] <- -1
write.dbf(dat,"aggregate_clusters/clusters.dbf")
