####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)

wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

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

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){next;}
    
    data <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                     ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    data$wealth <- data$v191/100000
    data$p20 <- (data$wealth < povperc)
    years.since <- 7
    
    data$survey.date <- data$v008
    data$weights <- data$v005/1000000
    
    p20 <- subset(data,p20==TRUE)
    non.p20 <- subset(data,p20==FALSE)
    
    demo.vars <- c("survey.date","wealth","weights")
    mm.vars <- names(data)[which(substr(names(data),1,2)=="mm" & substr(names(data),1,3)!="mmc")]
    if(length(mm.vars)==0){next;}
    
    data.long <- data[c(demo.vars,mm.vars)]
    varying.vars <- names(data.long)[which(grepl("_",names(data.long)))]
    data.long <- reshape(data.long,direction="long",varying=varying.vars,sep="_")
    
    sisters <- subset(data.long,mm1 %in% c(2,"female"))
    
    ####Numerator
    sisters$pod <- sisters$survey.date-sisters$mm8
    sisters$age.at.death <- sisters$mm8-sisters$mm4
    sisters$aad.groups <- floor(sisters$age.at.death/60)
    
    maternal <- c(2,"died while pregnant",3,"died during delivery",4,"since delivery",5,"6 weeks after delivery",6,"2 months after delivery")
    not.maternal <- c(0,"never pregnant",1,"death not related",98,"don't know",99,NA)
    
    numerator <- data.table(subset(sisters,pod<(12*years.since)))[,.(deaths=sum(weights)
                                                                     ,maternal=sum((mm9 %in% maternal)*weights)
                                                                     ,non.maternal=sum((mm9 %in% not.maternal)*weights)
    ),by=.(aad.groups)]
    
    numerator <- data.frame(transform(numerator,mm.deaths = deaths*(maternal/(maternal+non.maternal))))
    numerator <- data.frame(transform(numerator,maternal.perc = (maternal/(maternal+non.maternal))))
    numerator <- numerator[c("aad.groups","mm.deaths")]
    
    names(numerator) <- c("age.group","numerator")
    
    ####Denominator (period 0-6 years, 0-84 months)
    surv.denom <- subset(sisters, is.na(age.at.death))
    dead.denom <- subset(sisters, !is.na(age.at.death))
    
    #Highest age group, surviving
    surv.denom$ha <- (surv.denom$survey.date - 1)  - surv.denom$mm4
    surv.denom$ha.group <- floor(surv.denom$ha/60)
    surv.denom$hag.exposure.months <- surv.denom$ha - (surv.denom$ha.group*60 + 1)
    surv.denom$hag.exposure.months <- pmax(surv.denom$hag.exposure.months,0)
    
    #Middle age group, surviving
    surv.denom$ma <- (surv.denom$survey.date - 61)  - surv.denom$mm4
    surv.denom$ma.group <- floor(surv.denom$ma/60)
    surv.denom$mag.exposure.months <- surv.denom$ma - (surv.denom$ma.group*60 + 1)
    surv.denom$mag.exposure.months <- pmax(surv.denom$mag.exposure.months,0)
    
    #Lowest age group, surviving
    surv.denom$la <- (surv.denom$survey.date - 121)  - surv.denom$mm4
    surv.denom$la.group <- floor(surv.denom$la/60)
    surv.denom$lag.exposure.months <- years.since*12 - (surv.denom$hag.exposure.months+surv.denom$mag.exposure.months)
    surv.denom$lag.exposure.months <- pmax(surv.denom$lag.exposure.months,0)
    
    #Highest age group, dead
    dead.denom$ha <- (dead.denom$mm8)  - dead.denom$mm4
    dead.denom$ha.group <- floor(dead.denom$ha/60)
    dead.denom$hag.exposure.months <- dead.denom$ha - (dead.denom$ha.group*60 + 1)
    dead.denom$hag.exposure.months <- pmax(dead.denom$hag.exposure.months,0)
    dead.denom$hag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 60))] <- 0
    
    #Middle age group, dead
    dead.denom$ma <- (dead.denom$mm8-61)  - dead.denom$mm4
    dead.denom$ma.group <- floor(dead.denom$ma/60)
    dead.denom$mag.exposure.months <- dead.denom$ma - (dead.denom$ma.group*60 + 1)
    dead.denom$mag.exposure.months <- pmax(dead.denom$mag.exposure.months,0)
    dead.denom$mag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 120))] <- 0
    
    #Lowest age group, dead
    dead.denom$la <- (dead.denom$mm8-121) - dead.denom$mm4
    dead.denom$la.group <- floor(dead.denom$la/60)
    dead.denom$lag.exposure.months <- dead.denom$la - (dead.denom$la.group*60 + 1)
    dead.denom$lag.exposure.months <- pmax(dead.denom$lag.exposure.months,0)
    dead.denom$lag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - years.since*12))] <- 0
    
    denominator <- rbind(surv.denom,dead.denom)
    denom.ha <- data.table(denominator)[,.(ha.count=sum(hag.exposure.months*weights)/12),by=.(ha.group)]
    names(denom.ha) <- c("age.group","ha.count")
    denom.ma <- data.table(denominator)[,.(ma.count=sum(mag.exposure.months*weights)/12),by=.(ma.group)]
    names(denom.ma) <- c("age.group","ma.count")
    denom.la <- data.table(denominator)[,.(la.count=sum(lag.exposure.months*weights)/12),by=.(la.group)]
    names(denom.la) <- c("age.group","la.count")
    
    denominator <- data.frame(join_all(list(denom.ha,denom.ma,denom.la)))
    denominator$denominator <- rowSums(denominator[c("ha.count","ma.count","la.count")],na.rm=TRUE)
    
    mort <- join(numerator,denominator,by="age.group")
    mort$mortality <- mort$numerator/mort$denominator
    mort <- data.frame(mort)[c("age.group","mortality")]
    mort <- subset(mort,age.group>2 & age.group<10)
    mort <- mort[order(mort$age.group),]
    age.verbose <- c("age15.19","age20.24","age25.29","age30.34","age35.39","age40.44","age45.49")
    mort$mortality <- mort$mortality*1000
    mort <- mort[c("mortality")]
    mort <- data.frame(t(mort))
    names(mort) <- age.verbose
    mort$type <- "All"
    all.mort <- mort
    
    data <- p20
    data.long <- data[c(demo.vars,mm.vars)]
    varying.vars <- names(data.long)[which(grepl("_",names(data.long)))]
    data.long <- reshape(data.long,direction="long",varying=varying.vars,sep="_")
    
    sisters <- subset(data.long,mm1 %in% c(2,"female"))
    
    ####Numerator
    sisters$pod <- sisters$survey.date-sisters$mm8
    sisters$age.at.death <- sisters$mm8-sisters$mm4
    sisters$aad.groups <- floor(sisters$age.at.death/60)
    
    maternal <- c(2,"died while pregnant",3,"died during delivery",4,"since delivery",5,"6 weeks after delivery",6,"2 months after delivery")
    not.maternal <- c(0,"never pregnant",1,"death not related",98,"don't know",99,NA)
    
    numerator <- data.table(subset(sisters,pod<(12*years.since)))[,.(deaths=sum(weights)
                                                                     ,maternal=sum((mm9 %in% maternal)*weights)
                                                                     ,non.maternal=sum((mm9 %in% not.maternal)*weights)
    ),by=.(aad.groups)]
    
    numerator <- data.frame(transform(numerator,mm.deaths = deaths*(maternal/(maternal+non.maternal))))
    numerator <- data.frame(transform(numerator,maternal.perc = (maternal/(maternal+non.maternal))))
    numerator <- numerator[c("aad.groups","mm.deaths")]
    
    names(numerator) <- c("age.group","numerator")
    
    ####Denominator (period 0-6 years, 0-84 months)
    surv.denom <- subset(sisters, is.na(age.at.death))
    dead.denom <- subset(sisters, !is.na(age.at.death))
    
    #Highest age group, surviving
    surv.denom$ha <- (surv.denom$survey.date - 1)  - surv.denom$mm4
    surv.denom$ha.group <- floor(surv.denom$ha/60)
    surv.denom$hag.exposure.months <- surv.denom$ha - (surv.denom$ha.group*60 + 1)
    surv.denom$hag.exposure.months <- pmax(surv.denom$hag.exposure.months,0)
    
    #Middle age group, surviving
    surv.denom$ma <- (surv.denom$survey.date - 61)  - surv.denom$mm4
    surv.denom$ma.group <- floor(surv.denom$ma/60)
    surv.denom$mag.exposure.months <- surv.denom$ma - (surv.denom$ma.group*60 + 1)
    surv.denom$mag.exposure.months <- pmax(surv.denom$mag.exposure.months,0)
    
    #Lowest age group, surviving
    surv.denom$la <- (surv.denom$survey.date - 121)  - surv.denom$mm4
    surv.denom$la.group <- floor(surv.denom$la/60)
    surv.denom$lag.exposure.months <- years.since*12 - (surv.denom$hag.exposure.months+surv.denom$mag.exposure.months)
    surv.denom$lag.exposure.months <- pmax(surv.denom$lag.exposure.months,0)
    
    #Highest age group, dead
    dead.denom$ha <- (dead.denom$mm8)  - dead.denom$mm4
    dead.denom$ha.group <- floor(dead.denom$ha/60)
    dead.denom$hag.exposure.months <- dead.denom$ha - (dead.denom$ha.group*60 + 1)
    dead.denom$hag.exposure.months <- pmax(dead.denom$hag.exposure.months,0)
    dead.denom$hag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 60))] <- 0
    
    #Middle age group, dead
    dead.denom$ma <- (dead.denom$mm8-61)  - dead.denom$mm4
    dead.denom$ma.group <- floor(dead.denom$ma/60)
    dead.denom$mag.exposure.months <- dead.denom$ma - (dead.denom$ma.group*60 + 1)
    dead.denom$mag.exposure.months <- pmax(dead.denom$mag.exposure.months,0)
    dead.denom$mag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 120))] <- 0
    
    #Lowest age group, dead
    dead.denom$la <- (dead.denom$mm8-121) - dead.denom$mm4
    dead.denom$la.group <- floor(dead.denom$la/60)
    dead.denom$lag.exposure.months <- dead.denom$la - (dead.denom$la.group*60 + 1)
    dead.denom$lag.exposure.months <- pmax(dead.denom$lag.exposure.months,0)
    dead.denom$lag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - years.since*12))] <- 0
    
    denominator <- rbind(surv.denom,dead.denom)
    denom.ha <- data.table(denominator)[,.(ha.count=sum(hag.exposure.months*weights)/12),by=.(ha.group)]
    names(denom.ha) <- c("age.group","ha.count")
    denom.ma <- data.table(denominator)[,.(ma.count=sum(mag.exposure.months*weights)/12),by=.(ma.group)]
    names(denom.ma) <- c("age.group","ma.count")
    denom.la <- data.table(denominator)[,.(la.count=sum(lag.exposure.months*weights)/12),by=.(la.group)]
    names(denom.la) <- c("age.group","la.count")
    
    denominator <- data.frame(join_all(list(denom.ha,denom.ma,denom.la)))
    denominator$denominator <- rowSums(denominator[c("ha.count","ma.count","la.count")],na.rm=TRUE)
    
    mort <- join(numerator,denominator,by="age.group")
    mort$mortality <- mort$numerator/mort$denominator
    mort <- data.frame(mort)[c("age.group","mortality")]
    mort <- subset(mort,age.group>2 & age.group<10)
    mort <- mort[order(mort$age.group),]
    age.verbose <- c("age15.19","age20.24","age25.29","age30.34","age35.39","age40.44","age45.49")
    mort$mortality <- mort$mortality*1000
    mort <- mort[c("mortality")]
    mort <- data.frame(t(mort))
    names(mort) <- age.verbose
    mort$type <- "P20"
    p20.mort <- mort
    
    data <- non.p20
    data.long <- data[c(demo.vars,mm.vars)]
    varying.vars <- names(data.long)[which(grepl("_",names(data.long)))]
    data.long <- reshape(data.long,direction="long",varying=varying.vars,sep="_")
    
    sisters <- subset(data.long,mm1 %in% c(2,"female"))
    
    ####Numerator
    sisters$pod <- sisters$survey.date-sisters$mm8
    sisters$age.at.death <- sisters$mm8-sisters$mm4
    sisters$aad.groups <- floor(sisters$age.at.death/60)
    
    maternal <- c(2,"died while pregnant",3,"died during delivery",4,"since delivery",5,"6 weeks after delivery",6,"2 months after delivery")
    not.maternal <- c(0,"never pregnant",1,"death not related",98,"don't know",99,NA)
    
    numerator <- data.table(subset(sisters,pod<(12*years.since)))[,.(deaths=sum(weights)
                                                                     ,maternal=sum((mm9 %in% maternal)*weights)
                                                                     ,non.maternal=sum((mm9 %in% not.maternal)*weights)
    ),by=.(aad.groups)]
    
    numerator <- data.frame(transform(numerator,mm.deaths = deaths*(maternal/(maternal+non.maternal))))
    numerator <- data.frame(transform(numerator,maternal.perc = (maternal/(maternal+non.maternal))))
    numerator <- numerator[c("aad.groups","mm.deaths")]
    
    names(numerator) <- c("age.group","numerator")
    
    ####Denominator (period 0-6 years, 0-84 months)
    surv.denom <- subset(sisters, is.na(age.at.death))
    dead.denom <- subset(sisters, !is.na(age.at.death))
    
    #Highest age group, surviving
    surv.denom$ha <- (surv.denom$survey.date - 1)  - surv.denom$mm4
    surv.denom$ha.group <- floor(surv.denom$ha/60)
    surv.denom$hag.exposure.months <- surv.denom$ha - (surv.denom$ha.group*60 + 1)
    surv.denom$hag.exposure.months <- pmax(surv.denom$hag.exposure.months,0)
    
    #Middle age group, surviving
    surv.denom$ma <- (surv.denom$survey.date - 61)  - surv.denom$mm4
    surv.denom$ma.group <- floor(surv.denom$ma/60)
    surv.denom$mag.exposure.months <- surv.denom$ma - (surv.denom$ma.group*60 + 1)
    surv.denom$mag.exposure.months <- pmax(surv.denom$mag.exposure.months,0)
    
    #Lowest age group, surviving
    surv.denom$la <- (surv.denom$survey.date - 121)  - surv.denom$mm4
    surv.denom$la.group <- floor(surv.denom$la/60)
    surv.denom$lag.exposure.months <- years.since*12 - (surv.denom$hag.exposure.months+surv.denom$mag.exposure.months)
    surv.denom$lag.exposure.months <- pmax(surv.denom$lag.exposure.months,0)
    
    #Highest age group, dead
    dead.denom$ha <- (dead.denom$mm8)  - dead.denom$mm4
    dead.denom$ha.group <- floor(dead.denom$ha/60)
    dead.denom$hag.exposure.months <- dead.denom$ha - (dead.denom$ha.group*60 + 1)
    dead.denom$hag.exposure.months <- pmax(dead.denom$hag.exposure.months,0)
    dead.denom$hag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 60))] <- 0
    
    #Middle age group, dead
    dead.denom$ma <- (dead.denom$mm8-61)  - dead.denom$mm4
    dead.denom$ma.group <- floor(dead.denom$ma/60)
    dead.denom$mag.exposure.months <- dead.denom$ma - (dead.denom$ma.group*60 + 1)
    dead.denom$mag.exposure.months <- pmax(dead.denom$mag.exposure.months,0)
    dead.denom$mag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 120))] <- 0
    
    #Lowest age group, dead
    dead.denom$la <- (dead.denom$mm8-121) - dead.denom$mm4
    dead.denom$la.group <- floor(dead.denom$la/60)
    dead.denom$lag.exposure.months <- dead.denom$la - (dead.denom$la.group*60 + 1)
    dead.denom$lag.exposure.months <- pmax(dead.denom$lag.exposure.months,0)
    dead.denom$lag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - years.since*12))] <- 0
    
    denominator <- rbind(surv.denom,dead.denom)
    denom.ha <- data.table(denominator)[,.(ha.count=sum(hag.exposure.months*weights)/12),by=.(ha.group)]
    names(denom.ha) <- c("age.group","ha.count")
    denom.ma <- data.table(denominator)[,.(ma.count=sum(mag.exposure.months*weights)/12),by=.(ma.group)]
    names(denom.ma) <- c("age.group","ma.count")
    denom.la <- data.table(denominator)[,.(la.count=sum(lag.exposure.months*weights)/12),by=.(la.group)]
    names(denom.la) <- c("age.group","la.count")
    
    denominator <- data.frame(join_all(list(denom.ha,denom.ma,denom.la)))
    denominator$denominator <- rowSums(denominator[c("ha.count","ma.count","la.count")],na.rm=TRUE)
    
    mort <- join(numerator,denominator,by="age.group")
    mort$mortality <- mort$numerator/mort$denominator
    mort <- data.frame(mort)[c("age.group","mortality")]
    mort <- subset(mort,age.group>2 & age.group<10)
    mort <- mort[order(mort$age.group),]
    age.verbose <- c("age15.19","age20.24","age25.29","age30.34","age35.39","age40.44","age45.49")
    mort$mortality <- mort$mortality*1000
    mort <- mort[c("mortality")]
    mort <- data.frame(t(mort))
    names(mort) <- age.verbose
    mort$type <- "Non P20"
    non.p20.mort <- mort
    
    data <- rbind(all.mort,p20.mort,non.p20.mort)
    
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

total <- rbindlist(dataList)
setwd("D:/Documents/Data/DHSmeta2")
save(total,file="maternal_mort.RData")
write.csv(total,"maternal_mort.csv",row.names=FALSE,na="")