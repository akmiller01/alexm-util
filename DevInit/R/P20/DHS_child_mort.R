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
    
    brwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"br",phase,"dt/")
    if(!file_test(op="-d", brwd)){next;}

    data <- read.csv(paste0(brwd,iso2,"BR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    data$wealth <- data$v191/100000
    data$p20 <- (data$wealth < povperc)
    data$age.months <- data$b3-data$v008
    data$weights <- data$v005/1000000
    
    probs <- c()
    tl = -59
    tu = 0
    segments <- list(
      list("al"=0,"au"=0)
      ,list("al"=1,"au"=2)
      ,list("al"=3,"au"=5)
      ,list("al"=6,"au"=11)
      ,list("al"=12,"au"=23)
      ,list("al"=24,"au"=35)
      ,list("al"=36,"au"=47)
      ,list("al"=48,"au"=59)
    )
    for(i in 1:length(segments)){
      segment = segments[[i]]
      al = segment[["al"]]
      au = segment[["au"]]
      cohortA <- subset(data,age.months>=(tl-au) & age.months<(tl-al))
      cohortB <- subset(data,age.months>=(tl-al) & age.months<=(tu-au))
      cohortC <- subset(data,age.months>(tu-au) & age.months<=(tu-al))
      Amortalities <- subset(cohortA, b7>=al & b7<=au)
      Bmortalities <- subset(cohortB, b7>=al & b7<=au)
      Cmortalities <- subset(cohortC, b7>=al & b7<=au)
      if(tu==0){
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+nrow(Cmortalities)
      }else{
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+0.5*nrow(Cmortalities)
      }
      
      Asurvivals <- subset(cohortA,is.na(b7) | b7>al)
      Bsurvivals <- subset(cohortB,is.na(b7) | b7>al)
      Csurvivals <- subset(cohortC,is.na(b7) | b7>al)
      if(tu==0){
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+nrow(Csurvivals)
      }else{
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+0.5*nrow(Csurvivals)
      }
      
      prob <- 1-(mortalities/survivals)
      if(is.nan(prob)){
        prob <- 1
      }
      probs <- c(probs,prob)
    }
    mortality <- (1-prod(probs))*1000
    
    p20 <- subset(data,p20==TRUE)
    non.p20 <- subset(data,p20==FALSE)
    
    data <- p20
    probs <- c()
    tl = -59
    tu = 0
    segments <- list(
      list("al"=0,"au"=0)
      ,list("al"=1,"au"=2)
      ,list("al"=3,"au"=5)
      ,list("al"=6,"au"=11)
      ,list("al"=12,"au"=23)
      ,list("al"=24,"au"=35)
      ,list("al"=36,"au"=47)
      ,list("al"=48,"au"=59)
    )
    for(i in 1:length(segments)){
      segment = segments[[i]]
      al = segment[["al"]]
      au = segment[["au"]]
      cohortA <- subset(data,age.months>=(tl-au) & age.months<(tl-al))
      cohortB <- subset(data,age.months>=(tl-al) & age.months<=(tu-au))
      cohortC <- subset(data,age.months>(tu-au) & age.months<=(tu-al))
      Amortalities <- subset(cohortA, b7>=al & b7<=au)
      Bmortalities <- subset(cohortB, b7>=al & b7<=au)
      Cmortalities <- subset(cohortC, b7>=al & b7<=au)
      if(tu==0){
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+nrow(Cmortalities)
      }else{
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+0.5*nrow(Cmortalities)
      }
      
      Asurvivals <- subset(cohortA,is.na(b7) | b7>al)
      Bsurvivals <- subset(cohortB,is.na(b7) | b7>al)
      Csurvivals <- subset(cohortC,is.na(b7) | b7>al)
      if(tu==0){
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+nrow(Csurvivals)
      }else{
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+0.5*nrow(Csurvivals)
      }
      
      prob <- 1-(mortalities/survivals)
      if(is.nan(prob)){
        prob <- 1
      }
      probs <- c(probs,prob)
    }
    p20.mortality <- (1-prod(probs))*1000
    
    data <- non.p20
    probs <- c()
    tl = -59
    tu = 0
    segments <- list(
      list("al"=0,"au"=0)
      ,list("al"=1,"au"=2)
      ,list("al"=3,"au"=5)
      ,list("al"=6,"au"=11)
      ,list("al"=12,"au"=23)
      ,list("al"=24,"au"=35)
      ,list("al"=36,"au"=47)
      ,list("al"=48,"au"=59)
    )
    for(i in 1:length(segments)){
      segment = segments[[i]]
      al = segment[["al"]]
      au = segment[["au"]]
      cohortA <- subset(data,age.months>=(tl-au) & age.months<(tl-al))
      cohortB <- subset(data,age.months>=(tl-al) & age.months<=(tu-au))
      cohortC <- subset(data,age.months>(tu-au) & age.months<=(tu-al))
      Amortalities <- subset(cohortA, b7>=al & b7<=au)
      Bmortalities <- subset(cohortB, b7>=al & b7<=au)
      Cmortalities <- subset(cohortC, b7>=al & b7<=au)
      if(tu==0){
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+nrow(Cmortalities)
      }else{
        mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+0.5*nrow(Cmortalities)
      }
      
      Asurvivals <- subset(cohortA,is.na(b7) | b7>al)
      Bsurvivals <- subset(cohortB,is.na(b7) | b7>al)
      Csurvivals <- subset(cohortC,is.na(b7) | b7>al)
      if(tu==0){
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+nrow(Csurvivals)
      }else{
        survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+0.5*nrow(Csurvivals)
      }
      
      prob <- 1-(mortalities/survivals)
      if(is.nan(prob)){
        prob <- 1
      }
      probs <- c(probs,prob)
    }
    non.p20.mortality <- (1-prod(probs))*1000
    
    data <- data.frame(mortality,p20.mortality,non.p20.mortality)
    
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

total <- rbindlist(dataList)
setwd("D:/Documents/Data/DHSmeta2")
save(total,file="child_mort.RData")
write.csv(total,"child_mort.csv",row.names=FALSE,na="")