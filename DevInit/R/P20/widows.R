####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)
options(descr.plot = FALSE)

weighted.table <- function(x,y,w){
  return(
    data.frame(crosstab(x,y,weight=w,prop.t=TRUE)$prop.tbl)
  )
}
# weighted.table <- function(x,y,w){
#   return(
#     data.frame(crosstab(x,y,weight=w,prop.c=TRUE)$prop.col)
#   )
# }


psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 

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

setwd("D:/Documents/Data/P20_2013/meta")

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE,na.strings="")
filenames <- povcalcuts$filename

setwd("D:/Documents/Data/DHSmeta/")

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
  if(basename(dir) %in% filenames){
    hrwd <- basename(dir)
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    message(hrBase)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    # pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.dta"))
    # var.labs <- data.frame(names(pr),attributes(pr)[7])
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    pr$p20 <- (pr$wealth<povperc)
    
    hr <- read.csv(paste0(hrwd,"/",iso2,"HR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    names(hr)[which(names(hr)=="hv271")] <- "wealth"
    hr$wealth <- hr$wealth/100000
    names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
    hr$weights <- hr$sample.weights/1000000
    hr$p20 <- (hr$wealth<povperc)
    
    pov <- hr[c("hv001","hv002","weights","p20")]
    names(pov) <- c("v001","v002","weights","p20")
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){message("IR WD invalid");return(NA);}
    
    ir <- read.dta(paste0(irwd,iso2,"IR",phase,"FL.dta"),convert.factors = FALSE)
    names(ir) <- tolower(names(ir))
    # var.labs <- data.frame(names(ir),attributes(ir)[7])
    # ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
    #                ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    ir$head <- ir$v150==1
    
    if(length(which(ir$v501==3))>0){
    
      ir$widow <- ir$v501==3
        
      ir.tab <- data.table(ir)[,.(
        if(sum(widow==TRUE & head==TRUE,na.rm=TRUE)>=1){
          category="Widowed female head-of-household"
        }else if(sum(widow==FALSE & head==TRUE,na.rm=TRUE)>=1){
          category="Non-widowed female head-of-household"
        }else{
          category="Male head-of-household"
        }),by=.(v001,v002)]
    
        ir.tab <- merge(ir.tab,pov,by=c("v001","v002"))
        data <- weighted.table(ir.tab$p20,ir.tab$V1,ir.tab$weights)
        names(data) <- c("p20","category","frequency")
        data$p20 <- unfactor(data$p20)
        data$p20[which(data$p20==FALSE)] <- "Rest of the population"
        data$p20[which(data$p20==TRUE)] <- "P20"
        data$filename <- hrBase
        dataList[[dataIndex]] <- data
        dataIndex <- dataIndex + 1 
      
    }
  }
}

dat <- rbindlist(dataList)

pop <- povcalcuts[c("filename","pop.total")]
dat <- merge(dat,pop,by="filename")
dat.tab <- dat[,.(
  frequency = weighted.mean(frequency,pop.total,na.rm=TRUE)
),by=.(p20,category)]

setwd("D:/Documents/Data/P20_2013/meta")
write.csv(dat,"widowed-nationally.csv",na="",row.names=FALSE)
write.csv(dat.tab,"widowed-internationally.csv",na="",row.names=FALSE)

