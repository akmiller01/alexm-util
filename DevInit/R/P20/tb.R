####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
options(descr.plot = FALSE)

weighted.table <- function(x,y,w){
  return(
    data.frame(crosstab(x,y,weight=w,prop.t=TRUE)$prop.tbl)
  )
}
weighted.table <- function(x,y,w){
  return(
    data.frame(crosstab(x,y,weight=w,prop.c=TRUE)$prop.col)
  )
}


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
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1
dataList2 <- list()
dataIndex2 <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  if(basename(dir) %in% filenames){
    hrwd <- basename(dir)
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
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
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){message("IR WD invalid");return(NA);}
    
    # ir <- read.dta(paste0(irwd,iso2,"IR",phase,"FL.dta"))
    # var.labs <- data.frame(names(ir),attributes(ir)[7])
    ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    names(ir)[which(names(ir)=="v191")] <- "wealth"
    ir$wealth <- ir$wealth/100000
    #Rename sample.weights var
    names(ir)[which(names(ir)=="v005")] <- "sample.weights"
    ir$weights <- ir$sample.weights/1000000
    
    ir$p20 <- (ir$wealth<povperc)
    
    ir$fgm <- NA
    ir$fgm[which(ir$g102==0 | tolower(ir$g102)=="no")] <- 0
    ir$fgm[which(ir$g102==1 | tolower(ir$g102)=="yes")] <- 1
    
    ir$any.std <- NA
    ir$any.std[which(tolower(ir$v763a)=="no" | ir$v763a==0)] <- 0
    ir$any.std[which(tolower(ir$v763a)=="yes" | ir$v763a==1)] <- 1
    
    if(typeof(ir$any.std)!="NULL" & sum(is.na(ir$any.std))!=length(ir$any.std) & length(unique(ir$any.std[which(!is.na(ir$any.std))]))>1){
      std.data <- weighted.table(ir$any.std,ir$p20,ir$weights)
      std.data$filename <- hrBase
      dataList[[dataIndex]] <- std.data
      dataIndex <- dataIndex + 1
    }
    if(typeof(ir$fgm)!="NULL" & sum(is.na(ir$fgm))!=length(ir$fgm)){
      fgm.data <- weighted.table(ir$fgm,ir$p20,ir$weights)
      fgm.data$filename <- hrBase
      dataList2[[dataIndex]] <- fgm.data
      dataIndex2 <- dataIndex2 + 1
    }
  }
}
library(varhandle)
std <- rbindlist(dataList)
names(std) <- c("any.std","p20","Freq","filename")
std$any.std <- unfactor(std$any.std)
fgm <- rbindlist(dataList2)
names(fgm) <- c("fgm","p20","Freq","filename")
fgm$fgm <- unfactor(fgm$fgm)
setwd("D:/Documents/Data/DHSmeta2")

pop <- povcalcuts[c("filename","female.15.49")]
std <- merge(std,pop,by="filename")
fgm <- merge(fgm,pop,by="filename")
std.tab <- std[,.(
  any.std = weighted.mean(Freq,female.15.49,na.rm=TRUE)
),by=.(p20,any.std)]
fgm.tab <- fgm[,.(
  fgm = weighted.mean(Freq,female.15.49,na.rm=TRUE)
),by=.(p20,fgm)]

write.csv(std,"std-nationally.csv",na="",row.names=FALSE)
write.csv(std.tab,"std-globally.csv",na="",row.names=FALSE)
write.csv(fgm,"fgm-nationally.csv",na="",row.names=FALSE)
write.csv(fgm.tab,"fgm-globally.csv",na="",row.names=FALSE)
