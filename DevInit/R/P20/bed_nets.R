####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
options(descr.plot = FALSE)

# weighted.table <- function(x,y,w){
#   return(
#     data.frame(crosstab(x,y,weight=w,prop.t=TRUE)$prop.tbl)
#   )
# }
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
    
    if(typeof(pr$hv227)!="NULL" & sum(is.na(pr$hv227))!=length(pr$hv227) & length(unique(pr$hv227[which(!is.na(pr$hv227))]))>1){
      pr$bn <- NA
      pr$bn[which(pr$hv227==1 | tolower(pr$hv227)=="yes")] <- 1
      pr$bn[which(pr$hv227==0 | tolower(pr$hv227)=="no")] <- 0
      ct <- weighted.table(pr$bn,pr$p20,pr$weights)
      ct$filename <- hrBase
      dataList[[dataIndex]] <- ct
      dataIndex <- dataIndex + 1
    }
  }
}

bn <- rbindlist(dataList)
names(bn) <- c("bn","p20","Freq","filename")
setwd("D:/Documents/Data/DHSmeta2")

write.csv(bn,"bn-nationally.csv",na="",row.names=FALSE)
