####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
require(zoo)

wd <- "C:/Users/Alex/Documents/Data/P20/Meta"
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

psum <- function(...,na.rm=TRUE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm)
} 

####Run function####
# set our working directory, change this if using on another machine
wd <- "C:/Users/Alex/Documents/Data/P20/DHS/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
rdatas <- list.files(path=wd,pattern="*.RData",full.names=TRUE)

dataList <- list()
dataIndex <- 1

attendants <- paste0("m3",letters,"_1")

# Loop through every dir
for(i in 1:length(rdatas)){
  rdata <- rdatas[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(rdata),1,2))
  recode <- tolower(substr(basename(rdata),3,4))
  phase <- tolower(substr(basename(rdata),5,6))
  povcal_filename <- paste0(country,recode,phase,"dt")
  if(povcal_filename %in% povcalcuts$filename){
    message(povcal_filename)
    # load(rdata)
    # hr <- data
    # remove(data)
    
    ###Maternal
    ir_path <- paste0(country,"ir",phase,"fl.RData")
    load(ir_path)
    dat.labs <- data.frame(variable=names(data),label=attributes(data)[7])
    dat.labs <- subset(dat.labs,variable %in% attendants)
    dat.labs$filename <- povcal_filename
    remove(data)
    dataList[[dataIndex]] <- dat.labs
    dataIndex <- dataIndex + 1
  }
}
dat <- rbindlist(dataList)
wd <- "C:/Users/Alex/Documents/Data/P20/Meta"
setwd(wd)
write.csv(dat,"DHS_skilled_labels.csv")