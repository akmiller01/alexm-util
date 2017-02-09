####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(varhandle)
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
    
    # if(is.factor(ir$d123)){ir$d123 <- unfactor(ir$d123)}
    # ir$forced <- NA
    # ir$forced[which(ir$d123==1 | tolower(ir$d123)=="wanted")] <- 0
    # ir$forced[which(ir$d123==2 | tolower(ir$d123)=="forced")] <- 1
    ir$forced <- ir$d123
    if(length(which(ir$forced==9))>0){
      ir$forced[which(ir$forced==9)] <- NA 
    }
    
    # if(is.factor(ir$d118y)){ir$d118y <- unfactor(ir$d118y)}
    # ir$hurt.preg <- NA
    # ir$hurt.preg[which(tolower(ir$d118y)=="someone hurt respondent during pregnancy" | ir$d118y==0)] <- 1
    # ir$hurt.preg[which(tolower(ir$d118y)=="no one hurt respondent during pregnancy" | ir$d118y==1)] <- 0
    ir$hurt.preg <- ir$d118y
    if(length(which(ir$hurt.preg==9))>0){
      ir$hurt.preg[which(ir$hurt.preg==9)] <- NA
    }
    
    if(typeof(ir$forced)!="NULL" & sum(is.na(ir$forced))!=length(ir$forced) & length(unique(ir$forced[which(!is.na(ir$forced))]))>1){
      std.data <- weighted.table(ir$forced,ir$p20,ir$weights)
      std.data$filename <- hrBase
      dataList[[dataIndex]] <- std.data
      dataIndex <- dataIndex + 1
    }
    if(typeof(ir$hurt.preg)!="NULL" & sum(is.na(ir$hurt.preg))!=length(ir$hurt.preg) & length(unique(ir$hurt.preg[which(!is.na(ir$hurt.preg))]))>1){
      hurt.preg.data <- weighted.table(ir$hurt.preg,ir$p20,ir$weights)
      hurt.preg.data$filename <- hrBase
      dataList2[[dataIndex2]] <- hurt.preg.data
      dataIndex2 <- dataIndex2 + 1
    }
  }
}
library(varhandle)
forced <- rbindlist(dataList)
names(forced) <- c("forced","p20","Freq","filename")
forced$forced <- unfactor(forced$forced)
hurt.preg <- rbindlist(dataList2)
names(hurt.preg) <- c("hurt.preg","p20","Freq","filename")
hurt.preg$hurt.preg <- unfactor(hurt.preg$hurt.preg)
setwd("D:/Documents/Data/DHSmeta2")

pop <- povcalcuts[c("filename","female.15.49")]
forced <- merge(forced,pop,by="filename")
hurt.preg <- merge(hurt.preg,pop,by="filename")
forced.tab <- forced[,.(
  Freq = weighted.mean(Freq,female.15.49,na.rm=TRUE)
),by=.(p20,forced)]
hurt.preg.tab <- hurt.preg[,.(
  Freq = weighted.mean(Freq,female.15.49,na.rm=TRUE)
),by=.(p20,hurt.preg)]

write.csv(forced,"forced-nationally.csv",na="",row.names=FALSE)
write.csv(hurt.preg,"hurt-preg-nationally.csv",na="",row.names=FALSE)
