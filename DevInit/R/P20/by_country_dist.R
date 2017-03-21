library(data.table)
library(ggplot2)
library(plyr)
library(Hmisc)
require(gtools)
library(varhandle)

setwd("D:/Documents/Data/P20_2013/meta")
load("total_triple.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

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
quintile <- function(x,quintiles){
  for(i in 2:length(quintiles)){
    d <- quintiles[i]
    if(is.na(x)){return(NA)}
    if(x<=d){
      return(names(quintiles)[i])
    }
  }
  return(names(quintiles)[i])
}
shortAgeCat <- function(a){
  if(a %in% c(98,99,NA)){return(NA)}
  if(a<5){return("Under 5")}
  if(a>=5 & a<15){return("5 to 14")}
  if(a>=15 & a<50){return("15 to 49")}
  if(a>=50){return("50 or greater")}
}
rowwisePaste <- function(aV,bV,sep=" "){
  results <- c()
  for(i in 1:length(aV)){
    a <- aV[i]
    b <- bV[i]
    result <- paste(a,b,sep=sep)
    results <- c(results,result)
  }
  return(results)
}
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 
dist.data <- list()
dist.data.index <- 1

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  if(nrow(dat)>0){
    bin.count <- 5
    quintiles <- weighted.percentile(dat$wealth,dat$weights,prob=seq(0,1,1/bin.count))
    dat$quintile <- factor(sapply(dat$wealth,quintile,quintiles=quintiles)
                         ,levels=c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
    dat$shortAgeCat <- factor(sapply(dat$age,shortAgeCat),levels=c("Under 5","5 to 14","15 to 49","50 or greater"))
    if(is.factor(dat$region)){
      dat$region <- unfactor(dat$region)
    }
    dat$region <- sapply(dat$region,simpleCap)
    qggad <- c("quintile","sex","region","shortAgeCat")
    dat$stunted <- dat$stunting!="Not stunted"
    dat$not.reg <- dat$birth.reg==0
    dat$no.educ <- dat$educ=="No education, preschool"
    indicators <- c("stunted","not.reg")
    combinations <- list(
      list(x=c("quintile"))
      ,list(x=c("quintile","sex"))
      ,list(x=c("quintile","shortAgeCat"))
      ,list(x=c("region"))
      ,list(x=c("region","sex"))
      ,list(x=c("region","shortAgeCat"))
      ,list(x=c("sex"))
      ,list(x=c("sex","shortAgeCat"))
      ,list(x=c("shortAgeCat"))
    )
    for(comb in combinations){
      x <- comb$x
      plot.dat <- data.table(dat)
      setorderv(plot.dat,x)
      plot.dat <- plot.dat[,.(
        not.reg=weighted.mean(not.reg,weights,na.rm=TRUE)
        ,stunted=weighted.mean(stunted,weights,na.rm=TRUE)
        ,no.educ=weighted.mean(no.educ,weights,na.rm=TRUE)
        ,filename=max(filename)
        ),by=x]
      plot.dat$selection <- paste(x,collapse=",")
      if(length(x)==1){
        plot.dat$x <- plot.dat[,x,with=FALSE]
      }else{
        plot.dat$x <- rowwisePaste(plot.dat[,x[1],with=FALSE][[1]],plot.dat[,x[2],with=FALSE][[1]],sep=" and ")
      }
      plot.dat <- subset(plot.dat,!grepl("NA",x))
      plot.dat <- data.frame(plot.dat)
      plot.dat <- plot.dat[c("selection","x","not.reg","stunted","no.educ","filename")]
      plot.dat <- plot.dat[complete.cases(plot.dat$x),]
      if(nrow(plot.dat)>0){
        plot.dat$miss <- psum(is.na(plot.dat$not.reg) | is.nan(plot.dat$not.reg),is.na(plot.dat$stunted) | is.nan(plot.dat$stunted),is.na(plot.dat$no.educ) | is.nan(plot.dat$no.educ))
        plot.dat <- subset(plot.dat,miss<3)
        dist.data[[dist.data.index]] <- plot.dat
        dist.data.index <- dist.data.index + 1
      }
    }
  }
}

all.dist <- rbindlist(dist.data)
# plot.dat <- na.omit(subset(data.frame(all.dist)[c("x","selection","not.reg")],selection=="quintile,sex"))
# plot.dat$x <- factor(plot.dat$x)
# plot(not.reg~x,data=plot.dat,main=this.filename)

all.isos <- read.csv("D:/Documents/Data/DHSmeta/all.isos.csv",na.strings="")

all.dist.joined <- join(all.dist,all.isos,by="filename")
# all.dist.joined <- transform(all.dist.joined,
#                              not.reg = round(not.reg*100)
#                              ,stunted = round(stunted*100)
#                              ,no.educ = round(no.educ*100)
# )
write.csv(all.dist.joined,"C:/git/alexm-util/DevInit/P20-vis/dists/all.dist.joined.csv",row.names=FALSE,na="")