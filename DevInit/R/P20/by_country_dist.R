library(data.table)
library(ggplot2)
library(plyr)
library(Hmisc)
require(gtools)

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

dist.data <- list()
dist.data.index <- 1

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  if(nrow(dat)>0){
    bin.count <- 25
    max.wealth <- max(dat$wealth,na.rm=TRUE)
    min.wealth <- min(dat$wealth,na.rm=TRUE)
    weight.sum <- sum(dat$weights,na.rm=TRUE)
    wealth.stop <- (max.wealth-min.wealth)/bin.count
    stops <- c()
    counts <- c()
    regs <- c()
    p20s <- c()
    stunts <- c()
    for(j in 1:bin.count){
      bin.start <- min.wealth+(wealth.stop*(j-1))
      bin.end <- min.wealth+(wealth.stop*j)
      dat.subset <- subset(dat,wealth>=bin.start & wealth<bin.end)
      bin.weights <- sum(dat.subset$weights)
      bin.perc <- bin.weights/weight.sum
      weighted.reg <- weighted.mean(dat.subset$birth.reg==1,dat.subset$weights,na.rm=TRUE)
      weighted.p20 <- weighted.mean(dat.subset$p20,dat.subset$weights,na.rm=TRUE)
      weighted.stunting <- weighted.mean(dat.subset$stunting!="Not stunted",dat.subset$weights,na.rm=TRUE)
      stops <- c(stops,j)
      counts <- c(counts,bin.perc)
      regs <- c(regs,weighted.reg)
      p20s <- c(p20s,weighted.p20)
      stunts <- c(stunts,weighted.stunting)
    }
    wealth.dist <- data.frame(filename=this.filename,stops,counts,regs,p20s,stunts)
    dist.data[[dist.data.index]] <- wealth.dist
    dist.data.index <- dist.data.index + 1
#     ggplot(wealth.dist,aes(x=stops,y=counts,fill=regs)) + geom_bar(stat="identity")
#     ggplot(wealth.dist,aes(x=stops,y=counts,fill=stunts)) + geom_bar(stat="identity")
#     ggplot(wealth.dist,aes(x=stops,y=counts,fill=p20s)) + geom_bar(stat="identity")
  }
}

all.dist <- rbindlist(dist.data)
setwd("D:/Documents/P20 Visualizations")
write.csv(all.dist,"all.dist.csv",row.names=FALSE,na="")