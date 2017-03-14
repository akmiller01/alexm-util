library(Hmisc)
library(plyr)
library(data.table)

setwd("D:/Documents/Data/P20_2013/meta")
load("total_triple.RData")

man.bmi.tab <- data.table(subset(data.total,!is.na(man.bmi)))
man.bmi.tab <- man.bmi.tab[,.(
  severe.thinness = weighted.mean(man.bmi.class=="Severe thinness",weights,na.rm=TRUE)
  ,moderate.thinness = weighted.mean(man.bmi.class=="Moderate thinness",weights,na.rm=TRUE)
  ,mild.thinness = weighted.mean(man.bmi.class=="Mild thinness",weights,na.rm=TRUE)
  ,normal.range = weighted.mean(man.bmi.class=="Normal range",weights,na.rm=TRUE)
  ,pre.obese = weighted.mean(man.bmi.class=="Pre-obese",weights,na.rm=TRUE)
  ,obese.class.i = weighted.mean(man.bmi.class=="Obese class I",weights,na.rm=TRUE)
  ,obese.class.ii = weighted.mean(man.bmi.class=="Obese class II",weights,na.rm=TRUE)
  ,obese.class.iii = weighted.mean(man.bmi.class=="Obese class III",weights,na.rm=TRUE)
)
,by=.(filename)]

woman.bmi.tab <- data.table(subset(data.total,!is.na(woman.bmi)))
woman.bmi.tab <- woman.bmi.tab[,.(
  severe.thinness = weighted.mean(woman.bmi.class=="Severe thinness",weights,na.rm=TRUE)
  ,moderate.thinness = weighted.mean(woman.bmi.class=="Moderate thinness",weights,na.rm=TRUE)
  ,mild.thinness = weighted.mean(woman.bmi.class=="Mild thinness",weights,na.rm=TRUE)
  ,normal.range = weighted.mean(woman.bmi.class=="Normal range",weights,na.rm=TRUE)
  ,pre.obese = weighted.mean(woman.bmi.class=="Pre-obese",weights,na.rm=TRUE)
  ,obese.class.i = weighted.mean(woman.bmi.class=="Obese class I",weights,na.rm=TRUE)
  ,obese.class.ii = weighted.mean(woman.bmi.class=="Obese class II",weights,na.rm=TRUE)
  ,obese.class.iii = weighted.mean(woman.bmi.class=="Obese class III",weights,na.rm=TRUE)
)
,by=.(filename)]

wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
male.pop <- povcalcuts[c("filename","male.15.49")]
man.bmi.tab <- join(man.bmi.tab,male.pop,by="filename")
write.csv(man.bmi.tab,"male.bmi.csv",row.names=FALSE,na="")
female.pop <- povcalcuts[c("filename","female.15.49")]
woman.bmi.tab <- join(woman.bmi.tab,female.pop,by="filename")
write.csv(woman.bmi.tab,"female.bmi.csv",row.names=FALSE,na="")
