library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
library(venneuler)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
# load("total_triple.RData")
# 
# countryMeta <- read.csv("headcounts.csv",as.is=TRUE)
# 
# stunted = c("Severely stunted","Stunted, but not severely")
# data.total$p20 <- as.integer(data.total$p20)
# data.total$stunted <- as.integer(data.total$stunting %in% stunted)
# data.total$stunted[which(is.na(data.total$stunting))] <- NA
# data.total$no.birth.reg <- !as.double(data.total$birth.reg)
# data.total <- data.total[complete.cases(data.total$weights),]
# 
# s <- 0
# r <- 0
# p <- 0
# sr <- 0
# rp <- 0
# ps <- 0
# srp <- 0
# 
# filenames <- countryMeta$filename
# for(i in 1:length(filenames)){
#   this.filename <- filenames[i]
#   message(this.filename)
#   dat <- subset(data.total,filename==this.filename)
#   under5 <- subset(dat,age<5)
#   if(nrow(dat)>0){
#     this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
#     this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
#     this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
#     #Under5 nutrition
#     if(length(under5$stunting[which(!is.na(under5$stunting))])!=0){
#       if(length(under5$no.birth.reg[which(!is.na(under5$no.birth.reg))])!=0){
#         if(length(under5$p20[which(!is.na(under5$p20))])!=0){
#           s = s + weighted.mean(under5$stunted,under5$weights,na.rm=TRUE)*this.pop.under5
#           r = r + weighted.mean(under5$no.birth.reg,under5$weights,na.rm=TRUE)*this.pop.under5
#           p = p + weighted.mean(under5$p20,under5$weights,na.rm=TRUE)*this.pop.under5
#           
#           sr = sr + weighted.mean(under5$stunted&under5$no.birth.reg,under5$weights,na.rm=TRUE)*this.pop.under5
#           rp = rp + weighted.mean(under5$no.birth.reg&under5$p20,under5$weights,na.rm=TRUE)*this.pop.under5
#           ps = ps + weighted.mean(under5$p20&under5$stunted,under5$weights,na.rm=TRUE)*this.pop.under5
#           
#           srp = srp + weighted.mean(under5$stunted&under5$no.birth.reg&under5$p20,under5$weights,na.rm=TRUE)*this.pop.under5
#         }
#       }
#     }
#   }
# }
# 
# save(s,r,p,sr,rp,ps,srp,file="venn.RData")
load("venn.RData")

s <- round(s/1000000)
r <- round(r/1000000)
p <- round(p/1000000)
sr <- round(sr/1000000)
rp <- round(rp/1000000)
ps <- round(ps/1000000)
srp <- round(srp/1000000)

v <- venneuler(c(
  A=s
  ,B=r
  ,C=p
  ,"A&B"=sr
  ,"A&C"=ps
  ,"B&C"=rp
  ,"A&B&C"=srp
  ))
v$labels <- rep("",3)
plot(v)
v$labels <- c("Stunted","Not registered","P20")
plot(v)
