library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
library(venneuler)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("total_triple.RData")

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

stunted = c("Severely stunted","Stunted, but not severely")
data.total$p20 <- as.integer(data.total$p20)
data.total$stunted <- as.integer(data.total$stunting %in% stunted)
data.total$stunted[which(is.na(data.total$stunting))] <- NA
data.total$no.birth.reg <- !as.double(data.total$birth.reg)
data.total <- data.total[complete.cases(data.total$weights),]

s <- list(
  low = 0
  ,estimate = 0
  ,high = 0
  )
r <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)
p <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)
sr <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)
rp <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)
ps <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)
srp <- list(
  low = 0
  ,estimate = 0
  ,high = 0
)

confidence.interval <- function(x,w,pop){
  percent = weighted.mean(x,w,na.rm=TRUE)
  n = length(which(!is.na(x)))
  cv <- sd(w,na.rm=TRUE)/mean(w,na.rm=TRUE)
  deft <- cv*cv+1
  SE <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(percent*(1-percent)))
  corrected.SE <- SE*deft
  low.end <- (percent-(2*corrected.SE))*pop
  low.end <- max(low.end,0)
  estimate.point <- percent*pop
  high.end <- (percent+(2*corrected.SE))*pop
  high.end <- min(high.end,pop)
  return(
    list(
      low = low.end
      ,estimate = estimate.point
      ,high = high.end
      )
  )
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  under5 <- subset(dat,age<5)
  if(nrow(dat)>0){
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    #Under5 nutrition
    if(length(under5$stunting[which(!is.na(under5$stunting))])!=0){
      if(length(under5$no.birth.reg[which(!is.na(under5$no.birth.reg))])!=0){
        if(length(under5$p20[which(!is.na(under5$p20))])!=0){
          s.intervals = confidence.interval(under5$stunted,under5$weights,this.pop.under5)
          r.intervals = confidence.interval(under5$no.birth.reg,under5$weights,this.pop.under5)
          p.intervals = confidence.interval(under5$p20,under5$weights,this.pop.under5)
          
          sr.intervals = confidence.interval(under5$stunted&under5$no.birth.reg,under5$weights,this.pop.under5)
          rp.intervals = confidence.interval(under5$no.birth.reg&under5$p20,under5$weights,this.pop.under5)
          ps.intervals = confidence.interval(under5$p20&under5$stunted,under5$weights,this.pop.under5)
                                             
          srp.intervals = confidence.interval(under5$stunted&under5$no.birth.reg&under5$p20,under5$weights,this.pop.under5)
          
          s$low = s$low + s.intervals$low
          s$estimate = s$estimate + s.intervals$estimate
          s$high = s$high + s.intervals$high
          r$low = r$low + r.intervals$low
          r$estimate = r$estimate + r.intervals$estimate
          r$high = r$high + r.intervals$high
          p$low = p$low + p.intervals$low
          p$estimate = p$estimate + p.intervals$estimate
          p$high = p$high + p.intervals$high
          
          sr$low = sr$low + sr.intervals$low
          sr$estimate = sr$estimate + sr.intervals$estimate
          sr$high = sr$high + sr.intervals$high
          rp$low = rp$low + rp.intervals$low
          rp$estimate = rp$estimate + rp.intervals$estimate
          rp$high = rp$high + rp.intervals$high
          ps$low = ps$low + ps.intervals$low
          ps$estimate = ps$estimate + ps.intervals$estimate
          ps$high = ps$high + ps.intervals$high
          
          srp$low = srp$low + srp.intervals$low
          srp$estimate = srp$estimate + srp.intervals$estimate
          srp$high = srp$high + srp.intervals$high
        }
      }
    }
  }
}

save(s,r,p,sr,rp,ps,srp,file="venn2.RData")
load("venn2.RData")

s <- round(s$estimate/1000000)
r <- round(r$estimate/1000000)
p <- round(p$estimate/1000000)
sr <- round(sr$estimate/1000000)
rp <- round(rp$estimate/1000000)
ps <- round(ps$estimate/1000000)
srp <- round(srp$estimate/1000000)

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
