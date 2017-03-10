library(data.table)
library(Hmisc)
library(plyr)

wd <- "D:/Documents/Data/MNM/formatted"
setwd(wd)

files <- list.files(wd,pattern="*.csv",full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(file in files){
  dataList[[dataIndex]] <- read.csv(file,na.strings="",as.is=TRUE)
  message(names(dataList[[dataIndex]]))
  dataIndex <- dataIndex + 1
}

dat <- join_all(dataList,by=c("id","year"))
dat$mc.oda.pc <- dat$mc.oda/(dat$pop/100000)

mean.mc.oda.pc <- data.table(dat)[,.(mean.mc.oda.pc=mean(mc.oda.pc,na.rm=TRUE)),by=.(year)]
dat <- merge(dat,mean.mc.oda.pc,by=c("year"),all=TRUE)
dat$demeaned.mc <- dat$mc.oda.pc - dat$mean.mc.oda.pc

mean.deaths.per.100k <- data.table(dat)[,.(mean.deaths.per.100k=mean(deaths.per.100k,na.rm=TRUE)),by=.(year)]
dat <- merge(dat,mean.deaths.per.100k,by=c("year"),all=TRUE)
dat$demeaned.dp100k <- dat$deaths.per.100k - dat$mean.deaths.per.100k

plot(demeaned.dp100k~demeaned.mc,data=subset(dat,deaths.per.100k>0))
quadrant <- function(xV,yV){
  results <- c()
  for(i in 1:length(xV)){
    x <- xV[i]
    y <- yV[i]
    if(is.na(x) | is.na(y)){
      result <- NA
    }else{
      if(x>0 & y>0){result <- "i"}
      if(x<0 & y>0){result <- "ii"}
      if(x<0 & y<0){result <- "iii"}
      if(x>0 & y<0){result <- "iv"}
      if(x==0){
        if(y>0){result <- "i"}
        if(y<0){result <- "iii"}
        if(y==0){result <- "origin"}
      }
      if(y==0){
        if(x>0){result <- "i"}
        if(x<0){result <- "iii"}
      }
    }
    results <- c(results,result)
  }
  return(results)
}
dat$quadrant.mc <- quadrant(dat$demeaned.mc,dat$demeaned.dp100k)
describe(dat$quadrant.mc)

mean.health.exp <- data.table(dat)[,.(mean.health.exp=mean(health.exp,na.rm=TRUE)),by=.(year)]
dat <- merge(dat,mean.health.exp,by=c("year"),all=TRUE)
dat$demeaned.he <- dat$health.exp - dat$mean.health.exp

plot(demeaned.dp100k~demeaned.he,data=subset(dat,deaths.per.100k>0))
dat$quadrant.he <- quadrant(dat$demeaned.he,dat$demeaned.dp100k)
describe(dat$quadrant.he)

library(ggplot2)
ggplot(subset(dat,deaths.per.100k>0),aes(x=demeaned.he,y=demeaned.dp100k,colour=quadrant.he)) + geom_point()
ggplot(subset(dat,deaths.per.100k>0),aes(x=demeaned.mc,y=demeaned.dp100k,colour=quadrant.mc)) + geom_point()

ggplot(dat,aes(x=mc.oda.pc,y=deaths.per.100k,size=log(health.exp),colour=quadrant.mc)) + geom_point() +
  xlim(-0.1,.75) +
  labs(x="Malaria control ODA per 100k population"
      ,y="Deaths due to malaria per 100k population"
      ,title="Malaria ODA spending vs. Health outcomes"
      ,size="Log of health spending per capita"
      ,colour="Demeaned quadrant"
      )

ggplot(dat,aes(x=health.exp,y=deaths.per.100k,size=mc.oda.pc,colour=quadrant.he)) + geom_point() +
  xlim(0,1000) +
  labs(x="Health spending per capitaHealth spending per capita"
       ,y="Deaths due to malaria per 100k population"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Log Malaria control ODA per 100k population"
       ,colour="Demeaned quadrant"
  )
ggplot(dat,aes(x=mc.oda,y=cases,size=health.exp*pop)) + geom_point() +
  labs(x="Malaria control ODA"
       ,y="Reported malaria cases"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Health expenditure"
  )

dat$oda.per.case <- dat$mc.oda/dat$cases
ggplot(dat,aes(x=oda.per.case,y=health.exp)) + geom_point() + ylim(0,100)
dat$mc.oda.as.percent.govtspend <- (dat$mc.oda/dat$govspend.usd)*100
ggplot(dat,aes(x=mc.oda.as.percent.govtspend,y=cases,colour=deaths)) +
  geom_point(alpha=0.5) + xlim(0,7.5) + theme_bw() + scale_colour_gradient(low="blue",high="red")
ggplot(dat,aes(x=mc.oda.as.percent.govtspend,y=cases)) + geom_point() + xlim(0,7.5)
ggplot(dat,aes(x=mc.oda,y=deaths,size=health.exp,colour=cases)) + geom_point(alpha=0.5) + scale_colour_gradient(low="steelblue",high="red")

dat$year <- factor(dat$year)
dat$id <- factor(dat$id)
dat$deaths.pc <- dat$deaths/dat$pop
fit <- lm(deaths.pc~mc.oda.pc+pop+health.exp+id+year,data=dat)
summary(fit)
# complete <- dat[which(complete.cases(dat)),]
# cor(complete[c(names(complete)[3:12])])