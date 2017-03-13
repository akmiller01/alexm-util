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
  message(basename(file))
  dataIndex <- dataIndex + 1
}

dat <- join_all(dataList,by=c("id","year"))
dat$mc.oda.pc <- dat$mc.oda/(dat$pop/100000)
write.csv(dat,"../bubble_data.csv",row.names=FALSE,na="")

library(ggplot2)

ggplot(dat,aes(x=mc.oda.pc,y=deaths.per.100k,size=log(health.exp),colour=region)) + geom_point() +
  xlim(-0.1,.75) +
  labs(x="Malaria control ODA per 100k population"
      ,y="Deaths due to malaria per 100k population"
      ,title="Malaria ODA spending vs. Health outcomes"
      ,size="Log of health spending per capita"
      ,colour="Region"
      )

ggplot(dat,aes(x=health.exp,y=deaths.per.100k,size=mc.oda.pc,colour=region)) + geom_point() +
  xlim(0,1000) +
  labs(x="Health spending per capitaHealth spending per capita"
       ,y="Deaths due to malaria per 100k population"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Log Malaria control ODA per 100k population"
       ,colour="Region"
  )
ggplot(dat,aes(x=mc.oda,y=cases,size=health.exp*pop,colour=region)) + geom_point() +
  labs(x="Malaria control ODA"
       ,y="Reported malaria cases"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Health expenditure"
       ,colour="Region"
  )

dat$year <- factor(dat$year)
dat$id <- factor(dat$id)
dat$deaths.pc <- dat$deaths/dat$pop
fit <- lm(deaths.pc~mc.oda.pc+pop+health.exp+id+year,data=dat)
summary(fit)

cor(dat[c(3:7,9:13)],use="pairwise.complete.obs")

# complete <- dat[which(complete.cases(dat)),]
# cor(complete[c(names(complete)[3:12])])