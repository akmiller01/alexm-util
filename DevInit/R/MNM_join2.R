library(plyr)

setwd("/Users/Alex/Downloads/home_work/MNM/grouping_data")

cases <- read.csv("reported_cases.csv",na.strings="")
deaths <- read.csv("reported_deaths.csv",na.strings="")
ids <- read.csv("ids.csv",na.strings="")
eliminating <- read.csv("eliminating.csv",na.strings="")
burden <- read.csv("burden.csv",na.strings="")
pop <- read.csv("~/git/digital-platform/country-year/population-total.csv",na.strings="")
names(pop) <- c("id","year","pop")
dat <- join(cases,deaths,by=c("Country","year"))
dat <- join(dat,burden,by="Country")
dat <- join(dat,ids,by="Country")
dat <- join(dat,eliminating,by="id")
dat <- join(dat,pop,by=c("id","year"))
dat$eliminating[which(is.na(dat$eliminating))] <- FALSE
dat$high_burden[which(is.na(dat$high_burden))] <- FALSE

dat <- subset(dat,reported_cases>0)
dat <- transform(dat,
                 cases.p100k = reported_cases/(pop/100000)
                 ,deaths.p100k = reported_deaths/(pop/100000)
                 )

write.csv(dat,"../eliminating_joined.csv",na="",row.names=FALSE)

boxplot(deaths.p100k~eliminating,data=subset(dat,year==2014))
