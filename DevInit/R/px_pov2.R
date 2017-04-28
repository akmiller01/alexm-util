library(data.table)
library(Hmisc)
library(plyr)

setwd("D:/Documents/Data/EX")

dat <- read.csv("poor_pop.csv",na.strings=FALSE)

odaList <- list()
odaIndex <- 1
oda.files <- c(
  "D:/git/digital-platform/country-year/oda.csv"
  ,"D:/git/digital-platform/country-year/oda.2000.csv"
  ,"D:/git/digital-platform/country-year/oda.2001.csv"
  ,"D:/git/digital-platform/country-year/oda.2002.csv"
  ,"D:/git/digital-platform/country-year/oda.2003.csv"
  ,"D:/git/digital-platform/country-year/oda.2004.csv"
  ,"D:/git/digital-platform/country-year/oda.2005.csv"
  ,"D:/git/digital-platform/country-year/oda.2006.csv"
  ,"D:/git/digital-platform/country-year/oda.2007.csv"
  ,"D:/git/digital-platform/country-year/oda.2008.csv"
  ,"D:/git/digital-platform/country-year/oda.2009.csv"
  ,"D:/git/digital-platform/country-year/oda.2010.csv"
  ,"D:/git/digital-platform/country-year/oda.2011.csv"
  ,"D:/git/digital-platform/country-year/oda.2012.csv"
  ,"D:/git/digital-platform/country-year/oda.2013.csv"
  ,"D:/git/digital-platform/country-year/oda.2014.csv"
  ,"D:/git/digital-platform/country-year/oda.2015.csv"
  )

for(file in oda.files){
  temp <- read.csv(file,na.strings="")
  odaList[[odaIndex]] <- temp
  odaIndex <- odaIndex + 1
}

oda <- rbindlist(odaList)

uk.oda <- subset(oda,id.from=="GB")
uk.oda.tab <- data.table(uk.oda)[,.(oda=sum(value,na.rm=TRUE)),by=.(id.to,year)]
names(uk.oda.tab) <- c("id","year","oda")
uk.oda.tab$oda[which(uk.oda.tab$oda<0)] <- 0

dat <- join(dat,uk.oda.tab,by=c("id","year"))
dat$oda[which(is.na(dat$oda))] <- 0

entity <- read.csv("D:/git/digital-platform/reference/entity.csv",na.strings="",as.is=TRUE)
entity <- entity[c("id","region")]
dat <- join(dat,entity,by="id")
regionDict <- list(
  "europe"="Europe"               
  ,"middle-east"="Middle East"          
  ,"south-central-asia"="South central Asia"   
  ,"north-central-america"="North Central America"
  ,"south-of-sahara"="Sub-Saharan Africa"      
  ,"south-america"="South America"        
  ,"oceania"="Oceania"              
  ,"far-east-asia"="Far East Asia"        
  ,"north-of-sahara"="North Africa"
)
dat$region <- sapply(regionDict[dat$region],`[[`,index=1)

plot.dat <- dat[c("country.name","year","ext","ext.pop","oda","region")]
plot.dat <- plot.dat[complete.cases(plot.dat),]
write.csv(plot.dat,"D:/Documents/Data/EX/animated_bubble/data.csv",row.names=FALSE,na="")
