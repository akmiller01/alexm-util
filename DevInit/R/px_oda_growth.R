library(data.table)

setwd("D:/Documents/Data/EX")

oda.2010 <- read.csv("oda.2010.csv",na.strings="")
oda.2015 <- read.csv("oda.2015.csv",na.strings="")

oda.2010 <- data.table(oda.2010)[,.(oda2010=sum(value,na.rm=TRUE)),by=.(id.from)]
oda.2015 <- data.table(oda.2015)[,.(oda2015=sum(value,na.rm=TRUE)),by=.(id.from)]
oda <- merge(oda.2010,oda.2015,by="id.from")
oda <- transform(oda,growth=((oda2015-oda2010)/oda2010))
write.csv(oda,"oda.growth.csv",na="",row.names=FALSE)

