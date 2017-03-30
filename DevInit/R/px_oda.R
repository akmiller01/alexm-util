library(data.table)
library(plyr)

wd <- "~/git/digital-platform/country-year"
setwd(wd)

oda.files <- c(
  "oda.csv"
  ,"oda.2000.csv"
  ,"oda.2001.csv"
  ,"oda.2002.csv"
  ,"oda.2003.csv"
  ,"oda.2004.csv"
  ,"oda.2005.csv"
  ,"oda.2006.csv"
  ,"oda.2007.csv"
  ,"oda.2008.csv"
  ,"oda.2009.csv"
  ,"oda.2010.csv"
  ,"oda.2011.csv"
  ,"oda.2012.csv"
  ,"oda.2013.csv"
  ,"oda.2014.csv"
  ,"oda.2015.csv"
)

odaList <- list()
odaIndex <- 1

for(file in oda.files){
  message(file)
  temp <- read.csv(file,na.strings="")
  odaList[[odaIndex]] <- temp
  odaIndex <- odaIndex + 1
}

oda <- rbindlist(odaList)
entity <- read.csv("../reference/entity.csv",na.strings="")
entity <- entity[c("id","name")]
names(entity) <- c("id.from","name.from")
oda <- join(oda,entity,by="id.from")

oda.tab <- data.table(oda)[,.(oda=sum(value,na.rm=TRUE)),by=.(name.from,year)]
multi <- read.csv("/Users/Alex/Downloads/home_work/bubble/multi.csv",na.strings="")
oda.tab <- join(oda.tab,multi,by="name.from")
# write.csv(oda.tab,"/Users/Alex/Downloads/home_work/bubble/all_oda.csv",row.names=FALSE,na="")

oda.recip.tab <- data.table(oda)[,.(oda=sum(value,na.rm=TRUE)),by=.(name.from,id.to,year)]
pov.names <- read.csv("/Users/Alex/Downloads/home_work/bubble/pov4.csv",na.strings="")
pov.names <- unique(pov.names[c("name","id")])
pov.names <- pov.names[complete.cases(pov.names),]
names(pov.names) <- c("name.to","id.to")
oda.recip.tab <- join(oda.recip.tab,pov.names,by="id.to")
poor.cat <- read.csv("/Users/Alex/Downloads/home_work/bubble/poor_cat.csv",na.strings="")
names(poor.cat) <- c("name.to","year","pov.cat")
oda.recip.tab <- join(oda.recip.tab,poor.cat,by=c("name.to","year"))
pov.cat.tab <- data.table(oda.recip.tab)[,.(oda=sum(oda,na.rm=TRUE)),by=.(name.from,pov.cat,year)]
pov.cat.tab$pov.cat <- factor(pov.cat.tab$pov.cat,levels=c("Less than 25%","25 - 50%","Greater than 50%","Recipients without poverty data"))
pov.cat.tab$pov.cat[which(is.na(pov.cat.tab$pov.cat))] <- "Recipients without poverty data"
pov.cat.tab <- subset(pov.cat.tab,year>=1999)
pov.cat.tab <- join(pov.cat.tab,multi,by="name.from")
write.csv(pov.cat.tab,"/Users/Alex/Downloads/home_work/bubble/oda_pov_cat.csv",na="",row.names=FALSE)
