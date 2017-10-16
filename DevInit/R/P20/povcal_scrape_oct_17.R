library(Hmisc)
library(data.table)
library(readr)
library(WDI)

wd <- "/Users/Alex/Documents/Data/PovCal_Increment"
setwd(wd)

# agg.header <- c(
#   "country"
#   ,"year"
#   ,"type"
#   ,"pl"
#   ,"mean"
#   ,"hc"
#   ,"pg"
#   ,"pg2"
#   ,"watts"
#   ,"gini"
#   ,"median"
#   ,"mld"
#   ,"pop"
#   ,"svyYear"
#   ,"detail"
# )
# 
# agg <- read_csv("regional.csv",col_names=agg.header)
# 
# ind.header <- c(
#   "country"
#   ,"year"
#   ,"type"
#   ,"mean"
#   ,"pl"
#   ,"hc"
#   ,"pg"
#   ,"pg2"
#   ,"watts"
#   ,"gini"
#   ,"median"
#   ,"mld"
#   ,"pop"
#   ,"detail"
# )
# 
# ind <- read_csv("individual.csv",col_names=ind.header)
# 
# ind <- subset(ind,!grepl("--",country,fixed=TRUE))
# ind <- data.table(ind)
# 
# agg <- subset(agg,!grepl("--",country,fixed=TRUE))
# agg <- data.table(agg)
# agg.dup <- agg[,.(count=sum(!is.na(type))),by=.(country,year,pl)]
# agg.dup <- subset(agg.dup,count>1)
# agg.dup$id <- paste(agg.dup$country,agg.dup$year,agg.dup$pl)
# agg$id <- paste(agg$country,agg$year,agg$pl)
# all.dups <- agg[which(agg$id %in% agg.dup$id),]
# good.dups <- subset(all.dups,svyYear=="Interpolated")
# agg <- agg[!(agg$id %in% agg.dup$id),]
# agg <- rbind(agg,good.dups)
# agg[,("id"):=NULL]
# 
# 
# agg <- agg[order(-agg$year,agg$pl,agg$country),]

# save(agg,ind,file="agg.ind.RData")
load("agg.ind.RData")

# iso3s <- WDI(country="all",indicator="SP.POP.TOTL",start=1990,end=2015,extra=TRUE)
# iso3s <- unique(iso3s[c("country","iso3c")])
# iso3s <- iso3s[complete.cases(iso3s),]
# countries <- unique(c(agg$country,ind$country))
# countries <- data.frame(country=countries)
# countries <- merge(countries,iso3s,by="country",all.x=TRUE)
# write.csv(countries,"iso_key.csv",na="",row.names=FALSE)

iso.key <- read.csv("iso_key.csv",na.strings="")

ind <- merge(ind,iso.key,by="country",all.x=TRUE)
agg <- merge(agg,iso.key,by="country",all.x=TRUE)

growth <- read.csv("GDPgrowthRates.csv",na.strings="")
lkg10 <- growth[c("iso3c","Laknergrowth10")]
lkg20 <- growth[c("iso3c","Laknergrowth20")]

countries <- unique(agg$country)
agg2013 <- subset(agg,year==2013 & svyYear=="2013")
countries2013 <- unique(agg2013$country)
not2013 <- setdiff(countries,countries2013)

agg.test <- subset(agg,country %in% not2013)
agg.test2012 <- subset(agg.test,year==2012)
agg.test2013 <- subset(agg,year==2013 & country %in% not2013)

agg.test2012 <- merge(agg.test2012,lkg20,by="iso3c")
agg.test2012$pl2013 <- agg.test2012$pl*(1+agg.test2012$Laknergrowth20)
agg.test2012 <- agg.test2012[,c("country","pl2013","hc")]
# years <- unique(pcn$year)
# for(this.year in years){
#   filename <- paste0("years/pcn",this.year,".csv")
#   message(filename)
#   write.csv(subset(pcn,year==this.year),filename,na="",row.names=FALSE)
# }