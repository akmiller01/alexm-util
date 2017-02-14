library(Hmisc)
library(data.table)
library(zoo)
library(plyr)
library(WDI)

wd <- "D:/Documents/Data/PovCal_Increment"
setwd(wd)

agg.header <- c(
  "country"
  ,"year"
  ,"type"
  ,"pl"
  ,"mean"
  ,"hc"
  ,"pg"
  ,"pg2"
  ,"watts"
  ,"pop"
  ,"svyYear"
  ,"detail"
)

agg <- fread(
  "aggregates.csv"
  ,colClasses = c(
    "character"
    ,"numeric"
    ,"character"
    ,rep("numeric",7)
    ,"character"
    ,"character"
    )
  ,header=FALSE
  ,col.names=agg.header
  ,na.strings=c("NaN.00","n/a")
  )

ind.header <- c(
  "country"
  ,"year"
  ,"type"
  ,"mean"
  ,"pl"
  ,"hc"
  ,"pg"
  ,"pg2"
  ,"watts"
  ,"gini"
  ,"median"
  ,"mld"
  ,"pop"
  ,"detail"
)

ind <- fread(
  "individual.csv"
  ,colClasses = c(
    "character"
    ,"numeric"
    ,"character"
    ,rep("numeric",10)
    ,"character"
  )
  ,header=FALSE
  ,col.names=ind.header
  ,na.strings=c("NaN.00","n/a","Interpolated","Weighted sum","Weightedsum","Wght. interpolation")
)

# save(agg,ind,file="total.RData")
# 
# load("total.RData")

agg <- subset(agg,!grepl("--",country,fixed=TRUE))
ind <- subset(ind,!grepl("--",country,fixed=TRUE))

agg.dup <- agg[,.(count=sum(!is.na(type))),by=.(country,year,pl)]
agg.dup <- subset(agg.dup,count>1)
agg.dup$id <- paste(agg.dup$country,agg.dup$year,agg.dup$pl)
agg$id <- paste(agg$country,agg$year,agg$pl)
all.dups <- agg[which(agg$id %in% agg.dup$id),]
good.dups <- subset(all.dups,svyYear=="Interpolated")
agg <- agg[!(agg$id %in% agg.dup$id),]
agg <- rbind(agg,good.dups)
agg[,("id"):=NULL]

#Load in pop and fix Romania
indicator <- "SP.POP.TOTL"

pop <- WDI(country = "all", 
           indicator = indicator, 
           start = 1981, 
           end = 2013
)

rom <- agg[which(agg$country=="Romania"),]
rom[is.na(rom)] <- 0
agg <- agg[!(agg$country=="Romania"),]
rom <- merge(rom,pop,by=c("year","country"))
rom$pop <- rom$SP.POP.TOTL/1000000
rom[,(c("SP.POP.TOTL","iso2c")):=NULL]
agg <- rbind(agg,rom)
agg <- agg[order(-agg$year,agg$pl,agg$country),]

ctryDiff <- setdiff(unique(ind$country),unique(agg$country))
ind <- subset(ind,country %in% ctryDiff)
#Only 1 year leads to interpolation trouble...
ind <- subset(ind,country!="Japan")

ind$svyYear <- ind$year
all.perms <- expand.grid(unique(ind$country),unique(ind$year),unique(ind$pl))
names(all.perms) <- c("country","year","pl")
ind <- merge(ind,all.perms,by=c("country","year","pl"),all=TRUE)

ind.interp <- ind[,.(
  year=year
  ,hc=na.approx(hc,rule=2)
  ,pg=na.approx(pg,rule=2)
  ,mean=na.approx(mean,rule=2)
  ,svyYear=svyYear
  ,original.hc=hc
  ),by=.(country,pl)]

ind.interp <- merge(ind.interp,pop,by=c("country","year"))
ind.interp$pop <- ind.interp$SP.POP.TOTL/1000000
remove <- c("original.hc","iso2c","SP.POP.TOTL")
ind.interp[,(remove):=NULL]
ind.interp$type <- "i"

remove <- c("pg2","watts","detail")
agg[,(remove):=NULL]

pcn <- rbind(agg,ind.interp)
save(pcn,pop,file="pcn.RData")
# load("pcn.RData")

differentSpellings <- setdiff(pcn$country,pop$country)
missingCountries <- setdiff(pop$country,pcn$country)
missingCountries <- missingCountries[48:121]
remove <- c(16,32,33,17,18,34,42,47,50,72)
differentSpellings <- missingCountries[remove]
missingCountries <- subset(missingCountries,! missingCountries %in% differentSpellings)
mcPop <- subset(pop,year==2013 & country %in% missingCountries)
sum(mcPop$SP.POP.TOTL,na.rm=TRUE)
View(mcPop)[order(mcPop$SP.POP.TOTL),]

pcn$poorpop <- (pcn$hc/100)*pcn$pop
# write.csv(pcn,"pcn.csv",row.names=FALSE)
# write.csv(subset(pcn,country %in% ctryDiff),"lissy.csv",na="",row.names=FALSE)
years <- c(2013, 2012, 2011, 2010, 2008, 2005, 2002, 1999, 1996, 1993, 1990, 1987, 1984, 1981)
for(i in years){
  filename <- paste("years/pcn",i,"csv",sep=".")
  # write.csv(subset(pcn,year==i),filename,row.names=FALSE,na="")
  filename <- paste("lissy_years/lissy",i,"csv",sep=".")
  # write.csv(subset(pcn,year==i & country %in% ctryDiff),filename,row.names=FALSE,na="")
}

firstOverTwenty <- function(hc.vector,pl.vector){
  minimum.value <- 100
  minimum.index <- 0
  for(i in 1:length(hc.vector)){
    x <- hc.vector[i]
    diff <- abs(20-x)
    if(diff<minimum.value){
      minimum.value <- diff
      minimum.index <- i
    }
  }
  return(c(hc.vector[minimum.index],pl.vector[minimum.index]))
}
pcn <- pcn[order(pcn$hc),]
np20 <- subset(pcn,!is.na(hc))[,.(hc=firstOverTwenty(hc,pl)[1],pl=firstOverTwenty(hc,pl)[2]),by=.(country,year)]
np20 <- merge(np20,pcn,by=c("country","year","hc","pl"))
# write.csv(np20,"np20.csv",row.names=FALSE,na="")

mcPop.years <- subset(pop,country %in% missingCountries)
mcPop.years <- data.table(mcPop.years)[,.(pop=sum(SP.POP.TOTL,na.rm=TRUE)),by=.(year)]
world.pop <- subset(pop,country=="World")
world.pop <- merge(world.pop,mcPop.years,by="year")
world.pop$world.pop <- world.pop$SP.POP.TOTL-world.pop$pop 
world.pop <- world.pop[c("year","world.pop")]
pcn <- merge(pcn,world.pop,by=c("year"))
pcn <- transform(pcn,pop.perc = (poorpop*1000000)/world.pop)
global.percent <- pcn[,.(percent=sum(pop.perc,na.rm=TRUE)),by=.(year,pl)]
year <- c()
p20.threshold <- c()
p20.level <- c()
for(this.year in unique(global.percent$year)){
  global.percent.dat <- subset(global.percent,year==this.year)
  percents <- global.percent.dat$percent
  minimum.value <- 100
  minimum.index <- 0
  for(i in 1:length(percents)){
    x <- percents[i]
    diff <- abs(0.20-x)
    if(diff<minimum.value){
      minimum.value <- diff
      minimum.index <- i
    }
  }
  year <- c(year,this.year)
  p20.threshold <- c(p20.threshold,global.percent.dat$pl[minimum.index])
  p20.level <- c(p20.level,global.percent.dat$percent[minimum.index])
}
p20 <- data.frame(year,p20.threshold,p20.level)
p20 <- subset(p20,p20.threshold<10)
plot(percent~pl,data=subset(global.percent,year==1981 & pl<5),type="l")

setwd("D:/Documents/Data/P20_gif")
for(this.year in unique(p20$year)){
  filename = paste0(this.year,".png")
  png(filename)
  plot(percent*100~pl,data=subset(global.percent,year==this.year & pl<5),ylim=c(0,100),type="l",main=paste0("World population under $5 a day - ",this.year," (derived from PovcalNet)"),xlab="Poverty line (2011 $ PPP)",ylab="Percent of global population")
  dev.off()
}

popbypl <- pcn[,.(poorpop=sum(poorpop,na.rm=TRUE)),by=.(pl,year)]
# write.csv(popbypl,"popbypl.csv",row.names=FALSE)
