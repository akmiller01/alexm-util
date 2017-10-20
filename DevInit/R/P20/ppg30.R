library(data.table)
library(reshape2)
library(zoo)

setwd("C:/Users/Alex/Documents/Data/PovCal_Increment")
load("pcn2014.RData")
load("agg.ind.RData")

iso.key <- read.csv("iso_key.csv",na.strings="")

agg <- merge(agg,iso.key,by="country",all.x=TRUE)

pcn <- rbind(agg[,c("country","iso3c","year","pl","hc","type")],pcn2014)

dat <- data.table(pcn)[,.(pl=max(pl)),by=.(country,year,hc)]
dat <- subset(dat,hc>0)
mdat <- melt(dat,id.vars=c("country","hc","year"))
wdat <- dcast(mdat,country+hc~year+variable,value.var="value")
names(wdat)[3:length(wdat)] <- paste0("x",names(wdat)[3:length(wdat)])

wdat <- subset(wdat,!is.na(x2014_pl) & !is.na(x1984_pl))
keep <- c("country","hc","x1984_pl","x2014_pl")
wdat <- data.frame(wdat)[keep]

expand <- expand.grid(unique(wdat$country),seq(0.01,100,0.01))
names(expand) <- c("country","hc")
expand <- merge(expand,wdat,by=c("country","hc"),all=TRUE)
interp <- data.table(expand)[,.(
  hc = hc
  ,x1984_pl=na.approx(x1984_pl,rule=2)
  ,x2014_pl=na.approx(x2014_pl,rule=2)
)
,by=.(country)
]

interp = transform(interp,annual_growth= ((x2014_pl-x1984_pl)/x1984_pl)/30 )

firstto190 = function(plVec){
  min.diff = 1000
  min.pl = 0
  for(pl in plVec){
    diff = abs(pl-1.90)
    if(diff<min.diff){
      min.diff = diff
      min.pl = pl
    }
  }
  return(min.pl)
}
  
interp190 = data.table(interp)[,.(approx190=firstto190(x1984_pl)),by=.(country)]
interp <- merge(interp,interp190,by="country")

original.dpd <- subset(interp,x1984_pl==approx190)

original.dpd <- data.table(original.dpd)[,.(hc=max(hc)),by=.(country,x1984_pl,x2014_pl,annual_growth)]
original.dpd <- merge(original.dpd,iso.key,by="country")

setwd("C:/Users/Alex/Documents/Data/P20/Meta/")
write.csv(original.dpd,"ppg30.csv",na="",row.names=FALSE)
