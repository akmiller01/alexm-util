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

wdat <- subset(wdat,!is.na(x2010_pl) & !is.na(x2002_pl))
keep <- c("country","hc","x2002_pl","x2010_pl")
wdat <- data.frame(wdat)[keep]

expand <- expand.grid(unique(wdat$country),seq(0.01,100,0.01))
names(expand) <- c("country","hc")
expand <- merge(expand,wdat,by=c("country","hc"),all=TRUE)
interp <- data.table(expand)[,.(
  hc = hc
  ,x2002_pl=na.approx(x2002_pl,rule=2)
  ,x2010_pl=na.approx(x2010_pl,rule=2)
)
,by=.(country)
]

interp = transform(interp,annual_growth= ((x2010_pl-x2002_pl)/x2002_pl)/8 )

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

interp190 = data.table(interp)[,.(approx190=firstto190(x2002_pl)),by=.(country)]
interp <- merge(interp,interp190,by="country")

original.dpd <- subset(interp,x2002_pl==approx190)

original.dpd <- data.table(original.dpd)[,.(hc=max(hc)),by=.(country,x2002_pl,x2010_pl,annual_growth)]
original.dpd <- merge(original.dpd,iso.key,by="country")

# setwd("C:/Users/Alex/Documents/Data/P20/Meta/")
# write.csv(original.dpd,"ppg30.csv",na="",row.names=FALSE)


agg.mean <- unique(agg[,c("country","year","mean")])
agg.mean <- subset(agg.mean,year %in% c(2002,2010))
mdat.mean <- melt(agg.mean,id.vars=c("country","year"))
wdat.mean <- dcast(mdat.mean,country~year+variable,value.var="value")
names(wdat.mean) <- c("country","mean.2002","mean.2010")

svyMean <- transform(wdat.mean,growth=((mean.2010-mean.2002)/(mean.2002))/8 )

original.dpd <- merge(original.dpd,svyMean,by="country")
# original.dpd <- transform(original.dpd,pov.adj = (x2010_pl-x2002_pl) * 30.42)
# original.dpd <- transform(original.dpd,pov.adj.mean.2010 = mean.2010 - pov.adj)
# original.dpd <- transform(original.dpd,pov.adj.growth =((pov.adj.mean.2010-mean.2002)/(mean.2002))/8 )

original.dpd$growth.diff <- original.dpd$annual_growth - original.dpd$growth
original.dpd$adj.growth <- original.dpd$growth - original.dpd$growth.diff

india <- subset(interp,country=="India*")
india$diff2010 <- diff(india$x2010_pl)