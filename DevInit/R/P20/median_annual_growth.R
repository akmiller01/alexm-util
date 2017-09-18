library(data.table)
library(reshape2)
library(zoo)

setwd("C:/Users/Alex/Desktop/data/PovCal_Increment")
load("pcn.RData")

dat <- data.table(pcn)[,.(pl=max(pl)),by=.(country,year,hc)]
dat <- subset(dat,hc>0)
mdat <- melt(dat,id.vars=c("country","hc","year"))
wdat <- dcast(mdat,country+hc~year+variable,value.var="value")
names(wdat)[3:32] <- paste0("x",names(wdat)[3:32])

wdat <- subset(wdat,!is.na(x2013_pl) & !is.na(x1999_pl))
keep <- c("country","hc","x1999_pl","x2013_pl")
wdat <- wdat[keep]
ctyTab <- data.table(wdat)[,.(count=sum(!is.na(x1999_pl))),by="country"]
hcTab <- data.table(wdat)[,.(count=sum(!is.na(country))),by="hc"]

expand <- expand.grid(unique(wdat$country),seq(0.01,100,0.01))
names(expand) <- c("country","hc")
expand <- merge(expand,wdat,by=c("country","hc"),all=TRUE)
interp <- data.table(expand)[,.(
  hc = hc
  ,x1999_pl=na.approx(x1999_pl,rule=2)
  ,x2013_pl=na.approx(x2013_pl,rule=2)
  )
  ,by=.(country)
]

interp = transform(interp,annual_growth= ((x2013_pl-x1999_pl)/x1999_pl)/14 )

interpTab <- data.table(interp)[,.(
  mean_annual_growth = mean(annual_growth)
  ,median_annual_growth = median(annual_growth)
  ,percentile_05_annual_growth = quantile(annual_growth,0.05)
  ,percentile_15_annual_growth = quantile(annual_growth,0.15)
  ,percentile_25_annual_growth = quantile(annual_growth,0.25)
  ,percentile_75_annual_growth = quantile(annual_growth,0.75)
  ,percentile_85_annual_growth = quantile(annual_growth,0.85)
  ,percentile_95_annual_growth = quantile(annual_growth,0.95)
),
by=c("country")]
plot(annual_growth~hc,data=subset(interp,country=="South Sudan"))
plot(hc~x1999_pl,data=subset(interp,country=="Uganda"))

setwd("C:/Users/Alex/Documents/Data/P20/Meta/")
write.csv(interpTab,"povcal_interpTab.csv",na="",row.names=FALSE)
write.csv(interp,"povcal_interp.csv",na="",row.names=FALSE)
