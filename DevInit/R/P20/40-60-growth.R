library(data.table)
library(reshape2)
library(zoo)

setwd("C:/Users/Alex/Desktop/data/PovCal_Increment")
# load("pcn.RData")
# 
# dat <- data.table(pcn)[,.(pl=max(pl)),by=.(country,year,hc)]
# dat <- subset(dat,hc>0)
# dat$key <- paste(dat$country,dat$year,dat$hc,sep=".")
# expand <- expand.grid(unique(dat$country),c(1981:2013),seq(0.01,100,0.01))
# names(expand) <- c("country","year","hc")
# expand$key <- paste(expand$country,expand$year,expand$hc,sep=".")
# expand$pl <- dat$pl[match(expand$key,dat$key)]
# save(expand,file="expanded.RData")
load("expanded.RData")

expand$key <- NULL

mdat <- melt(expand,id.vars=c("country","hc","year"))
wdat <- dcast(mdat,country+hc~year+variable,value.var="value")
names(wdat)[3:ncol(wdat)] <- paste0("x",names(wdat)[3:ncol(wdat)])

approx_if_poss = function(x,rule=2){
  if(sum(!is.na(x))>0){
    return(na.approx(x,rule=rule))
  }else{
    return(x)
  }
}

interp <- data.table(wdat)[,.(
  hc = hc
  ,x1981_pl=approx_if_poss(x1981_pl,rule=2)
  ,x1982_pl=approx_if_poss(x1982_pl,rule=2)
  ,x1983_pl=approx_if_poss(x1983_pl,rule=2)
  ,x1984_pl=approx_if_poss(x1984_pl,rule=2)
  ,x1985_pl=approx_if_poss(x1985_pl,rule=2)
  ,x1986_pl=approx_if_poss(x1986_pl,rule=2)
  ,x1987_pl=approx_if_poss(x1987_pl,rule=2)
  ,x1988_pl=approx_if_poss(x1988_pl,rule=2)
  ,x1989_pl=approx_if_poss(x1989_pl,rule=2)
  ,x1990_pl=approx_if_poss(x1990_pl,rule=2)
  ,x1991_pl=approx_if_poss(x1991_pl,rule=2)
  ,x1992_pl=approx_if_poss(x1992_pl,rule=2)
  ,x1993_pl=approx_if_poss(x1993_pl,rule=2)
  ,x1994_pl=approx_if_poss(x1994_pl,rule=2)
  ,x1995_pl=approx_if_poss(x1995_pl,rule=2)
  ,x1996_pl=approx_if_poss(x1996_pl,rule=2)
  ,x1997_pl=approx_if_poss(x1997_pl,rule=2)
  ,x1998_pl=approx_if_poss(x1998_pl,rule=2)
  ,x1999_pl=approx_if_poss(x1999_pl,rule=2)
  ,x2000_pl=approx_if_poss(x2000_pl,rule=2)
  ,x2001_pl=approx_if_poss(x2001_pl,rule=2)
  ,x2002_pl=approx_if_poss(x2002_pl,rule=2)
  ,x2003_pl=approx_if_poss(x2003_pl,rule=2)
  ,x2004_pl=approx_if_poss(x2004_pl,rule=2)
  ,x2005_pl=approx_if_poss(x2005_pl,rule=2)
  ,x2006_pl=approx_if_poss(x2006_pl,rule=2)
  ,x2007_pl=approx_if_poss(x2007_pl,rule=2)
  ,x2008_pl=approx_if_poss(x2008_pl,rule=2)
  ,x2009_pl=approx_if_poss(x2009_pl,rule=2)
  ,x2010_pl=approx_if_poss(x2010_pl,rule=2)
  ,x2011_pl=approx_if_poss(x2011_pl,rule=2)
  ,x2012_pl=approx_if_poss(x2012_pl,rule=2)
  ,x2013_pl=approx_if_poss(x2013_pl,rule=2)
)
,by=.(country)
]

interp <- interp[seq(100,1530000,100),]
interp <- data.frame(interp)
colApprox = function(row,rule=2){
  return(approx_if_poss(unlist(row),rule=rule))
}
interp[,3:ncol(interp)] <- t(apply(interp[,3:ncol(interp)],1,colApprox))

growthDat <- interp[,1:2]

yearStart = 1981 - 4
for(col in 4:ncol(interp)){
  year1 <- yearStart + col
  year2 <- yearStart + col + 1
  growthDat[paste0("growth",year1,"to",year2)] <- (interp[,col]-interp[,col-1])/interp[,col-1]
}

growthDat$avg1981to2013 <- rowMeans(growthDat[,3:34])
growthDat$avg1983to2013 <- rowMeans(growthDat[,5:34])
growthDat$avg1993to2013 <- rowMeans(growthDat[,15:34])
setwd("~/Data/P20/Meta/")
write.csv(growthDat,"growthdat.csv",na="",row.names=FALSE)
