library(data.table)

setwd("C:/Users/Alex/Desktop/data/PovCal_Increment")
load("pcn.RData")

plgrowth = function(pcn,this.country,base,future,povline=1.9){
  dat <- subset(pcn,country==this.country)
  basehc <- subset(dat,year==base & pl==povline)$hc
  datfuture <- subset(dat,year==future)
  datfuture <- datfuture[order(datfuture$pl),]
  datfuture$absdiff <- abs(datfuture$hc-basehc)
  minabsdiff <- min(datfuture$absdiff)
  futurepl <- subset(datfuture,absdiff==minabsdiff)$pl[1]
  yearsdiff <- future-base
  annualizedgrowth <- ((futurepl-povline)/povline)/yearsdiff
  return(annualizedgrowth)
}

plgrowthrev = function(pcn,this.country,base,future,povline=1.9){
  dat <- subset(pcn,country==this.country)
  futurehc <- subset(dat,year==future & pl==povline)$hc
  datbase <- subset(dat,year==base)
  datbase <- datbase[order(datbase$pl),]
  datbase$absdiff <- abs(datbase$hc-futurehc)
  minabsdiff <- min(datbase$absdiff)
  basepl <- subset(datbase,absdiff==minabsdiff)$pl[1]
  yearsdiff <- future-base
  annualizedgrowth <- ((povline-basepl)/basepl)/yearsdiff
  return(annualizedgrowth)
}

plgrowth(pcn,"China*",2002,2013)
plgrowth(pcn,"China*",1993,2013)
plgrowthrev(pcn,"China*",2002,2013)
plgrowthrev(pcn,"China*",1993,2013)

# countries <- unique(pcn$country)
# dataList <- list()
# dataIndex <- 1
# for(country in countries){
#   message(country)
#   dat <- tryCatch(
#     {
#       data.frame(country,tenyear=plgrowth(pcn,country,2002,2013),twentyyear=plgrowth(pcn,country,1993,2013))
#     },error=function(err){
#       message(err);
#       return(data.frame(country,tenyear=0,twentyyear=0));
#       }
#     )
#   dataList[[dataIndex]] <- dat
#   dataIndex <- dataIndex + 1
# }
# 
# all.growth <- rbindlist(dataList)
# write.csv(all.growth,"C:/Users/Alex/Documents/Data/P20/Meta/pov.adj.growth.csv",na="",row.names=FALSE)

countries <- unique(pcn$country)
dataList <- list()
dataIndex <- 1
for(country in countries){
  message(country)
  dat <- tryCatch(
    {
      data.frame(country,tenyear=plgrowthrev(pcn,country,2002,2013),twentyyear=plgrowthrev(pcn,country,1993,2013))
    },error=function(err){
      message(err);
      return(data.frame(country,tenyear=0,twentyyear=0));
    }
  )
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1
}

all.growth <- rbindlist(dataList)
write.csv(all.growth,"C:/Users/Alex/Documents/Data/P20/Meta/pov.adj.growthrev.csv",na="",row.names=FALSE)

# spl <- smooth.spline(ban2013$pl,y=ban2013$hc)
# pred <- predict(spl)
# 
# # plot(ban2013$pl,ban2013$hc,log="xy")
# # lines(pred,col=2)
# ycs.prime <- diff(ban2013$hc)/diff(ban2013$pl)
# pred.prime <- predict(spl,deriv=1)
# plot(ycs.prime)
# lines(pred.prime$y,col=2)