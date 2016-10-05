library(Hmisc)
library(data.table)
library(zoo)
library(plyr)
library(WDI)

wd <- "D:/Documents/Data/PovCal_Increment"
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
#   ,"pop"
#   ,"svyYear"
#   ,"detail"
# )
# 
# agg <- fread(
#   "aggregates.csv"
#   ,colClasses = c(
#     "character"
#     ,"numeric"
#     ,"character"
#     ,rep("numeric",8)
#     ,"character"
#     )
#   ,header=FALSE
#   ,col.names=agg.header
#   ,na.strings=c("NaN.00","n/a","Interpolated","Weighted sum","Weightedsum","Wght. interpolation")
#   )
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
# ind <- fread(
#   "individual.csv"
#   ,colClasses = c(
#     "character"
#     ,"numeric"
#     ,"character"
#     ,rep("numeric",10)
#     ,"character"
#   )
#   ,header=FALSE
#   ,col.names=ind.header
#   ,na.strings=c("NaN.00","n/a","Interpolated","Weighted sum","Weightedsum","Wght. interpolation")
# )

# save(agg,ind,file="total.RData")

load("total.RData")

agg <- subset(agg,!grepl("--",country,fixed=TRUE))
ind <- subset(ind,!grepl("--",country,fixed=TRUE))

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
  ,svyYear=svyYear
  ,original.hc=hc
  ),by=.(country,pl)]

indicator <- "SP.POP.TOTL"

pop <- WDI(country = "all", 
           indicator = indicator, 
           start = 1981, 
           end = 2013
)

ind.interp <- merge(ind.interp,pop,by=c("country","year"))
ind.interp$pop <- ind.interp$SP.POP.TOTL/1000000
remove <- c("original.hc","iso2c","SP.POP.TOTL")
ind.interp[,(remove):=NULL]
ind.interp$type <- "i"

remove <- c("mean","pg","pg2","watts","detail")
agg[,(remove):=NULL]

pcn <- rbind(agg,ind.interp)
save(pcn,file="pcn.RData")
