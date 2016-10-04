library(Hmisc)
library(data.table)

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
