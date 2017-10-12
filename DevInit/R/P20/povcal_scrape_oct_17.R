library(Hmisc)
library(data.table)
library(readr)

wd <- "/Users/Alex/Documents/Data/PovCal_Increment"
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
  ,"gini"
  ,"median"
  ,"mld"
  ,"pop"
  ,"svyYear"
  ,"detail"
)

agg <- read_csv("regional.csv",col_names=agg.header)

agg <- subset(agg,!grepl("--",country,fixed=TRUE))
agg <- data.table(agg)
agg.dup <- agg[,.(count=sum(!is.na(type))),by=.(country,year,pl)]
agg.dup <- subset(agg.dup,count>1)
agg.dup$id <- paste(agg.dup$country,agg.dup$year,agg.dup$pl)
agg$id <- paste(agg$country,agg$year,agg$pl)
all.dups <- agg[which(agg$id %in% agg.dup$id),]
good.dups <- subset(all.dups,svyYear=="Interpolated")
agg <- agg[!(agg$id %in% agg.dup$id),]
agg <- rbind(agg,good.dups)
agg[,("id"):=NULL]


agg <- agg[order(-agg$year,agg$pl,agg$country),]

pcn <- agg
save(pcn,file="pcn.RData")
# load("pcn.RData")

years <- unique(pcn$year)
for(this.year in years){
  filename <- paste0("years/pcn",this.year,".csv")
  message(filename)
  write.csv(subset(pcn,year==this.year),filename,na="",row.names=FALSE)
}