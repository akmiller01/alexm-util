library(reshape)
library(reshape2)

wd <- "C:/git/alexm-util/DevInit/GNR/2017"
setwd(wd)

raw <- read.csv("aggregate_data_raw.csv",check.names=FALSE,na.strings="")

# names(raw) <- gsub(" ","_",names(raw),fixed=TRUE)

regions <- c(
  "Global"
  ,"Australia and New Zealand"
  ,"Caribbean"
  ,"Central America"
  ,"Central Asia"
  ,"Eastern Africa"
  ,"Eastern Asia"
  ,"Eastern Europe"
  ,"Melanesia"
  ,"Micronesia"
  ,"Middle Africa"
  ,"Northern Africa"
  ,"Northern America"
  ,"Northern Europe"
  ,"Polynesia"
  ,"South America"
  ,"South-Eastern Asia"
  ,"Southern Africa"
  ,"Southern Asia"
  ,"Southern Europe"
  ,"Western Africa"
  ,"Western Asia"
  ,"Western Europe"
  ,"Africa"
  ,"Asia"
  ,"Europe"
  ,"LAC"
  ,"N.America"
  ,"Oceania"
)

long <- reshape(raw
                ,direction="long"
                ,idvar=c("indicator","year")
                ,varying=c(3:60)
                ,sep="."
                ,v.names=c("n","value")
                ,times=regions
                ,timevar="region")

write.csv(long,"agg_long.csv",row.names=FALSE,na="")
