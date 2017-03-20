library(data.table)
library(Hmisc)
####Data####
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)

#Set the working directory
wd <- "D:/git/digital-platform/country-year"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("oda.2015.csv"
              ,"oda.2014.csv"
              ,"oda.2013.csv"
              ,"oda.2012.csv"
              ,"oda.2011.csv"
              ,"oda.2010.csv"
)

#Set up empty variables
dataList <- list()
dataIndex <- 1

#Iterate through the datasets
for(i in 1:length(datasets)){
  dataset <- datasets[i]
  message(dataset)
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE)
  dat <- subset(dat,id.from %in% dac$id)
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1
}
oda <- rbindlist(dataList)