library(data.table)
library(Hmisc)
library(ggplot2)
####Data####
dacPath <- "C:/git/alexm-util/DevInit/R/GHA/dac.csv"
dac <- read.csv(dacPath,header=T,as.is=T)

#Set the working directory
wd <- "D:/CRS/"
setwd(wd)

#Define the datasets we want to work with
datasets <- c("CRS 2015 data.csv"
              ,"CRS 2014 data.csv"
              ,"CRS 2013 data.csv"
              ,"CRS 2012 data.csv"
              ,"CRS 2011 data.csv"
              ,"CRS 2010 data.csv"
)
#Define the purposecodes we want to filter by
purposes <- c(
  11182
  ,12182
  ,23182
  ,31182
  ,31282
  ,31382
  ,32182
  ,41082
  ,43082
)

#Define the flownames we want to filter by
flownames <- c("ODA Grants","ODA Grant-Like", "ODA Loans", "Equity Investment")

#Set up empty variables
dataList <- list()
dataIndex <- 1

#Iterate through the datasets
for(i in 1:length(datasets)){
  dataset <- datasets[i]
  message(dataset)
  #Read it in
  dat <- read.csv(dataset,stringsAsFactors=FALSE,encoding='latin1',check.names=FALSE)
  names(dat) <- tolower(make.names(gsub("\"","",names(dat))))
  #Keep only those rows which purposecode == purposes
  # dat <- subset(dat,purposecode %in% purposes & flowname %in% flownames & donorname %in% dac$donorname)
  dat <- subset(dat,flowname %in% flownames & donorname %in% dac$donorname)
  dat$research <- dat$purposecode %in% purposes
  #Keep only those rows which have usd_disbursement_defl
  dat <- dat[complete.cases(dat$usd_disbursement_defl),]
  #Append to our blank variables
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1
}

#Create a dataframe out of our new variables
dat <- rbindlist(dataList,fill=TRUE)
setwd("D:/Documents/Data/MNM")
# write.csv(dat,"filtered_CRS.csv",na="",row.names=FALSE)
dat.tab <- dat[,.(total_disbursement_defl=sum(usd_disbursement_defl)),by=.(year,donorname,research)]
write.csv(dat.tab,"research_percent.csv",na="",row.names=FALSE)
