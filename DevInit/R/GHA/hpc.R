library(httr)
library(jsonlite)
library(data.table)

setwd("C:/git/alexm-util/DevInit/R/GHA")

auth <- readLines("D:/Documents/auth.txt")
years <- c(2000:2016)
dataList <- list()
dataIndex <- 1

for(year in years){
  req <- GET(
    paste0("https://api.hpc.tools/v1/public/fts/flow?year=",year)
    ,authenticate(auth[1],auth[2],type="basic")
  )
  stop_for_status(req)
  dat <- fromJSON(content(req))
  closeAllConnections()
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1
}

dat <- rbindlist(dataList)
save(dat,file="fts.2000.2016.RData")
