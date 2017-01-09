# install.packages("R.matlab")
# install.packages("ggmap")
library(ggmap)
library(R.matlab)
library(data.table)

wd <- "D:/Documents/StreetView/"
setwd(wd)

# gps <- data.frame(readMat("GPS_Long_Lat_Compass.mat")[[1]])
# setnames(gps,"X1","lat")
# setnames(gps,"X2","lon")
# setnames(gps,"X3","bearing")
# write.csv(gps,"GPS.csv",row.names=FALSE)
# 
# gps.sample.indexes <- sample(c(1:nrow(gps)),2000)
# gps.sample <- gps[gps.sample.indexes,]
# 
# coords <- list()
# for(i in 1:nrow(gps.sample)){
#   coords[[i]] <- c(gps.sample$lon[i],gps.sample$lat[i])
# }
# 
# gps.sample$address <- sapply(coords,revgeocode)
# write.csv(gps.sample,"GPS_addresses.csv",row.names=FALSE)
# save(gps.sample,file="geocoded.RData")
load("geocoded.RData")

citystatezips <- c()
streets <- c()
for(i in 1:nrow(gps.sample)){
  address = gps.sample$address[i]
  address.split <- strsplit(address,",")[[1]]
  street = address.split[1]
  if(length(address.split)==4){
    citystatezip = paste(address.split[2],address.split[3],sep=",")
  }else{
    citystatezip = paste(address.split[3],address.split[4],sep=",")
  }
  citystatezips <- c(citystatezips,trimws(citystatezip))
  streets <- c(streets,street)
}

gps.sample$citystatezip <- citystatezips
gps.sample$street <- streets

source("C:/git/housing-analysis/api_plugin.R")
library(jsonlite)

resultList <- list()
listIndex <- 1

for(i in 1:nrow(gps.sample)){
  result <- GetDeepSearchResults(gps.sample$street[i],gps.sample$citystatezip[i])
  
  name <- c(rownames(gps.sample)[i])
  
  bed <- c(as.double(result$response$results$result$bedrooms))
  bath <- c(as.double(result$response$results$result$bathrooms))
  finishedSqFt <- c(as.double(result$response$results$result$finishedSqFt))
  lotSizeSqFt <- c(as.double(result$response$results$result$lotSizeSqFt))
  yearBuilt <- c(as.double(result$response$results$result$yearBuilt))
  useCode <- c(result$response$results$result$useCode)
  
  if(length(bed)==0){
    bed <- c(0)
  }
  if(length(bath)==0){
    bath <- c(0)
  }
  if(length(finishedSqFt)==0){
    finishedSqFt <- c(0)
  }
  if(length(lotSizeSqFt)==0){
    lotSizeSqFt <- c(0)
  }
  if(length(yearBuilt)==0){
    yearBuilt <- c(NA)
  }
  if(length(useCode)==0){
    useCode <- c(NA)
  }
  
  zpid <- result$response$results$result$zpid
  detail_url <- result$response$results$result$links$homedetails
  
  if(!is.null(detail_url)){
    message(i)
    con <- curl(detail_url)
    open(con)
    detail_body <- readLines(con)
    closeAllConnections()
    
    detail_line <- detail_body[sapply(detail_body,grepl,pattern="&h=")]
    h.pos <- regexpr("&h=", detail_line)
    p3d.pos <- regexpr("%3D",substr(detail_line,h.pos,nchar(detail_line)))
    key <- substr(detail_line,h.pos+3,h.pos+p3d.pos+1)
    
    pp_url <- paste0("http://ppz.zillowstatic.com/hdp_chart/render.json?v=2&h="
                     ,key
                     ,"&zpid="
                     ,zpid
                     ,"&m=1&t=tenYears&jsonp=YUI.Env.JSONP.paparazzi"
                     ,zpid
                     ,"&size=standard&showForecast=true&signedIn=true")
    
    con <- curl(pp_url)
    open(con)
    pp_json <- readLines(con)[2]
    closeAllConnections()
    
    start.pos <- regexpr("paparazziData",pp_json,fixed=TRUE)+16
    end.pos <- regexpr(", \"version\":",pp_json,fixed=TRUE)-1
    data_json <- substr(pp_json,start.pos,end.pos)
    
    data <- fromJSON(data_json,flatten=TRUE)
    home.prices <- subset(data,regionType=="Home")$data
    neighborhood.prices <- subset(data,regionType=="Neighborhood")$data
    city.prices <-  subset(data,regionType=="City")$data
    
    if(length(home.prices)>0){
      regionType="Home"
      prices = home.prices
    }else if(length(neighborhood.prices)>0){
      regionType="Neighborhood"
      prices = neighborhood.prices
    }else if(length(city.prices)>0){
      regionType="City"
      prices = city.prices
    }else{
      next
    }
    home <- data.frame(name
                       ,regionType
                       ,prices
                       ,result$response$results$result$address
                       ,bed
                       ,bath
                       ,finishedSqFt
                       ,lotSizeSqFt
                       ,useCode
    )
    home$date <- as.Date(home$xValue/100000000,origin=as.Date("1975/01/01"))
    home$price <- home$yValue
    home$yValue <- NULL
    home$xValue <- NULL
    home$x <- NULL
    home$y <- NULL
    resultList[[listIndex]] <- home
    listIndex <- listIndex + 1
  } 
}

wd <- "D:/Documents/StreetView/"
setwd(wd)

coded.data <- rbindlist(resultList,fill=TRUE)
save(coded.data,file="coded_data.RData")

june <- subset(coded.data,date>as.Date("2015-06-18") & date<as.Date("2015-06-19"))

june <- unique(june)
june <- subset(june,regionType!="City" & city=="New York")
june$above.median <- june$price>=quantile(june$price,.5)
write.csv(june,"median_categorical.csv",row.names=FALSE)

for(i in 1:nrow(june)){
  base_url <- "http://www.cs.ucf.edu/~aroshan/index_files/Dataset_PitOrlManh/images/"
  name <- june$name[i]
  category <- june$above.median[i]
  base_url <- paste0(base_url,sprintf("%06d", name))
  if(category){
    download.file(paste0(base_url,"_1.jpg"),destfile=paste0("images/2/",sprintf("%06d", name),"_1.jpg"),method="wget")
    download.file(paste0(base_url,"_2.jpg"),destfile=paste0("images/2/",sprintf("%06d", name),"_2.jpg"),method="wget")
    download.file(paste0(base_url,"_3.jpg"),destfile=paste0("images/2/",sprintf("%06d", name),"_3.jpg"),method="wget")
    download.file(paste0(base_url,"_4.jpg"),destfile=paste0("images/2/",sprintf("%06d", name),"_4.jpg"),method="wget")
  }else{
    download.file(paste0(base_url,"_1.jpg"),destfile=paste0("images/1/",sprintf("%06d", name),"_1.jpg"),method="wget")
    download.file(paste0(base_url,"_2.jpg"),destfile=paste0("images/1/",sprintf("%06d", name),"_2.jpg"),method="wget")
    download.file(paste0(base_url,"_3.jpg"),destfile=paste0("images/1/",sprintf("%06d", name),"_3.jpg"),method="wget")
    download.file(paste0(base_url,"_4.jpg"),destfile=paste0("images/1/",sprintf("%06d", name),"_4.jpg"),method="wget")
  }
}
