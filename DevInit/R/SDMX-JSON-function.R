#Necessary libraries
library(RCurl)
library(rjson)

####OECD SDMX-JSON function####
OECD <- function(url){
  content <- getURL(url, httpheader = list('User-Agent' = 'rsdmx-json/0.0.1'), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  BOM <- "\ufeff"
  if(attr(regexpr(BOM, content), "match.length") != - 1){
    content <- gsub(BOM, "", content)
  }
  rawJson <- fromJSON(content)
  rawData <- rawJson$dataSets[[1]]$series
  rawStructure <- rawJson$structure
  dimensions <- rawStructure$dimensions[[1]]
  attributes <- rawStructure$attributes$series
  datnames <- c(sapply(dimensions, "[[", 2),sapply(attributes, "[[", 1),"value","value2")
  ndim <- length(sapply(dimensions, "[[", 2))
  natt <- length(sapply(attributes, "[[", 1))
  ncol <- ndim+natt
  data <- matrix(ncol=ncol+2,nrow=length(rawData))
  for(i in 1:length(rawData)){
    row <- rawData[i]
    rawDimensions <- names(row)
    splitDimensions <- strsplit(rawDimensions,":")[[1]]
    # Dimensions
    for(j in 1:length(splitDimensions)){
      dimensionReference <- dimensions[[j]]$values
      dimensionIndex <- as.integer(splitDimensions[j])+1
      dimensionValue <- dimensionReference[[dimensionIndex]][[2]]
      data[i,j] <- dimensionValue
    }
    # Attributes
    for(j in 1:length(row[[1]]$attributes)){
      attributeReference <- attributes[[j]]$values
      rawAttIndex <- row[[1]]$attributes[[j]]
      if(is.null(rawAttIndex)){
        attributeValue <- NA
      }else{
        attributeIndex <- as.integer(rawAttIndex+1)
        attributeValue <- attributeReference[[attributeIndex]][[2]]
      }
      data[i,ndim+j] <- attributeValue
    }
    # Observations
    obs = unlist(row[[1]]$observations[[1]])
    data[i,ncol:ncol+2] = obs
  }
  data <- setNames(data.frame(data,stringsAsFactors=FALSE),datnames)
  names(data)[which(names(data)=="Year")] <- "obsTime"
  return(data)
}

####Table 1 Example####
url <- "https://stats.oecd.org/SDMX-JSON/data/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+20+21+6+701+742+22+7+820+8+76+9.1.5+1010+2102.1121+1122.A+D+N/all?startTime=2005&amp;endTime=2014&amp;dimensionAtObservation=allDimensions/"
data <- OECD(url) 