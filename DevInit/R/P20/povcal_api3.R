povcal.p20 <- function(iso3){
  library(curl)
  cut <- 2.38
  url <- paste0(
    "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
    ,iso3
    ,"_3"
    ,"&PPP0=0&PL0="
    ,cut
    ,"&Y0=2012"
    ,"&NumOfCountries=1"
  )
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  closeAllConnections()
  grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
  if(length(grepResults)>0){
    datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
    if(datum>1){
      datum <- datum/100
    }
  }else{
    datum <- NA
  }
  return(datum)
}
povcal.int <- function(iso3){
  library(curl)
  cut <- 1.90
  url <- paste0(
    "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
    ,iso3
    ,"_3"
    ,"&PPP0=0&PL0="
    ,cut
    ,"&Y0=2012"
    ,"&NumOfCountries=1"
  )
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  closeAllConnections()
  grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
  if(length(grepResults)>0){
    datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
    if(datum>1){
      datum <- datum/100
    }
  }else{
    datum <- NA
  }
  return(datum)
}
povcal.mmd <- function(iso3){
  library(curl)
  cut <- 0.99
  url <- paste0(
    "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
    ,iso3
    ,"_3"
    ,"&PPP0=0&PL0="
    ,cut
    ,"&Y0=2012"
    ,"&NumOfCountries=1"
  )
  con <- curl(url)
  open(con)
  text <- readLines(curl(url))
  closeAllConnections()
  grepResults <- grep("Headcount(HC): ",text,fixed=TRUE)
  if(length(grepResults)>0){
    datum <- as.numeric(trimws(strsplit(text[grepResults[length(grepResults)]],":")[[1]][2]))
    if(datum>1){
      datum <- datum/100
    }
  }else{
    datum <- NA
  }
  return(datum)
}