povcal <- function(iso3,year=2013,urban="all"){
  library(curl)
  yearDict <- list(
    "1990"=1990
    ,"1991"=1990
    ,"1992"=1993
    ,"1993"=1993
    ,"1994"=1993
    ,"1995"=1996
    ,"1996"=1996
    ,"1997"=1996
    ,"1998"=1999
    ,"1999"=1999
    ,"2000"=1999
    ,"2001"=2002
    ,"2002"=2002
    ,"2003"=2002
    ,"2004"=2005
    ,"2005"=2005
    ,"2006"=2005
    ,"2007"=2008
    ,"2008"=2008
    ,"2009"=2008
    ,"2010"=2012
    ,"2011"=2011
    ,"2012"=2012
    ,"2013"=2013
    ,"2014"=2013
    ,"2015"=2013
    ,"2016"=2013
    ,"2017"=2013
  )
  urbanDict = list(
    "rural"=1,
    "r"=1,
    "1"=1,
    "urban"=2,
    "u"=2,
    "2"=2,
    "all"=3,
    "a"=3,
    "3"=3
  )
  cuts <- read.csv("D:/Documents/Data/DHS map/cuts.full.csv",na.strings="",as.is=TRUE)
  subcuts <- subset(cuts,DHSYEAR==yearDict[as.character(year)])
  data <- c()
  steps <- names(cuts)[2:6]
  for(step in steps){
    cut <- subcuts[step][1,1]
    url <- paste0(
      "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="
      ,iso3
      ,"_"
      ,urbanDict[as.character(urban)]
      ,"&PPP0=0&PL0="
      ,cut
      ,"&Y0="
      ,yearDict[as.character(year)]
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
    data <- c(data,datum)
  }
  names(data) <- steps
  return(data)
}