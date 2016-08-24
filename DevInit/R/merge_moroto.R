library(data.table)
library(varhandle)

wd <- "D:/Documents/Data/Moroto budget/"
setwd(wd)

proposed.e <- read.csv("2015/Expenditure.csv")
setnames(proposed.e,"Year","year")
proposed.r <- read.csv("2015/Revenue.csv")
actual.e <- read.csv("2017/Expenditure.csv")
setnames(actual.e,"Year","year")
actual.r <- read.csv("2017/Revenue.csv")

proposed.e <- subset(proposed.e,year=="2014/15 Proposed Budget")
proposed.e$year <- 2015
proposed.r <- subset(proposed.r,year=="2014/15 Proposed Budget")
proposed.r$year <- 2015
actual.e <- subset(actual.e,year=="2015/16 Approved Budget")
actual.e$year <- 2016
actual.r <- subset(actual.r,year=="2015/16 Approved Budget")
actual.r$year <- 2016

moroto.exp <- rbind(proposed.e,actual.e)
moroto.exp$id <- "d308"
moroto.exp["budget-type"] <- "budget"
moroto.exp$l1 <- "expenditure"
splitWorkplan <- function(x,sep=": "){
  if(is.factor(x)){
    x <- unfactor(x)
  }
  split <- strsplit(x,sep,fixed=TRUE)[[1]][2]
  return(split)
}
moroto.exp$l2 <- sapply(moroto.exp$Workplan,splitWorkplan)
setnames(moroto.exp,"Budget.Type","l3")
moroto.exp <- subset(moroto.exp,grepl("expend",l3,ignore.case=TRUE))
setnames(moroto.exp,"Revenue.Source","l4")
moroto.exp$value <- as.double(gsub(",","",moroto.exp$Value))
keep <- c("id","year","budget-type","l1","l2","l3","l4","value")
moroto.exp <- moroto.exp[keep]

moroto.rev <- rbind(proposed.r,actual.r)
moroto.rev$id <- "d308"
moroto.rev["budget-type"] <- "budget"
moroto.rev$l1 <- "revenue"
moroto.rev$l2 <- sapply(moroto.rev$Revenue.Type,splitWorkplan,sep=". ")
setnames(moroto.rev,"Revenue.Source","l3")
moroto.rev$l4 <- NA
moroto.rev$value <- as.double(gsub(",","",moroto.rev$Value))
moroto.rev <- moroto.rev[keep]

moroto <- rbind(moroto.rev,moroto.exp)

moroto <- transform(moroto,l2=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l2))))
moroto <- transform(moroto,l3=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l3))))
moroto <- transform(moroto,l4=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l4))))
moroto <- transform(moroto,l2.m=gsub(" ","",tolower(gsub("[^[:alnum:] ]", "", l2))))
moroto <- transform(moroto,l3.m=gsub(" ","",tolower(gsub("[^[:alnum:] ]", "", l3))))
moroto <- transform(moroto,l4.m=gsub(" ","",tolower(gsub("[^[:alnum:] ]", "", l4))))

uf <- read.csv("C:/git/digital-platform/country-year/uganda-finance.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
uf <- subset(uf,id=="d308" & (year==2015 | year==2016))
uf <- unique(uf)
uf <- transform(uf,l2.m=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l2))))
uf <- transform(uf,l3.m=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l3))))
uf <- transform(uf,l4.m=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l4))))
uf <- merge(uf,moroto,by=c("id","budget.type","year","l1","l2.m","l3.m","l4.m"),all=TRUE)

uf$l2 <- NA
uf$l3 <- NA
uf$l4 <- NA
uf$value <- NA

for(i in 1:nrow(uf)){
  row <- uf[i,]
  l2.x <- row[[8]]
  l3.x <- row[[9]]
  l4.x <- row[[10]]
  value.x <- row[[11]]
  l2.y <- row[[13]]
  l3.y <- row[[14]]
  l4.y <- row[[15]]
  value.y <- row[[16]]
  value <- sum(value.x,value.y,na.rm=TRUE)
  if(is.na(value.x) & is.na(value.y)){
    value <- NA
    l2 <- l2.x
    l3 <- l3.x
    l4 <- l4.x
  } else if(!is.na(value.x) & is.na(value.y)){
    l2 <- l2.x
    l3 <- l3.x
    l4 <- l4.x
  } else if(is.na(value.x) & !is.na(value.y)){
    l2 <- l2.y
    l3 <- l3.y
    l4 <- l4.y
  } else if(!is.na(value.x) & !is.na(value.y)){
    l2 <- l2.x
    l3 <- l3.x
    l4 <- l4.x
  }
  uf[i,]$value <- value
  uf[i,]$l2 <- l2
  uf[i,]$l3 <- l3
  uf[i,]$l4 <- l4
}
keep <- c("id","budget.type","year","l1","l2","l3","l4","value")
uf <- uf[keep]

write.csv(uf,"first_draft.csv")
