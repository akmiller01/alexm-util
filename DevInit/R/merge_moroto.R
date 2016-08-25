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

# proposed.e <- subset(proposed.e,year=="2014/15 Proposed Budget")
# proposed.r <- subset(proposed.r,year=="2014/15 Proposed Budget")
actual.e <- subset(actual.e,year=="2015/16 Approved Budget")
actual.r <- subset(actual.r,year=="2015/16 Approved Budget")

# moroto.exp <- rbind(proposed.e,actual.e)
moroto.exp <- actual.e
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

# moroto.rev <- rbind(proposed.r,actual.r)
moroto.rev <- actual.r
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

moroto$value <- moroto$value*1000

uf <- read.csv("C:/git/alexm-util/DevInit/Uganda/Spotlight/uganda-finance.csv", header = TRUE,sep=",",na.strings="",check.names=FALSE,stringsAsFactors=FALSE)
uf <- subset(uf,grepl("moroto",District,ignore.case=TRUE))

###Testing####
# uf$id <- "d308"
# moroto$District <- "Moroto District (new scrape)"
# dat <- rbind(uf,moroto)
# write.csv(dat,"raw_unmerged.csv")
###End testing####

uf <- subset(uf,year=="2014/15 Proposed Budget" | year=="2015/16 Proposed Budget")
uf$year[which(uf$year=="2015/16 Proposed Budget")] <- "2015/16 Approved Budget"
uf.d <- subset(uf,District=="Moroto District")
uf.d$District <- NULL
uf.mc <- subset(uf,District=="Moroto Municipal Council")
uf.mc$District <- NULL
uf <- merge(uf.d,uf.mc,by=names(uf)[3:length(uf)-1],all=TRUE)
uf$id <- "d308"
uf$value <- NA
for(i in 1:nrow(uf)){
  row <- uf[i,]
  value.x <- row$value.x
  value.y <- row$value.y
  value <- sum(value.x,value.y,na.rm=TRUE)
  if(is.na(value.x) & is.na(value.y)){
    value <- NA
  }
  uf[i,]$value <- value
}
uf$value.x <- NULL
uf$value.y <- NULL
uf <- merge(uf,moroto,by=c("id","budget.type","year","l1","l2","l3","l4"),all=TRUE)
uf$value <- NA
for(i in 1:nrow(uf)){
  row <- uf[i,]
  value.x <- row$value.x
  value.y <- row$value.y
  value <- sum(value.x,value.y,na.rm=TRUE)
  if(is.na(value.x) & is.na(value.y)){
    value <- NA
  }
  uf[i,]$value <- value
}
uf$value.x <- NULL
uf$value.y <- NULL

keep <- c("id","budget.type","year","l1","l2","l3","l4","value")
uf <- uf[keep]

uf <- unique(uf)
uf$year[which(uf$year=="2014/15 Proposed Budget")] <- 2015
uf$year[which(uf$year=="2015/16 Approved Budget")] <- 2016
write.csv(uf,"fourth_draft.csv",row.names=FALSE,na="")
