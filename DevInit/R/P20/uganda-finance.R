library(data.table)
library(varhandle)
library(plyr)

wd <- "C:/git/alexm-util/DevInit/Uganda/Spotlight"
setwd(wd)

e1 <- read.csv("Expenditure-12-13.csv")
e2 <- read.csv("Expenditure-12-13-missing.csv")
e3 <- read.csv("Expenditure-13-14.csv")
e4 <- read.csv("Expenditure-13-14-missing.csv")
e5 <- read.csv("Expenditure-14-15.csv")
e6 <- read.csv("Expenditure-14-15-missing.csv")
e7 <- read.csv("Expenditure-15-16.csv")
e8 <- read.csv("Expenditure-15-16-missing.csv")
e9 <- read.csv("Nakasongola_2013-14.csv")
e10 <- read.csv("Tororo_MC_2013-14.csv")

revenue <- read.csv("Revenue-12-16.csv")
revenue <- unique(revenue)

expenditure <- rbindlist(list(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10))
# revenue <- subset(expenditure,Revenue.Expenditure=="Revenue")
expenditure <- subset(expenditure,Revenue.Expenditure=="Expenditure")
expenditure <- data.frame(unique(expenditure))

setnames(expenditure,"Year","year")
expenditure["budget-type"] <- "budget"
setnames(expenditure,"Revenue.Expenditure","l1")
splitWorkplan <- function(x,sep=": "){
  if(is.factor(x)){
    x <- unfactor(x)
  }
  split <- strsplit(x,sep,fixed=TRUE)[[1]][2]
  return(split)
}
expenditure$l2 <- sapply(expenditure$Workplan,splitWorkplan)
setnames(expenditure,"Budget.Type","l3")
expenditure <- subset(expenditure,grepl("expend",l3,ignore.case=TRUE))
setnames(expenditure,"Revenue.Source","l4")
expenditure$value <- as.double(gsub(",","",expenditure$Value))
keep <- c("District","year","budget-type","l1","l2","l3","l4","value")
expenditure <- expenditure[keep]

revenue["budget-type"] <- "budget"
revenue$l1 <- "Revenue"
revenue$l2 <- sapply(revenue$Revenue.Type,splitWorkplan,sep=". ")
setnames(revenue,"Revenue.Source","l3")
revenue$l4 <- NA
revenue$value <- as.double(gsub(",","",revenue$Value))
revenue <- revenue[keep]

finance <- rbind(expenditure,revenue)
finance$value <- finance$value*1000

finance <- transform(finance,l1=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l1))))
finance <- transform(finance,l2=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l2))))
finance <- transform(finance,l3=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l3))))
finance <- transform(finance,l4=gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", l4))))

write.csv(finance,"uganda-finance.csv",row.names=FALSE,na="")
