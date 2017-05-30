weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

library(Hmisc)
us <- read.LIS("us13h") 
nrow(us) 
us <- transform(us,daily.total.income = (hi/365))
us <- transform(us,daily.income = ((hi-hit)/365))
us <- transform(us,daily.transfers = (hit/365))
us <- transform(us,daily.disposable = ((hi-hxit)/365))
describe(us$daily.total.income)
describe(us$daily.income)
describe(us$daily.transfers)
describe(us$daily.disposable)
total.centiles = weighted.percentile(us$daily.total.income,us$hwgt,prob=seq(0,1,0.01))
centiles = weighted.percentile(us$daily.income,us$hwgt,prob=seq(0,1,0.01))
transfer.centiles = weighted.percentile(us$daily.transfers,us$hwgt,prob=seq(0,1,0.01))
disposable.centiles = weighted.percentile(us$daily.disposable,us$hwgt,prob=seq(0,1,0.01))

print(total.centiles)
print(centiles)
print(transfer.centiles)
print(disposable.centiles)

negative <- subset(us,daily.income<0)
positive <- subset(us,daily.income>=0)
negWt <- sum(negative$hwgt,na.rm=TRUE)
posWt <- sum(positive$hwgt,na.rm=TRUE)
print(nrow(negative))
print(negWt)
print(nrow(positive))
print(posWt)
print(negWt/(negWt+posWt))

negative <- subset(us,daily.total.income<0)
positive <- subset(us,daily.total.income>=0)
negWt <- sum(negative$hwgt,na.rm=TRUE)
posWt <- sum(positive$hwgt,na.rm=TRUE)
print(nrow(negative))
print(negWt)
print(nrow(positive))
print(posWt)
print(negWt/(negWt+posWt))