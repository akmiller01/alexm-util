####Function and setup####
library(Hmisc)
library(plyr)
library(data.table)
library(ggplot2)
library(reshape2)

setwd("/media/alex/Windows/Users/Alex/Documents/Data/P20/Meta")
load("total_tab_data.RData")
load("/media/alex/Windows/Users/Alex/Desktop/data/P20_small_wealth_multiples/povcaldata.RData")

countryMeta <- read.csv("~/headcounts.csv",as.is=TRUE)

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

####Run function####
dataList <- list()
dataIndex <- 1

# Loop through every survey
for(ufilename in unique(data.total$filename)){
  message(ufilename)
  pr <- subset(data.total,filename==ufilename)
  iso3 <- countryMeta$iso3[which(countryMeta$filename==ufilename)]
  
  povcalcut <- subset(countryMeta,filename==ufilename)$extreme
  povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
  
  weighted.wealths <- weighted.percentile(pr$wealth,pr$weights,prob=seq(0,1,length=1001))
  weighted.wealths <- weighted.wealths - povcalperc
  
  weighted.wealths <- data.frame(weighted.wealths)
  weighted.wealths$iso3 <- iso3
  weighted.wealths$i = seq(0,1,length=1001)
  dataList[[dataIndex]] <- weighted.wealths
  dataIndex <- dataIndex + 1
}

all.data <- rbindlist(dataList)

####Aggregates
iso3s <- unique(long$iso3)

dataList <- list()
dataIndex <- 1

for(i in 1:length(iso3s)){
  data <- subset(long,iso3==iso3s[i])
  iso3 <- iso3s[i]
  hc <- unique(data$povheadcount)
  y.prime <- diff(data$P)/diff(data$L)  
  y.prime <- quantile(y.prime,probs=seq(0,1,length=1001),na.rm=TRUE)
  y.prime <- y.prime-1.9
  incomes <- data.frame(y.prime)
  names(incomes) <- c("income")
  incomes$iso3 <- iso3
  incomes$hc <- hc
  incomes$i = seq(0,1,length=1001)
  dataList[[dataIndex]] <- incomes
  dataIndex <- dataIndex + 1
}

all.data.pcn <- rbindlist(dataList)
all.data.pcn <- subset(all.data.pcn,hc>0.01)

all.data = merge(all.data,all.data.pcn,by=c("iso3","i"),all=T)
all.data[,c("i","hc"):=NULL]
setnames(all.data,"weighted.wealths","Survey wealth")
setnames(all.data,"income","Income")
all.data.m = melt(all.data,id.vars=c("iso3"))
all.data.m <- subset(all.data.m,iso3!="DJI")

blank <- data.frame(c("Blank1","Blank2","Blank3"),c(NA,NA,NA),c(NA,NA,NA))
names(blank) <- names(all.data.m)
all.plot.alpha <- ggplot(rbind(all.data.m,blank)) +
  geom_density(aes(x=value,color=variable,fill=variable,group=variable),alpha=0.3) +
  xlim(-3,3) +
  facet_wrap(~iso3,ncol=5) +
  theme_bw() +
  labs(x="Adj. wealth",y="Density")

ggsave(filename="~/all.pdf",plot=all.plot.alpha,width=8,height=30,units="in",limitsize=FALSE)
