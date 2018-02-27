####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(ggplot2)

setwd("C:/Users/Alex/Documents/Data/P20/Meta")
load("total_tab_data.RData")

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)
mpi <- read.csv("C:/Users/Alex/Desktop/data/P20_small_wealth_multiples/mpi.csv",as.is=TRUE)

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
dataList.urbrur <- list()
dataList.gender <- list()
dataIndex <- 1

# Loop through every survey
for(ufilename in unique(data.total$filename)){
  message(ufilename)
  pr <- subset(data.total,filename==ufilename)
  iso3 <- countryMeta$iso3[which(countryMeta$filename==ufilename)]
  
  povcalcut <- subset(countryMeta,filename==ufilename)$hc
  povcalperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
  
  weighted.wealths <- weighted.percentile(pr$wealth,pr$weights,prob=seq(0,1,length=1001))
  weighted.wealths <- weighted.wealths - povcalperc
  
  urb <- subset(pr,urban==TRUE)
  weighted.wealths.urban <- weighted.percentile(urb$wealth,urb$weights,prob=seq(0,1,length=1001))
  weighted.wealths.urban <- data.frame(weighted.wealths.urban)
  weighted.wealths.urban$urban <- TRUE
  names(weighted.wealths.urban) <- c("wealth","urban")
  rur <- subset(pr,urban==FALSE)
  weighted.wealths.rural <- weighted.percentile(rur$wealth,rur$weights,prob=seq(0,1,length=1001))
  weighted.wealths.rural <- data.frame(weighted.wealths.rural)
  weighted.wealths.rural$urban <- FALSE
  names(weighted.wealths.rural) <- c("wealth","urban")
  weighted.wealths.ur <- rbindlist(list(weighted.wealths.urban,weighted.wealths.rural))
  weighted.wealths.ur$wealth <- weighted.wealths.ur$wealth - povcalperc
  
  men <- subset(pr,sex=="Male")
  weighted.wealths.male <- weighted.percentile(men$wealth,men$weights,prob=seq(0,1,length=1001))
  weighted.wealths.male <- data.frame(weighted.wealths.male)
  weighted.wealths.male$sex <- "Male"
  names(weighted.wealths.male) <- c("wealth","sex")
  women <- subset(pr,sex=="Female")
  weighted.wealths.female <- weighted.percentile(women$wealth,women$weights,prob=seq(0,1,length=1001))
  weighted.wealths.female <- data.frame(weighted.wealths.female)
  weighted.wealths.female$sex <- "Female"
  names(weighted.wealths.female) <- c("wealth","sex")
  weighted.wealths.gender <- rbindlist(list(weighted.wealths.male,weighted.wealths.female))
  weighted.wealths.gender$wealth <- weighted.wealths.gender$wealth - povcalperc
  
  pic.file <- paste0("C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/individual/",iso3,".jpg")
  pic.file.r <- paste0("C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/individual/",iso3,"_urb_rur.jpg")
  pic.file.g <- paste0("C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/individual/",iso3,"_gender.jpg")
  r <- ggplot(weighted.wealths.ur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3)
  g <- ggplot(weighted.wealths.gender,aes(x=wealth)) + geom_density(aes(group=sex,colour=sex,fill=sex),alpha=0.3)
  d <- ggplot(data.frame(weighted.wealths),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3)
  
  this.mpi <- subset(mpi,iso==iso3)$hc
  
  if(length(this.mpi)>0){
    vline.cut <- weighted.wealths[(this.mpi*10)+1]
    d <- d + geom_vline(xintercept=vline.cut)
    g <- g + geom_vline(xintercept=vline.cut)
    r <- r + geom_vline(xintercept=vline.cut)
  }else{
    vline.cut <- NA
  }
  
  r <- r + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
  ggsave(filename=pic.file.r,plot=r,height=5,width=8,units="in")
  g <- g + theme_bw() + theme(legend.title=element_blank()) + labs(title=iso3,x="Adj. wealth",y="Density")
  ggsave(filename=pic.file.g,plot=g,height=5,width=8,units="in")
  d <- d + theme_bw() + theme(legend.position="none") + labs(title=iso3,x="Adj. wealth",y="Density")
  ggsave(filename=pic.file,plot=d,height=5,width=8,units="in")
  
  weighted.wealths <- data.frame(weighted.wealths)
  weighted.wealths$iso3 <- iso3
  weighted.wealths$vline.cut <- vline.cut
  weighted.wealths.ur <- data.frame(weighted.wealths.ur)
  weighted.wealths.ur$iso3 <- iso3
  weighted.wealths.ur$vline.cut <- vline.cut
  weighted.wealths.gender <- data.frame(weighted.wealths.gender)
  weighted.wealths.gender$iso3 <- iso3
  weighted.wealths.gender$vline.cut <- vline.cut
  dataList[[dataIndex]] <- weighted.wealths
  dataList.urbrur[[dataIndex]] <- weighted.wealths.ur
  dataList.gender[[dataIndex]] <- weighted.wealths.gender
  dataIndex <- dataIndex + 1
}

####Aggregates

all.data <- rbindlist(dataList)
all.data <- subset(all.data,iso3!="DJI")
blank <- data.frame(c(NA,NA,NA),c("Blank1","Blank2","Blank3"),c(NA,NA,NA))
names(blank) <- names(all.data)
all.plot.alpha <- ggplot(rbind(all.data,blank),aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. wealth",y="Density")
hcs <- countryMeta[c("iso3","hc")]
all.data <- join(all.data,hcs,by="iso3")
all.data <- all.data[order(-all.data$hc),]
blank <- data.frame(c(NA,NA,NA),c("Blank1","Blank2","Blank3"),c(NA,NA,NA),c(1,1,1))
names(blank) <- names(all.data)
all.data <- rbind(all.data,blank)
all.data$iso3 <- factor(all.data$iso3,levels=unique(all.data$iso3))
all.plot <- ggplot(all.data,aes(x=weighted.wealths)) + geom_density(aes(fill=1),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.position="none") + labs(x="Adj. wealth",y="Density")

ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.pdf",plot=all.plot,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.alpha.pdf",plot=all.plot.alpha,width=8,height=30,units="in",limitsize=FALSE)


all.data.urbrur <- rbindlist(dataList.urbrur)
blank <- data.frame(c(NA,NA),c("",""),c("",""),c(NA,NA))
names(blank) <- names(all.data.urbrur)
all.urb.rur.alpha <- ggplot(rbind(all.data.urbrur,blank),aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")
all.data.urbrur <- join(all.data.urbrur,hcs,by="iso3")
all.data.urbrur <- all.data.urbrur[order(-all.data.urbrur$hc),]
blank <- data.frame(c(NA,NA),c("",""),c("",""),c(NA,NA),c(0,0))
names(blank) <- names(all.data.urbrur)
all.data.urbrur <- rbind(all.data.urbrur,blank)
all.data.urbrur$iso3 <- factor(all.data.urbrur$iso3,levels=unique(all.data.urbrur$iso3))
all.urb.rur <- ggplot(all.data.urbrur,aes(x=wealth)) + geom_density(aes(group=urban,colour=urban,fill=urban),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")

ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.urb.rur.pdf",plot=all.urb.rur,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.urb.rur.alpha.pdf",plot=all.urb.rur.alpha,width=8,height=30,units="in",limitsize=FALSE)

all.data.gender <- rbindlist(dataList.gender)
blank <- data.frame(c(NA,NA),c("",""),c("",""),c(NA,NA))
names(blank) <- names(all.data.gender)
all.gender.alpha <- ggplot(rbind(all.data.gender,blank),aes(x=wealth)) + geom_density(aes(group=sex,colour=sex,fill=sex),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")
all.data.gender <- join(all.data.gender,hcs,by="iso3")
all.data.gender <- all.data.gender[order(-all.data.gender$hc),]
blank <- data.frame(c(NA,NA),c("",""),c("",""),c(NA,NA),c(0,0))
names(blank) <- names(all.data.gender)
all.data.gender <- rbind(all.data.gender,blank)
all.data.gender$iso3 <- factor(all.data.gender$iso3,levels=unique(all.data.gender$iso3))
all.gender <- ggplot(all.data.gender,aes(x=wealth)) + geom_density(aes(group=sex,colour=sex,fill=sex),alpha=0.3) + geom_vline(aes(xintercept=vline.cut)) + xlim(-3,3) + facet_wrap(~iso3,ncol=5) + theme_bw() + theme(legend.title=element_blank()) + labs(x="Adj. wealth",y="Density")

ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.gender.pdf",plot=all.gender,width=8,height=30,units="in",limitsize=FALSE)
ggsave(filename="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/aggregate/all.gender.alpha.pdf",plot=all.gender.alpha,width=8,height=30,units="in",limitsize=FALSE)

save(all.data,file="C:/Users/Alex/Documents/Data/P20_small_wealth_multiples/wealth_dist_all.RData")
