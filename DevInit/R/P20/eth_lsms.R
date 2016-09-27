library(foreign)
library(data.table)
library(plyr)
library(Hmisc)

wd <- "D:/Documents/Data/LSMSauto/ETH_2011_ERSS_v02_M_STATA"

setwd(wd)

dat <- read.dta("cons_agg_w1.dta",convert.factors=FALSE)
var.labs <- data.frame(names(dat),attributes(dat)[7])
# View(var.labs)

ex.rate <- (72.3917/344.707)
pov.line <- 72.3917

dat <- transform(dat,pcexpppp=((total_cons_ann/hh_size)*ex.rate)/12)
dat <- transform(dat,poverty=pcexpppp<pov.line)
keep <- c("household_id","saq01","rural","pw","hh_size","poverty","pcexpppp")
dat <- dat[keep]

geo <- read.dta("Pub_ETH_HouseholdGeovariables_Y1.dta")
geo.labs <- data.frame(names(geo),attributes(geo)[7])
# View(geo.labs)

assets <- read.dta("sect10_hh_w1.dta")
asset.labs <- data.frame(names(assets),attributes(assets)[7])
# View(asset.labs)
# write.csv(asset.labs,"asset.vars.csv")
keep <- c("household_id","hh_s10q00","hh_s10q01")
assets <- assets[keep]
assets.wide <- reshape(assets,idvar="household_id",timevar="hh_s10q00",direction="wide")
names(assets.wide)[3:length(assets.wide)] <- sapply(names(assets.wide)[3:length(assets.wide)],substring,first=11)
names(assets.wide) <- make.names(names(assets.wide),unique=TRUE)

dat <- join(dat,assets.wide,by="household_id")
dat <- join(dat,geo,by="household_id")

dat <- data.table(dat)
dat[,c("household_id","ea_id","LAT_DD_MOD","LON_DD_MOD"):=NULL]
dat <- data.frame(dat)

dat$poverty[which(dat$poverty==TRUE)] <- 1
dat$poverty[which(dat$poverty==FALSE)] <- 0

for(i in 1:length(dat)){
  column = dat[[i]]
  if(is.factor(column)){
    dat[[i]] <- as.integer(column)
  }
}

dat <- dat[c(5,6,3,1,2,4,7:length(dat))]

# write.csv(dat,"eth_dat.csv",row.names=FALSE,na="")
fit <- glm(poverty~.,data=dat,family="binomial")
summary(fit)
# install.packages("pscl")
library("pscl")
pR2(fit)

keep <- c(5:45,1,3,2)
nn <- dat[keep]
nn <- nn[complete.cases(nn),]

rural <- nn$rural
hh_size <- (nn$hh_size)/max(nn$hh_size)
pov <- nn$poverty
logicals <- nn[c(3:37)] >= 1
logicals <- logicals*1

normalized.nn <- data.frame(rural,hh_size,logicals,pov)

# write.csv(normalized.nn,"C:/git/alexm-util/CUDA/LUA/eth_dat.csv",row.names=FALSE)

source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

catvars <- c(1,38)
numvars <- c(3:37,39:41)
nn$urban <- NA
nn$urban[which(nn$rural==1)] <- 0
nn$urban[which(nn$rural==0)] <- 1

nn.wealth <- wealth(nn,catvars,numvars,"urban")
plot(log(pcexpppp)~wealth,data=nn.wealth)

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

povcalcut <- weighted.mean(nn$poverty,nn$pw)
povperc <- weighted.percentile(nn.wealth$wealth,nn.wealth$pw,prob=povcalcut)

nn.wealth$p20 <- (nn.wealth$wealth < povperc[1])

nn.wealth$accuracy <- nn.wealth$poverty==nn.wealth$p20
mean(nn.wealth$accuracy)

fit.normal <- lm(pov~.,data=normalized.nn)
summary(fit.normal)
yhat <- round(predict(fit.normal,normalized.nn))
normalized.nn$yhat <- yhat

normalized.nn$lm.accuracy <- normalized.nn$pov == normalized.nn$yhat
mean(normalized.nn$lm.accuracy)
