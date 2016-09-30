library(foreign)
library(data.table)
library(plyr)
library(Hmisc)

source("C:/git/alexm-util/DevInit/R/normalize.R")

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
dat[,c("household_id","ea_id"):=NULL]
dat <- data.frame(dat)

dat$poverty[which(dat$poverty==TRUE)] <- 1
dat$poverty[which(dat$poverty==FALSE)] <- 0

catvars <- c(1,42,54:55,58:65,72)
numvars <- c(2,4,5,7:41,43:53,56,57,66:71,73:82)

nn <- normalize(dat,catvars,numvars)
nn <- nn[c(1:2,4:length(nn),3)]
write.csv(nn,"C:/git/alexm-util/CUDA/LUA/eth_dat.csv",row.names=FALSE)
# fit <- glm(poverty~.,family="binomial",data=nn)
# summary(fit)
# library(pscl)
# pR2(fit)

catvars <- c(1,42,54:55,58:65,72)
numvars <- c(83,2,4,7:41,43:53,56,57,66:71,73:82)

dat$lexp <- log(dat$pcexpppp)

nn.yhat <- normalize(dat,catvars,numvars)
nn.yhat <- nn.yhat[c(2:length(nn.yhat),1)]
nn.yhat$lexp <- round(nn.yhat$lexp*100)
write.csv(nn.yhat,"C:/git/alexm-util/CUDA/LUA/eth_dat_yhat.csv",row.names=FALSE)

pov <- dat$poverty
long <- dat$LON_DD_MOD/180
lat <- (dat$LAT_DD_MOD+90)/(180)
nn.ll <- data.frame(cbind(long,lat,pov))
nn.ll <- nn.ll[complete.cases(nn.ll),]
write.csv(nn.ll,"C:/git/alexm-util/CUDA/LUA/eth_dat_ll.csv",row.names=FALSE)
# fit <- glm(pov~dat$LON_DD_MOD+dat$LAT_DD_MOD+dat$LON_DD_MOD*dat$LAT_DD_MOD,family="binomial")
# summary(fit)
# library(pscl)
# pR2(fit)

