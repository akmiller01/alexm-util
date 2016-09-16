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
keep <- c("household_id","saq01","rural","pw","hh_size","poverty")
dat <- dat[keep]

# geo <- read.dta("Pub_ETH_HouseholdGeovariables_Y1.dta")
# geo.labs <- data.frame(names(geo),attributes(geo)[7])
# View(geo.labs)

pr <- read.dta("sect1_hh_w1.dta")
pr.labs <- data.frame(names(pr),attributes(pr)[7])
names(pr)[which(names(pr)=="hh_s1q00")] <- "person_id"
names(pr)[which(names(pr)=="hh_s1q03")] <- "sex"
names(pr)[which(names(pr)=="hh_s1q04_a")] <- "age"
names(pr)[which(names(pr)=="hh_s1q07")] <- "religion"
names(pr)[which(names(pr)=="hh_s1q08")] <- "marital"
names(pr)[which(names(pr)=="hh_s1q11")] <- "region.born"
names(pr)[which(names(pr)=="hh_s1q14")] <- "father.alive"
names(pr)[which(names(pr)=="hh_s1q15")] <- "father.educ"
names(pr)[which(names(pr)=="hh_s1q20")] <- "father.occu"
names(pr)[which(names(pr)=="hh_s1q18")] <- "mother.alive"
names(pr)[which(names(pr)=="hh_s1q19")] <- "mother.educ"
names(pr)[which(names(pr)=="hh_s1q21")] <- "mother.occu"
keep <- c("household_id","person_id","sex"
          ,"age","religion","marital","region.born","father.alive"
          ,"father.educ","father.occu","mother.alive","mother.educ","mother.occu")
pr <- pr[keep]

assets <- read.dta("sect10_hh_w1.dta")
asset.labs <- data.frame(names(assets),attributes(assets)[7])
# View(asset.labs)
assets.intra <- assets[c("household_id","hh_s10q00","hh_s10q02_a","hh_s10q01","pw")]
names(assets.intra) <- c("household_id","item","person_id","number.owned","weights")
assets.inter <- assets[c("household_id","hh_s10q00","hh_s10q01","pw")]
names(assets.inter) <- c("household_id","item","number.owned","weights")
assets.intra <- reshape(assets.intra,idvar=c("household_id","person_id","weights"),timevar="item",direction="wide")
assets.inter <- reshape(assets.inter,idvar=c("household_id","weights"),timevar="item",direction="wide")
names(assets.intra)[4:length(assets.intra)] <- sapply(names(assets.intra)[4:length(assets.intra)],substring,first=14)
assets.intra <- assets.intra[complete.cases(assets.intra$person_id),]
assets.intra <- data.table(assets.intra)
assets.intra[is.na(assets.intra)] <- 0

logical.intra <- assets.intra > 0
logical.intra <- logical.intra[,c(4:length(assets.intra))]
assets.intra <- data.frame(assets.intra)
assets.intra <- cbind(assets.intra[,c(1:3)],logical.intra)
names(assets.intra) <- make.names(names(assets.intra),unique=TRUE)

# dat <- join(dat,assets.inter,by="household_id")
# dat <- join(dat,geo,by="household_id")

# pr <- join(pr,assets.intra,by=c("household_id","person_id"))
pr <- merge(pr,assets.intra,by=c("household_id","person_id"))
# pr[which(is.na(pr[,c(14:length(pr))])),c(14:length(pr))] <- 0
pr <- join(pr,dat,by="household_id")
pr$urban <- NA
pr$urban[which(pr$rural==1)] <- 0
pr$urban[which(pr$rural==0)] <- 1

source("C:/git/alexm-util/DevInit/R/p20/wealth_pca.R")

catvars <- c(15:49)

intra.wealth <- wealth(pr,catvars,numvars=NULL,"urban")
plot(intra.wealth$wealth[order(intra.wealth$wealth)])

intra.tab <- data.table(intra.wealth)
intra.tab <- intra.tab[,.(mean.wealth=weighted.mean(wealth,weights,na.rm=TRUE)),by=.(sex)]
