library(foreign)
library(Hmisc)
source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

wd <- "D:/Documents/Data/DHSauto/aohr51dt"
setwd(wd)

dat <- read.dta("AOHR51FL.dta")
dat.labs <- data.frame(names(dat),attributes(dat)[7])
# View(dat.labs)

catvars <- c(41,44:52,60,62)
### This is missing, how did they use it?
dat$memsleep <- dat$hv009/dat$hv216
###Let's just use sqrt household members
dat$memsleep <- sqrt(dat$hv009)
numvars <- c("memsleep")
dat$urban <- (dat$hv025=="urban")*1

dat.w <- wealth(dat,catvars,numvars,"urban")
dat.w$c.div3 <- dat.w$c.wealth/3
dat.w$div3 <- dat.w$wealth/3
plot(hv271/100000~div3,data=dat.w)
plot(hv271/100000~c.div3,data=dat.w)
