library(foreign)

setwd("D:/GADM/GADM1 joined with ACLED")
dat <- read.dbf("p20.dbf",as.is=TRUE)

fit <- lm(meanp20~fatal_pp,data=dat)
summary(fit)

dat <- transform(dat,logpop = log(X_sum),logfatal = log(sumFATALIT))
dat <- subset(dat,is.finite(logfatal))

fit <- lm(meanp20~logfatal+logpop,data=dat)
summary(fit)

ug <- subset(dat,NAME_0=="Uganda")
fit <- lm(meanp20~log(fatal_pp),data=ug)
plot(meanp20~log(fatal_pp),data=ug)
summary(fit)
