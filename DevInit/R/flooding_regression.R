setwd("C:/Users/Alex/Documents/Data/flooding")

dat <- read.csv("p20_by_flooding_region.csv")

lc <- read.csv("flooding_w_lc.csv")

lc$lc <- factor(lc$lc.majorit)



fit <- lm(P10_bh_10~meanp20+pop.sum+pop.mean,data=dat)
summary(fit)

lc$logflood = log(lc$P10_bh_10)

fit <- lm(logflood~lc,data=subset(lc,is.finite(logflood)))
summary(fit)

lc <- lc[c("unit_id","lc")]

dat <- merge(dat,lc)

dat$logflood = log(dat$P10_bh_10)
dat$logpop = log(dat$pop.sum)
dat$p20pop = dat$meanp20*dat$pop.sum
dat$logp20pop = log(dat$p20pop)
dat$floodpc <- dat$P10_bh_10/dat$pop.sum

fit <- lm(floodpc~lc,data=dat)
summary(fit)

fit <- lm(floodpc~lc+meanp20,data=dat)
summary(fit)

fit <- lm(meanp20~lc,data=dat)
summary(fit)

fit <- lm(logflood~meanp20+logpop+pop.mean,data=dat[is.finite(dat$logflood),])
summary(fit)

fit <- lm(logflood~logp20pop+pop.mean,data=dat[is.finite(dat$logflood) & is.finite(dat$logp20pop),])
summary(fit)

#Interaction
fit <- lm(logflood~meanp20+logpop+logp20pop+pop.mean,data=dat[is.finite(dat$logflood) & is.finite(dat$logp20pop),])
summary(fit)

dat$logfutureflood = log(dat$P30_24_10)

fit <- lm(P30_24_10~meanp20+pop.sum+pop.mean,data=dat)
summary(fit)

fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)

#Interaction
fit <- lm(logfutureflood~meanp20+logpop+logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)


###Testing different models
dat$logfutureflood = log(dat$P30_28_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)
dat$logfutureflood = log(dat$P30_38_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)
dat$logfutureflood = log(dat$P30_b4_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)
dat$logfutureflood = log(dat$P30_b8_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)
dat$logfutureflood = log(dat$P30_2h_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)
dat$logfutureflood = log(dat$P30_3h_10)
fit <- lm(logfutureflood~logp20pop+pop.mean,data=dat[is.finite(dat$logfutureflood) & is.finite(dat$logp20pop),])
summary(fit)