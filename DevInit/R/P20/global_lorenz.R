library(data.table)

wd <- "C:/git/alexm-util/DevInit/PovCalNet/"
setwd(wd)
# 
# dat <- read.csv("data.csv",as.is=TRUE,header=FALSE)
# names(dat) <- c("iso3","type","ppp","year","pop")
# 
# dat <- dat[order(dat$iso3,dat$year),]
# dat <- data.table(dat)
# dat$latest <- rev(!duplicated(dat[,list(rev(dat$iso3))]))
# dat <- subset(dat,latest)
# 
# dat <- data.frame(dat)
# dat$latest <- NULL
# 
# write.csv(dat,"data_new.csv",row.names=FALSE)

dat <- read.csv("D:/Documents/Data/lorenz.csv",header=FALSE)
keep <- c("V1","V2","V3","V4","V7")
dat <- dat[keep]
names(dat) <- c("iso3","type","year","l","headcount")
dat <- dat[order(dat$iso3,dat$l),]
dat$headcount[which(dat$headcount>1)] <- dat$headcount[which(dat$headcount>1)]/100
dat$year <- round(dat$year)

pop <- read.csv("D:/Documents/Data/P20 baseline/undesa.pop.csv")
cc <- read.csv("D:/Documents/Data/P20 baseline/country-codes.csv")
cc <- cc[c("ISO3166.1.Alpha.3","ISO3166.1.numeric")]
names(cc) <- c("iso3","LocID")
pop <- subset(pop,Variant=="Medium" & Sex=="Both")
pop <- merge(pop,cc,by="LocID")
setnames(pop,"Time","year")
pop <- data.table(pop)
pop <- pop[,.(pop=sum(Value,na.rm=TRUE)*1000),by=.(year,iso3)]

dat <- merge(
  dat
  ,pop
  ,by=c("iso3","year")
)
dat <- dat[order(dat$iso3,dat$l),]

dat$p <- dat$pop*dat$headcount
write.csv(dat,"lorenz.csv",row.names=FALSE)

dat <- data.table(dat)

lorenz <- dat[,.(p=sum(p,na.rm=TRUE)),by=.(l)]
lorenz <- lorenz[order(lorenz$l),]
plot(l~p,data=lorenz)

lorenz$logl <- log(lorenz$l)

fit <- lm(logl~p,data=lorenz)
summary(fit)
plot(logl~p,data=lorenz)
abline(fit)

logl <- NA
p <- 7200000000
new.data <- data.frame(logl,p)
exp(predict(fit,new.data))
yhat <- predict(fit,new.data)
new.data$logl <- yhat

plot(logl~p,data=rbind(lorenz,new.data,fill=TRUE))
abline(fit)

logl <- NA
p <- 7200000000*.2
new.data <- data.frame(logl,p)
exp(predict(fit,new.data))
