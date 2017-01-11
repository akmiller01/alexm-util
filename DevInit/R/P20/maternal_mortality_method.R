library(foreign)
library(Hmisc)
library(data.table)

setwd("D:/Documents/Data/DHSauto/ugir60dt")
data <- read.dta("UGIR60FL.dta")
# setwd("D:/Documents/Data/DHSauto/tzbr7hdt")
# data <- read.dta("TZBR7HFL.dta")
labs <- data.frame(names(data),attributes(data)[7])

data$survey.date <- data$v008
data$wealth <- data$v191/100000
data$weights <- data$v005/1000000

demo.vars <- c("survey.date","wealth","weights")
mm.vars <- names(data)[which(substr(names(data),1,2)=="mm" & substr(names(data),1,3)!="mmc")]

data.long <- data[c(demo.vars,mm.vars)]
varying.vars <- names(data.long)[which(grepl("_",names(data.long)))]
data.long <- reshape(data.long,direction="long",varying=varying.vars,sep="_")

sisters <- subset(data.long,mm1 %in% c(2,"female"))

####Numerator
sisters$pod <- sisters$survey.date-sisters$mm8
sisters$age.at.death <- sisters$mm8-sisters$mm4
sisters$aad.groups <- floor(sisters$age.at.death/60)

####Denominator (period 0-6 years, 0-84 months)
surv.denom <- subset(sisters, is.na(age.at.death))
dead.denom <- subset(sisters, !is.na(age.at.death))

#Highest age group, surviving
surv.denom$ha <- (surv.denom$survey.date - 1)  - surv.denom$mm4
surv.denom$ha.group <- floor(surv.denom$ha/60)
surv.denom$hag.exposure.months <- surv.denom$ha - (surv.denom$ha.group*60 + 1)
surv.denom$hag.exposure.months <- pmax(surv.denom$hag.exposure.months,0)

#Middle age group, surviving
surv.denom$ma <- (surv.denom$survey.date - 61)  - surv.denom$mm4
surv.denom$ma.group <- floor(surv.denom$ma/60)
surv.denom$mag.exposure.months <- surv.denom$ma - (surv.denom$ma.group*60 + 1)
surv.denom$mag.exposure.months <- pmax(surv.denom$mag.exposure.months,0)

#Lowest age group, surviving
surv.denom$la <- (surv.denom$survey.date - 121)  - surv.denom$mm4
surv.denom$la.group <- floor(surv.denom$la/60)
surv.denom$lag.exposure.months <- 84 - (surv.denom$hag.exposure.months+surv.denom$mag.exposure.months)
surv.denom$lag.exposure.months <- pmax(surv.denom$lag.exposure.months,0)

#Highest age group, dead
dead.denom$ha <- (dead.denom$mm8)  - dead.denom$mm4
dead.denom$ha.group <- floor(dead.denom$ha/60)
dead.denom$hag.exposure.months <- dead.denom$ha - (dead.denom$ha.group*60 + 1)
dead.denom$hag.exposure.months <- pmax(dead.denom$hag.exposure.months,0)
dead.denom$hag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 60))] <- 0

#Middle age group, dead
dead.denom$ma <- (dead.denom$mm8-61)  - dead.denom$mm4
dead.denom$ma.group <- floor(dead.denom$ma/60)
dead.denom$mag.exposure.months <- dead.denom$ma - (dead.denom$ma.group*60 + 1)
dead.denom$mag.exposure.months <- pmax(dead.denom$mag.exposure.months,0)
dead.denom$mag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 120))] <- 0

#Lowest age group, dead
dead.denom$la <- (dead.denom$mm8-121) - dead.denom$mm4
dead.denom$la.group <- floor(dead.denom$la/60)
dead.denom$lag.exposure.months <- dead.denom$la - (dead.denom$la.group*60 + 1)
dead.denom$lag.exposure.months <- pmax(dead.denom$lag.exposure.months,0)
dead.denom$lag.exposure.months[which(dead.denom$mm8<(dead.denom$survey.date - 84))] <- 0

denominator <- rbind(surv.denom,dead.denom)
