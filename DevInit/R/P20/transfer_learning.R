setwd("D:/git/models/tutorials/image/imagenet")

dat <- read.csv("analysis.csv")

fit <- glm(category~word,data=dat,family="binomial")
summary(fit)

fit <- glm(category~word,data=subset(dat,confidence>.9),family="binomial")
summary(fit)
