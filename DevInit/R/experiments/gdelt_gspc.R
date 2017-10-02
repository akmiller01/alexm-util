library(data.table)
library(readr)
library(reshape2)
library(Hmisc)
library(DataCombine)

setwd("C:/Users/Alex/Documents/Data/US data/2005")

colnames <- read.csv("CSV.header.dailyupdates.txt", header = FALSE,
                     sep="\t",colClasses="character")

gdelt <- read_delim("2005.csv","\t",col_names=unlist(colnames[1,]))

gdelt.tab <- data.table(gdelt)[,.(
  mentions = sum(NumMentions,na.rm=TRUE)
  ,avgTone=mean(AvgTone,na.rm=TRUE)
),by=.(SQLDATE,EventRootCode)]

mgdelt.tab <- melt(gdelt.tab,id.vars=c("SQLDATE","EventRootCode"),measure.vars=c("mentions","avgTone"))
wgdelt.tab <- dcast(mgdelt.tab,formula=SQLDATE~variable+EventRootCode)
wgdelt.tab[is.na(wgdelt.tab)] <- 0
wgdelt.tab$mentions <- rowSums(wgdelt.tab[,2:21])
wgdelt.tab$avgTone <- rowMeans(wgdelt.tab[,22:41])
wgdelt.tab[,2:21] <- wgdelt.tab[,2:21] / wgdelt.tab$mentions

wgdelt.tab$Date <- parse_date(wgdelt.tab$SQLDATE,format="%Y%m%d")
wgdelt.tab$SQLDATE <- NULL

gspc <- read_csv("GSPC.csv")

dat <- merge(gspc,wgdelt.tab,by="Date",all.x=TRUE)
dat <- slide(dat,Var="Open",slideBy=20)
dat <- transform(dat,pctChange = (Open20-Open)/Open)

fit <- lm(pctChange~avgTone+mentions,data=dat)
summary(fit)

###Cameo codes
# 1: Make Public Statement
# 2: Appeal
# 3: Express intent to cooperate
# 4: Consult
# 5: Engage in diplomatic cooperation
# 6: Engage in material cooperation
# 7: Provide aid
# 8: Yield
# 9: Investigate
# 10: Demand
# 11: Disapprove
# 12: Reject
# 13: Threaten
# 14: Protest
# 15: Exhibit military posture
# 16: Reduce relations
# 17: Coerce
# 18: Assault
# 19: Fight
# 20: Engage in unconventional mass violence

fit <- lm(pctChange~mentions_01+
            mentions_02+
            mentions_03+
            mentions_04+
            mentions_05+
            mentions_06+
            mentions_07+
            mentions_08+
            mentions_09+
            mentions_10+
            mentions_11+
            mentions_12+
            mentions_13+
            mentions_14+
            mentions_15+
            mentions_16+
            mentions_17+
            mentions_18+
            mentions_19+
            mentions_20+
            avgTone_01+
            avgTone_02+
            avgTone_03+
            avgTone_04+
            avgTone_05+
            avgTone_06+
            avgTone_07+
            avgTone_08+
            avgTone_09+
            avgTone_10+
            avgTone_11+
            avgTone_12+
            avgTone_13+
            avgTone_14+
            avgTone_15+
            avgTone_16+
            avgTone_17+
            avgTone_18+
            avgTone_19+
            avgTone_20
          ,data=dat)
summary(fit)
#Adj R2: 0.3067
#Sig Cameos:
# . 02 Appeal -1.647e-02
# . 06 Material Cooperation -7.600e-03
# . 15 Exhibit military posture 4.121e-03
# . 17 Coerce 1.329e-02
# ** 04 Consult 2.596e-02
# *** 08 Yield 2.215e-02
dat$yhat <- predict(fit,dat)
plot(yhat~pctChange,data=dat)
dat$directionCorrect = sign(dat$yhat)==sign(dat$pctChange)
mean(dat$directionCorrect,na.rm=TRUE)
dat$confidence <- round(abs(dat$yhat),3)
acc.tab <- data.table(dat)[,.(acc=mean(directionCorrect,na.rm=TRUE)),by="confidence"]
plot(acc.tab)
