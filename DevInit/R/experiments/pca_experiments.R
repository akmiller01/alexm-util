library(foreign)
library(ggplot2)
library(reshape2)
library(data.table)
library(Hmisc)
library(reshape)
source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

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

wd <- "C:/Users/Alex/Documents/Data/Nigeria13/"
setwd(wd)

riga.income <- read.dta("Nigeria13_HH_INCOME_V12.dta")
riga.income.labs <- data.frame(names(riga.income),attributes(riga.income)[7])

riga.admin <- read.dta("Nigeria13_HH_ADMIN_V12.dta")
riga.admin.labs <- data.frame(names(riga.admin),attributes(riga.admin)[7])
weights <- riga.admin[c("hh","weight","pcexp")]

riga.income <- merge(riga.income,weights,by="hh")

riga.char <- read.dta("Nigeria13_HHCHAR_V12.dta")
riga.char.labs <- data.frame(names(riga.char),attributes(riga.char)[7])
wealth.dat <- riga.char[c("hh","agwealth","wealth")]

riga.income <- merge(riga.income,wealth.dat,by="hh")

urban <- riga.admin[c("hh","urban")]
setnames(urban,"urban","urban.rural")
urban$urban <- NA
urban$urban[which(urban$urban.rural=="urban")] <- 1
urban$urban[which(urban$urban.rural=="rural")] <- 0
urban$urban.rural <- NULL

riga.char <- merge(riga.char,urban,by="hh")

catvars <- c(
  "landless"
  ,"runwater"
  ,"nondirtfloor"
  ,"brickwalls"
  ,"toilet"
  ,"electricity"
  ,"telephone"
  ,"cellphone"
  ,"cementfloor"
  ,"ownhome"
)

numvars <- c(
  79:88
)

riga.pca1 <- wealth(riga.char,catvars,numvars,urbanvar="urban",pcaorder=1)
pca.wealth <- riga.pca1[c("hh","wealth")]
setnames(pca.wealth,"wealth","pca.wealth1")
riga.income <- merge(riga.income,pca.wealth,by="hh")
cor(riga.income$wealth,riga.income$pca.wealth1,use="pairwise.complete.obs")
cor(riga.income$pcexp,riga.income$pca.wealth1,use="pairwise.complete.obs")
cor(riga.income$totincome1,riga.income$pca.wealth1,use="pairwise.complete.obs")
cor(riga.income$totincome2,riga.income$pca.wealth1,use="pairwise.complete.obs")

riga.pca2 <- wealth(riga.char,catvars,numvars,urbanvar="urban",pcaorder=2)
pca.wealth2 <- riga.pca2[c("hh","wealth")]
setnames(pca.wealth2,"wealth","pca.wealth2")
riga.income <- merge(riga.income,pca.wealth2,by="hh")
cor(riga.income$wealth,riga.income$pca.wealth2,use="pairwise.complete.obs")
cor(riga.income$pcexp,riga.income$pca.wealth2,use="pairwise.complete.obs")
cor(riga.income$totincome1,riga.income$pca.wealth2,use="pairwise.complete.obs")
cor(riga.income$totincome2,riga.income$pca.wealth2,use="pairwise.complete.obs")

riga.lm <- merge(riga.income,riga.char,by="hh")
fit <- lm(totincome1~landless+runwater+nondirtfloor+brickwalls+toilet+electricity+telephone+cellphone+
            cementfloor+ownhome+TLU_cattle+TLU_horse+TLU_sheep+TLU_pigs+TLU_chicken+TLU_donkey+
            TLU_camel+TLU_goat+TLU_rabbit,data=riga.lm
            )
summary(fit)
riga.lm$yhat <- predict(fit,riga.lm)
cor(riga.lm$yhat,riga.lm$totincome1,use="pairwise.complete.obs")
