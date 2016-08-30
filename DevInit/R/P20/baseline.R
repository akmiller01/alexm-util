library(WDI)
library(data.table)

setwd("D:/Documents/Data/P20 baseline")

# indicators <- c(
#   "SH.STA.STNT.ZS"
#   ,"SP.REG.BRTH.ZS"
#   ,"SP.REG.DTHS.ZS"
#   ,"NY.GDP.PCAP.PP.KD"
#   ,"NY.GDP.MKTP.PP.KD"
#   ,"NY.GNP.PCAP.PP.KD"
#   ,"NY.GNP.MKTP.PP.KD"
#   ,"NY.GDP.PCAP.KD.ZG"
#   )
# 
# names(indicators) <- c(
#   "stunting"
#   ,"birth.registration"
#   ,"death.registration"
#   ,"gdp.pc.ppp.constant.2011"
#   ,"gdp.ppp.constant.2011"
#   ,"gni.pc.ppp.constant.2011"
#   ,"gnp.ppp.constant.2011"
#   ,"gdp.pc.growth"
#   )
# 
# dat <- WDI(country = "all", 
#            indicator = indicators, 
#            start = 1960, 
#            end = 2016,
#            extra = TRUE
# )
# 
# save(dat,file="wdi.RData")
load("wdi.RData")

#Stunting target is 40% global decrease
keep <- c("country","iso3c","year","SH.STA.STNT.ZS")
stunting <- dat[keep]
stunting <- stunting[order(stunting$iso3c,stunting$year),]
stunting <- stunting[complete.cases(stunting),]
stunting <- data.table(stunting)
stunting$latest <- rev(!duplicated(stunting[,list(rev(stunting$country),rev(stunting$iso3c))]))
penult.stunting <- subset(stunting,!latest)
penult.stunting$latest <- rev(!duplicated(penult.stunting[,list(rev(penult.stunting$country),rev(penult.stunting$iso3c))]))
penult.stunting <- subset(penult.stunting,latest)
setnames(penult.stunting,"latest","penult")
stunting <- subset(stunting,latest)
stunting.rate <- merge(
  stunting
  ,penult.stunting
  ,by=c("country","iso3c")
  ,suffixes = c(".latest", ".penult")
  )
stunting.rate$rate = (stunting.rate$SH.STA.STNT.ZS.latest-stunting.rate$SH.STA.STNT.ZS.penult)
stunting.rate$annualized.rate = stunting.rate$rate/(stunting.rate$year.latest-stunting.rate$year.penult)
keep <- c("iso3c","year.penult","annualized.rate")
stunting.rate <- data.frame(stunting.rate)
stunting.rate <- stunting.rate[keep]
stunting$years.to.target <- 2030-stunting$year
stunting$target <- 0.6*stunting$SH.STA.STNT.ZS
stunting$necessary.reduction <- stunting$SH.STA.STNT.ZS-stunting$target
stunting$annual.reduction <- stunting$necessary.reduction/stunting$years.to.target
stunting <- merge(
  stunting
  ,stunting.rate
  ,by="iso3c"
  ,all.x=TRUE
  )
write.csv(stunting,"stunting-rates.csv",na="",row.names=FALSE)

#CRVS target is 100%
keep <- c("country","iso3c","year","SP.REG.BRTH.ZS")
crvs <- dat[keep]
crvs <- crvs[order(crvs$iso3c,crvs$year),]
crvs <- crvs[complete.cases(crvs),]
crvs <- data.table(crvs)
crvs$latest <- rev(!duplicated(crvs[,list(rev(crvs$country),rev(crvs$iso3c))]))
penult.crvs <- subset(crvs,!latest)
penult.crvs$latest <- rev(!duplicated(penult.crvs[,list(rev(penult.crvs$country),rev(penult.crvs$iso3c))]))
penult.crvs <- subset(penult.crvs,latest)
setnames(penult.crvs,"latest","penult")
crvs <- subset(crvs,latest)
crvs.rate <- merge(
  crvs
  ,penult.crvs
  ,by=c("country","iso3c")
  ,suffixes = c(".latest", ".penult")
)
crvs.rate$rate = (crvs.rate$SP.REG.BRTH.ZS.latest-crvs.rate$SP.REG.BRTH.ZS.penult)
crvs.rate$annualized.rate = crvs.rate$rate/(crvs.rate$year.latest-crvs.rate$year.penult)
keep <- c("iso3c","year.penult","annualized.rate")
crvs.rate <- data.frame(crvs.rate)
crvs.rate <- crvs.rate[keep]
crvs$years.to.target <- 2030-crvs$year
crvs$target <- 100
crvs$necessary.increase <- crvs$target-crvs$SP.REG.BRTH.ZS
crvs$annual.increase <- crvs$necessary.increase/crvs$years.to.target
crvs <- merge(
  crvs
  ,crvs.rate
  ,by="iso3c"
  ,all.x=TRUE
)
write.csv(crvs,"crvs-rates.csv",na="",row.names=FALSE)

#Income target is at least 7% GDP growth per annum in least developed
keep <- c("country","iso3c","year","NY.GDP.PCAP.KD.ZG")
gdp.growth <- dat[keep]
gdp.growth <- gdp.growth[order(gdp.growth$iso3c,gdp.growth$year),]
gdp.growth <- gdp.growth[complete.cases(gdp.growth),]
gdp.growth <- data.table(gdp.growth)
gdp.growth$latest <- rev(!duplicated(gdp.growth[,list(rev(gdp.growth$country),rev(gdp.growth$iso3c))]))
penult.gdp.growth <- subset(gdp.growth,!latest)
penult.gdp.growth$latest <- rev(!duplicated(penult.gdp.growth[,list(rev(penult.gdp.growth$country),rev(penult.gdp.growth$iso3c))]))
penult.gdp.growth <- subset(penult.gdp.growth,latest)
setnames(penult.gdp.growth,"latest","penult")
gdp.growth <- subset(gdp.growth,latest)
gdp.growth.rate <- merge(
  gdp.growth
  ,penult.gdp.growth
  ,by=c("country","iso3c")
  ,suffixes = c(".latest", ".penult")
)
gdp.growth.rate$rate = (gdp.growth.rate$NY.GDP.PCAP.KD.ZG.latest-gdp.growth.rate$NY.GDP.PCAP.KD.ZG.penult)
gdp.growth.rate$annualized.rate = gdp.growth.rate$rate/(gdp.growth.rate$year.latest-gdp.growth.rate$year.penult)
keep <- c("iso3c","year.penult","annualized.rate")
gdp.growth.rate <- data.frame(gdp.growth.rate)
gdp.growth.rate <- gdp.growth.rate[keep]
gdp.growth$years.to.target <- 2030-gdp.growth$year
gdp.growth$target <- 7
gdp.growth$necessary.increase <- gdp.growth$target-gdp.growth$NY.GDP.PCAP.KD.ZG
gdp.growth$annual.increase <- gdp.growth$necessary.increase/gdp.growth$years.to.target
gdp.growth <- merge(
  gdp.growth
  ,gdp.growth.rate
  ,by="iso3c"
  ,all.x=TRUE
)
write.csv(gdp.growth,"gdp.growth-rates.csv",na="",row.names=FALSE)

write.csv(dat,"baseline_wdi.csv",na="",row.names=FALSE)
