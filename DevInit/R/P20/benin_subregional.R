library(foreign)
library(data.table)
library(Hmisc)

setwd("C:/Users/Alex/Documents/Data/Benin_Sub")

# hh2012 <- read.dta("BJHR61dt/BJHR61FL.dta",convert.factors=FALSE)
# hh2001 <- read.dta("BJHR41DT/BJHR41FL.dta",convert.factors=FALSE)
# wi2001 <- read.dta("BJWI41DT/BJWI41FL.dta")
# 
# save(hh2012,hh2001,wi2001,file="raw_data.RData")
load("raw_data.RData")

hh2012_labs <- data.frame(varname=names(hh2012),varlab=attributes(hh2012)[7])
hh2001_labs <- data.frame(varname=names(hh2001),varlab=attributes(hh2001)[7])

setnames(wi2001,"whhid","hhid")
setnames(wi2001,"wlthindf","wealth")
hh2001 <- merge(hh2001,wi2001,by="hhid",all.x=TRUE)

hh2012$region2012 <- hh2012$hv024
# > attributes(hh2012)$label.table$HV024
# Alibori    Atacora Atlantique     Borgou   Collines     Couffo      Donga 
# 1          2          3          4          5          6          7 
# Littoral       Mono      Ouémé    Plateau        Zou 
# 8              9         10       11             12 
####################################################
# > attributes(hh2001)$label.table$hv024
# atacora/donga atlantique     borgou/alibori       
# 1             2              3          
# mono/couffo      oueme/plateau        zou/collines 
# 4                5                    6 

hh2001$region2001 <- hh2001$hv024
hh2012$region2001 <- NA
hh2012$region2001[which(hh2012$region2012 %in% c(2,7))] <- 1
hh2012$region2001[which(hh2012$region2012 %in% c(3))] <- 2
hh2012$region2001[which(hh2012$region2012 %in% c(4,1))] <- 3
hh2012$region2001[which(hh2012$region2012 %in% c(9,6))] <- 4
hh2012$region2001[which(hh2012$region2012 %in% c(10,11))] <- 5
hh2012$region2001[which(hh2012$region2012 %in% c(12,5))] <- 6

pov2001 <- 0.5361
pov2012 <- 0.5147

#Rename vars
hh2012$wealth <- hh2012$hv271/100000
hh2012$weights <- hh2012$hv005/1000000
hh2001$weights <- hh2001$hv005/1000000

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

povperc2001 <- weighted.percentile(hh2001$wealth,hh2001$weights,prob=pov2001)
povperc2012 <- weighted.percentile(hh2012$wealth,hh2012$weights,prob=pov2012)

hh2001$ext <- (hh2001$wealth < povperc2001)
hh2012$ext <- (hh2012$wealth < povperc2012)

regiontab2001 <- data.table(hh2001)[,.(poverty2001=weighted.mean(ext,weights)),by=.(region2001)]
regiontab2012 <- data.table(hh2012)[,.(poverty2012=weighted.mean(ext,weights)),by=.(region2001)]

regiontab <- merge(regiontab2001,regiontab2012,by="region2001")
regiontab$name <- c("Atacora/Donga","Atlantique","Borgou/Alibori"
                    ,"Mono/Couffo","Oueme/Plateau","Zou/Collines"
                    )
regiontab$povdiff <- regiontab$poverty2012-regiontab$poverty2001
write.csv(regiontab,"regiontab.csv",row.names=FALSE,na="")

