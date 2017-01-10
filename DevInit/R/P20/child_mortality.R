library(foreign)

setwd("D:/Documents/Data/DHSauto/ugbr60dt")
data <- read.dta("UGBR60FL.dta")
# setwd("D:/Documents/Data/DHSauto/tzbr7hdt")
# data <- read.dta("TZBR7HFL.dta")

#The trick to getting this to work is expressing age as a time period.
#E.g. someone born 59 months ago would be expressed as -59
#So we subtract the date of the survey from the birth date
data$age.months <- data$b3-data$v008
data$weights <- data$v005/1000000

probs <- c()
#Time-lower and time-upper also need to be expressed as time period, with 0 being the survey
tl = -59
tu = 0
# tl = -119
# tu = -60
# tl = -179
# tu = -120
#Age of death is expressed as a duration... so there's no need for it to be negative. This matches var b7 (age of death in months)
#Here al stands for age-lower and au stands for age-upper
segments <- list(
  list("al"=0,"au"=0)
  ,list("al"=1,"au"=2)
  ,list("al"=3,"au"=5)
  ,list("al"=6,"au"=11)
  ,list("al"=12,"au"=23)
  ,list("al"=24,"au"=35)
  ,list("al"=36,"au"=47)
  ,list("al"=48,"au"=59)
)
for(i in 1:length(segments)){
  segment = segments[[i]]
  al = segment[["al"]]
  au = segment[["au"]]
  cohortA <- subset(data,age.months>=(tl-au) & age.months<(tl-al))
  cohortB <- subset(data,age.months>=(tl-al) & age.months<=(tu-au))
  cohortC <- subset(data,age.months>(tu-au) & age.months<=(tu-al))
  Amortalities <- subset(cohortA, b7>=al & b7<=au)
  Bmortalities <- subset(cohortB, b7>=al & b7<=au)
  Cmortalities <- subset(cohortC, b7>=al & b7<=au)
  if(tu==0){
    mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+nrow(Cmortalities)
  }else{
    mortalities <- nrow(Bmortalities)+0.5*nrow(Amortalities)+0.5*nrow(Cmortalities)
  }

  Asurvivals <- subset(cohortA,is.na(b7) | b7>al)
  Bsurvivals <- subset(cohortB,is.na(b7) | b7>al)
  Csurvivals <- subset(cohortC,is.na(b7) | b7>al)
  if(tu==0){
    survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+nrow(Csurvivals)
  }else{
    survivals <- nrow(Bsurvivals)+0.5*nrow(Asurvivals)+0.5*nrow(Csurvivals)
  }

  prob <- 1-(mortalities/survivals)
  if(is.nan(prob)){
    prob <- 1
  }
  probs <- c(probs,prob)
}
mortality <- (1-prod(probs))
mortality*1000

# cv <- sd(data$weights,na.rm=TRUE)/mean(data$weights,na.rm=TRUE)
# deft <- cv*cv+1
# n <- nrow(data)
# pop.dat <- read.csv("D:/Documents/Data/P20 baseline/undesa.pop.csv",as.is=TRUE,na.strings="")
# pop <- subset(pop.dat,Location=="Uganda" & Time==2011 & Sex=="Both" & AgeGrpStart==0)$Value*1000
# SE <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(mortality*(1-mortality)))
# corrected.SE <- SE*deft
# low.end <- (mortality-(2*corrected.SE))
# low.end <- pmax(low.end,0)
# high.end <- (mortality+(2*corrected.SE))
# high.end <- pmin(high.end,1)
