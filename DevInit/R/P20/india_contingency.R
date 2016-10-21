####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(WDI)
library(varhandle)
require(zoo)

# indicator <- "SI.POV.NAHC"
# 
# pov <- WDI(country = "all", 
#            indicator = indicator, 
#            start = 1990, 
#            end = 2016,
#            extra = TRUE
#            #cache = new_cache
# )
# 
# pov <- pov[c("iso3c","year","SI.POV.NAHC")]
# names(pov) <- c("iso3","year","hc")
# pov$hc <- pov$hc/100
# pov <- pov[order(pov$year),]
# pov <- pov[order(pov$iso3),]
# colname <- "hc"
# pov <- ddply(pov,.(iso3),function(x)
# {
#   naLen <- nrow(x[which(is.na(x[,colname])),])
#   allLen <- nrow(x)
#   valueLen <- allLen-naLen
#   ival <- x[,colname]
#   x[,paste("original",colname,sep="-")] <- ival 
#   if(valueLen>=2)
#   {
#     interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
#   }
#   else if(valueLen==1){
#     interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
#   }
#   else{
#     interpVals <- rep(NA,allLen)
#   }
#   x[,colname] <- interpVals
#   return(x)
# }
# )
# names(pov) <- c("iso3","year","pl.hc","pl.hc.original")
# 
# pov <- subset(pov,iso3=="IND")

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

wd <- "C:/git/alexm-util/DevInit/P20-vis/India Contingency"
setwd(wd)

pop <- read.csv("D:/Documents/Data/DHS shapefiles/IA 2006 DHS/regions.csv")
pop$region.l <- tolower(pop$region)

# hr <- read.dta("D:/Documents/Data/DHSauto/iahr52dt/IAHR52FL.dta")
# hr.labs <- data.frame(names(hr),attributes(hr)[7])
# pr <- read.dta("D:/Documents/Data/DHSauto/iapr52dt/IAPR52FL.dta")
# pr.labs <- data.frame(names(pr),attributes(pr)[7])
# save(hr,hr.labs,pr,pr.labs,pov,file="india.RData")
load("india.RData")

names(pr)[which(names(pr)=="hv271")] <- "wealth"
pr$wealth <- pr$wealth/100000

#Rename sample.weights var
names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
pr$weights <- pr$sample.weights/1000000

#Rename urban var
names(pr)[which(names(pr)=="hv025")] <- "urban.rural"

#Rename educ var
names(pr)[which(names(pr)=="hv109")] <- "educ"
recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
  else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
  else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
  else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
  else if(x==5 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
pr$educ <- sapply(pr$educ,recode.educ)

#Rename age var
names(pr)[which(names(pr)=="hv105")] <- "age"

#Rename sex var
names(pr)[which(names(pr)=="hv104")] <- "sex"

#Rename cluster/hh var
names(pr)[which(names(pr)=="hv001")] <- "cluster"
names(pr)[which(names(pr)=="hv002")] <- "household"
names(pr)[which(names(pr)=="hvidx")] <- "line"
names(pr)[which(names(pr)=="hv112")] <- "mother.line"
pr$mother.line[which(pr$mother.line==99)] <- NA

#Head vars
names(pr)[which(names(pr)=="hv219")] <- "head.sex"
names(pr)[which(names(pr)=="hv220")] <- "head.age"

#reg?
names(pr)[which(names(pr)=="hv140")] <- "birth.cert"

#nutrition
names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
if(typeof(pr$woman.bmi)!="NULL"){
  pr$woman.bmi <- pr$woman.bmi/100 
}else{
  pr$woman.bmi <- NA
}
names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
if(typeof(pr$man.bmi)!="NULL"){
  pr$man.bmi <- pr$man.bmi/100 
}else{
  pr$man.bmi <- NA
}
names(pr)[which(names(pr)=="hc1")] <- "age.months"
names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
names(pr)[which(names(pr)=="hc3")] <- "height.cm"
names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
names(pr)[which(names(pr)=="hc5")] <- "child.height.age"
names(pr)[which(names(pr)=="hv024")] <- "region"
if(typeof(pr$child.height.age)=="NULL"){
  pr$child.height.age <- NA
}else{
  pr$child.height.age <- pr$child.height.age/100
}
pr$child.weights <- pr$weights

povcalcut <- 0.383
np20cut <- 0.2
nplcut <- subset(pov,year==2006)$pl.hc[[1]]
cuts <- c(povcalcut,np20cut,nplcut)
povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)

pr$p20 <- (pr$wealth < povperc[1])
pr$np20 <- (pr$wealth < povperc[2])
pr$npl <- (pr$wealth < povperc[3])

names(pr)[which(names(pr)=="hv245")] <- "hectares"
names(pr)[which(names(pr)=="sh44")] <- "religion"
names(pr)[which(names(pr)=="sh45")] <- "casteortribe"
names(pr)[which(names(pr)=="sh46")] <- "castetype"

pr$castetype <- unfactor(pr$castetype)
pr$castetype[which(pr$casteortribe=="no caste/tribe")] <- "no caste/tribe"
pr$castetype[which(pr$castetype=="don't know")] <- NA

keep <- c("wealth","weights","region","urban.rural","educ","age","sex","cluster","household","head.sex","head.age","p20","np20","npl"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
          ,"woman.bmi","man.bmi","child.weights","mother.bmi","castetype","religion"
)
prNames <- names(pr)
namesDiff <- setdiff(keep,prNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    pr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data <- pr[keep]

generateDummies <- function(df,vars){
  dummyList <- list()
  listIndex <- 1
  for(i in 1:length(vars)){
    var = vars[i]
    df[,var] <- factor(df[,var])
    cmd = paste0("dum = model.matrix( ~ ",var," - 1, data=df)")
    eval(parse(text=cmd))
    dummyList[[listIndex]] = dum
    listIndex <- listIndex + 1
  }
  return(dummyList)
}

catvars <- c("castetype","religion","urban.rural","sex","head.sex","educ","birth.cert")

dummyList <- generateDummies(data,catvars)

#Function to bind a list by of dfs by rowname, make our dummies line up
cbindlist <- function(list) {
  n <- length(list)
  res <- list[[1]]
  for (i in 2:n){
    item <- list[[i]]
    res <- cbind(res, item[match(rownames(res),rownames(item)),]) 
  }
  return(res)
}

dummies <- cbindlist(dummyList)

data$region <- unfactor(data$region)
data$region.l <- substr(data$region,6,nchar(data$region))

df <- cbindlist(list(data,dummies))
names(df) <- make.names(names(df),unique=TRUE)
df$man.bmi[which(df$man.bmi>90)] <- NA
df$woman.bmi[which(df$woman.bmi>90)] <- NA
df$child.height.age[which(df$child.height.age>90)] <- NA

region.tab <- data.table(df)
region.tab <- region.tab[,.(
  p20=weighted.mean(p20,weights,na.rm=TRUE)
  ,np20=weighted.mean(np20,weights,na.rm=TRUE)
  ,npl=weighted.mean(npl,weights,na.rm=TRUE)
  ,age=weighted.mean(age,weights,na.rm=TRUE)
  ,female=weighted.mean(sexfemale,weights,na.rm=TRUE)
  ,head.female=weighted.mean(head.sexfemale,weights,na.rm=TRUE)
  ,urban=weighted.mean(urban.ruralurban,weights,na.rm=TRUE)
  ,man.bmi=weighted.mean(man.bmi,weights,na.rm=TRUE)
  ,woman.bmi=weighted.mean(woman.bmi,weights,na.rm=TRUE)
  ,child.height.age=weighted.mean(child.height.age,weights,na.rm=TRUE)
  ,no.caste=weighted.mean(castetypeno.caste.tribe,weights,na.rm=TRUE)
  ,other.caste=weighted.mean(castetypenone.of.above,weights,na.rm=TRUE)
  ,obc=weighted.mean(castetypeother.backward.class,weights,na.rm=TRUE)
  ,scheduled.cast=weighted.mean(castetypescheduled.caste,weights,na.rm=TRUE)
  ,scheduled.tribe=weighted.mean(castetypescheduled.tribe,weights,na.rm=TRUE)
  ,hindu=weighted.mean(religionhindu,weights,na.rm=TRUE)
  ,muslim=weighted.mean(religionmuslim,weights,na.rm=TRUE)
  ,christian=weighted.mean(religionchristian,weights,na.rm=TRUE)
  ,sikh=weighted.mean(religionsikh,weights,na.rm=TRUE)
  ,buddhist=weighted.mean(religionbuddhist.neo.buddhist,weights,na.rm=TRUE)
  ,jain=weighted.mean(religionjain,weights,na.rm=TRUE)
  ,jewish=weighted.mean(religionjewish,weights,na.rm=TRUE)
  ,zoroastrian=weighted.mean(religionparsi.zoroastrian,weights,na.rm=TRUE)
  ,athiest=weighted.mean(religionno.religion,weights,na.rm=TRUE)
  ,donyi.polo=weighted.mean(religiondonyi.polo,weights,na.rm=TRUE)
  ,other.religion=weighted.mean(religionother,weights,na.rm=TRUE)
  ,higher.educ=weighted.mean(educHigher,weights,na.rm=TRUE)
  ,secondary.educ=weighted.mean(educSecondary,weights,na.rm=TRUE)
  ,primary.educ=weighted.mean(educPrimary,weights,na.rm=TRUE)
  ,no.educ=weighted.mean(educNo.education..preschool,weights,na.rm=TRUE)
  ,no.cert.no.reg=weighted.mean(birth.certneither.certificate.or.registered,weights,na.rm=TRUE)
  ,certificate=weighted.mean(birth.certhas.certificate,weights,na.rm=TRUE)
  ,registered=weighted.mean(birth.certregistered,weights,na.rm=TRUE)
  ,unknown.certificate=weighted.mean(birth.certdon.t.know,weights,na.rm=TRUE)
  )
  ,by=.(region.l)
  ]

region.tab <- merge(
  region.tab
  ,pop
  ,by="region.l"
  )
region.tab$region <- unfactor(region.tab$region)
region.tab$region[which(region.tab$region=="Jharkhand")] <- "Jharkhard"
write.csv(region.tab,"region_data.csv",row.names=FALSE,na="")
