library(Hmisc)
library(readr)
library(varhandle)

load("C:/Users/Alex/Documents/Data/PNAD/Leitura_em_R_20170517/Leitura em R/dicPNAD2015.RData")
# 
# #Remove duplicate starts, we can recode UF later from other var if we need it
# dicdom2015 <- dicdom2015[c(1,3:nrow(dicdom2015)),]
# dicdom2015$cod <- unfactor(dicdom2015$cod)
# dicpes2015 <- dicpes2015[c(1,3:nrow(dicpes2015)),]
# dicpes2015$cod2 <- unfactor(dicpes2015$cod2)
# 
# # #Read fixed_width
# hr <- read_fwf("C:/Users/Alex/Documents/Data/PNAD/Dados_20170517/Dados/DOM2015.txt"
#                ,fwf_widths(dicdom2015$tamanho,col_names=dicdom2015$cod)
#                ,col_types=paste(rep("n",nrow(dicdom2015)),collapse="")
# )
# 
# # #Read fixed_width
# pr <- read_fwf("C:/Users/Alex/Documents/Data/PNAD/Dados_20170517/Dados/PES2015.txt"
#                ,fwf_widths(dicpes2015$tamanho2,col_names=dicpes2015$cod2)
#                ,col_types=paste(rep("n",nrow(dicpes2015)),collapse="")
# )
# 
# setwd("C:/Users/Alex/Documents/Data/PNAD/Dados_20170517/Dados/")
# source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")
# 
# catvars = c(
#   "V0203" #Walls
#   ,"V0204" #Roof
#   ,"V0207" #Own/Rent
#   ,"V0211" #Running water
#   ,"V0212" #Water source
#   ,"V0213" #Water from network?
#   ,"V0214" #Water from well
#   ,"V0215" #Bathroom on property
#   ,"V0216" #Share toilets
#   ,"V0217" #Type of bathroom
#   ,"V0218" #Destination of waste
#   ,"V0219" #Electrification
#   ,"V0220" #Binary asset vars
#   ,"V2020" #
#   ,"V0221" #
#   ,"V0222" #
#   ,"V0223" #
#   ,"V0224" #
#   ,"V0225" #
#   ,"V0226" #
#   ,"V0227" #
#   ,"V02272" #
#   ,"V02273" #
#   ,"V02274" #
#   ,"V2027" #
#   ,"V0228" #
#   ,"V0229" #
#   ,"V0230" #
#   ,"V0231" #
#   ,"V0232" #
#   ,"V02321" #
#   ,"V02322" #
#   ,"V02323" #
#   ,"V02324" #
#   ,"V02325" #
#   ,"V02326" #
#   ,"V02327" #
#   ,"V02424" #
#   ,"V02425" #
#   ,"V02426" #
#   ,"V2032" #
#   )
# 
# numvars = c(
#   "V0205" #Number of rooms
#   ,"V0206" #Number of bedrooms
#   ,"V2016" #Number of bathrooms
#   ,"V02270" #Numerical asset vars
#   ,"V02271" #
#   )
# 
# hr$urban = NA
# hr$urban[which(hr$V4105 %in% c(1:3))] = 1
# hr$urban[which(hr$V4105 %in% c(4:8))] = 0
# urbanvar = "urban"
# hr <- data.frame(hr)
# pr <- data.frame(pr)
# 
# hr$missingCount = rowSums(is.na(hr[c(catvars,numvars)]))
# hr = subset(hr,missingCount<46)
# 
# hr.wealth <- wealth(hr,catvars,numvars,urbanvar)
# keep = c("V0102","V0103","wealth")
# hr <- hr.wealth
# hr.wealth <- hr.wealth[keep]
# pr <- merge(pr,hr.wealth)
# 
# save(pr,hr,file="dados.RData")
setwd("C:/Users/Alex/Documents/Data/PNAD/Dados_20170517/Dados/")
load("dados.RData")

# wd <- "D:/Documents/Data/P20_2013/meta"
# setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

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

names(pr)[which(names(pr)=="V0301")] <- "line"
names(pr)[which(names(pr)=="V0102")] <- "cluster"
names(pr)[which(names(pr)=="V0103")] <- "household"

pr$urban = NA
pr$urban[which(pr$V4728 %in% c(1:3))] = 1
pr$urban[which(pr$V4728 %in% c(4:8))] = 0

names(pr)[which(names(pr)=="V4729")] <- "weights"
pr$weights <- pr$weights/1000

pr$sex <- NA
pr$sex[which(pr$V0302==2)] <- "Male"
pr$sex[which(pr$V0302==4)] <- "Female"
names(pr)[which(names(pr)=="V8005")] <- "age"

pr$birth.reg <- NA
pr$birth.reg[which(pr$V0408==2)] = 1
pr$birth.reg[which(pr$V0408==4)] = 0

povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
extcut <- subset(povcalcuts,filename=="Brazil")$ext
povperc <- weighted.percentile(pr$wealth,pr$weights,prob=c(povcalcut,extcut))

pr$p20 <- (pr$wealth < povperc[1])
pr$ext <- (pr$wealth < povperc[2])

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
pr$ageCategory <- factor(pr$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing")                          
)

# * http://stats.uis.unesco.org/unesco/TableViewer/tableView.aspx?ReportId=163
# * Entrance age of primary: 6y
# * Duration of primary: 5y
# * Entrance age of lower secondary: 11y
# * Durantion lower secondary: 4y
# * Entrance age high secondary: 15y
# * Duration high secondary: 3y /* reviewed January 19 2016 */
#   
#   ** 1- DEPRIVED IN EDUCATION **
#   
#   gen yschooling = 0 if (v0602==4 & v0606==4) | v6003==7 | v6003==9 /* never attended school, creche (preschool) and maternal */
pr$yschooling = NA
pr$yschooling[which((pr$V0602==4 & pr$V0606==4) | pr$V6003==7 | pr$V6003==9)] = 0
#   
#   * currently attending so I substract 1 year to get the completed grade *
#   replace yschooling = 0 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==1
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==1)] = 0
# replace yschooling = 1 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==2
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==2)] = 1
# replace yschooling = 2 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==3
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==3)] = 2
# replace yschooling = 3 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==4
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==4)] = 3
# replace yschooling = 4 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==5
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==5)] = 4
# replace yschooling = 5 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==6
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==6)] = 5
# replace yschooling = 6 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==7
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==7)] = 6
# replace yschooling = 7 if (pr$V6003==1 | pr$V6003==3) & pr$V0605==8
pr$yschooling[which((pr$V6003==1 | pr$V6003==3) & pr$V0605==8)] = 7
# replace yschooling = 8 if pr$V6003==1 & pr$V0605==0
pr$yschooling[which(pr$V6003==1 & pr$V0605==0)] = 8
# 
# replace yschooling = 8 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==1
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==1)] = 8
# replace yschooling = 9 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==2
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==2)] = 9
# replace yschooling = 10 if (pr$V6003==2 | pr$V6003==4) & pr$V0605==3
pr$yschooling[which((pr$V6003==2 | pr$V6003==4) & pr$V0605==3)] = 10
# replace yschooling = 11 if pr$V6003==2 & pr$V0605==4
pr$yschooling[which(pr$V6003==2 & pr$V0605==4)] = 11
# 
# replace yschooling = 12 if pr$V6003==5 & pr$V0605==1
pr$yschooling[which(pr$V6003==5 & pr$V0605==1)] = 12
# replace yschooling = 13 if pr$V6003==5 & pr$V0605==2
pr$yschooling[which(pr$V6003==5 & pr$V0605==2)] = 13
# replace yschooling = 14 if pr$V6003==5 & pr$V0605==3
pr$yschooling[which(pr$V6003==5 & pr$V0605==3)] = 14
# replace yschooling = 15 if pr$V6003==5 & pr$V0605==4
pr$yschooling[which(pr$V6003==5 & pr$V0605==4)] = 15
# replace yschooling = 16 if pr$V6003==5 & pr$V0605==5
pr$yschooling[which(pr$V6003==5 & pr$V0605==5)] = 16
# replace yschooling = 17 if pr$V6003==5 & pr$V0605==6
pr$yschooling[which(pr$V6003==5 & pr$V0605==6)] = 17
# 
# 
# *attended school in the past but not currently *
#   replace yschooling = 0 if (pr$V6007==1 | pr$V6007==4 | pr$V6007==6) & pr$V0609==3
pr$yschooling[which((pr$V6007==1 | pr$V6007==4 | pr$V6007==6) & pr$V0609==3)] = 0
# replace yschooling = 0 if pr$V6007==11 | pr$V6007==13 /*creche (preschool) and maternal */
pr$yschooling[which(pr$V6007==11 | pr$V6007==13)] = 0
#   
#   replace yschooling = 1 if pr$V6007==1 & pr$V0610==1
pr$yschooling[which(pr$V6007==1 & pr$V0610==1)] = 1
# replace yschooling = 2 if pr$V6007==1 & pr$V0610==2
pr$yschooling[which(pr$V6007==1 & pr$V0610==2)] = 2
# replace yschooling = 3 if pr$V6007==1 & pr$V0610==3
pr$yschooling[which(pr$V6007==1 & pr$V0610==3)] = 3
# replace yschooling = 4 if pr$V6007==1 & pr$V0610==4
pr$yschooling[which(pr$V6007==1 & pr$V0610==4)] = 4
# replace yschooling = 5 if pr$V6007==1 & pr$V0610==5
pr$yschooling[which(pr$V6007==1 & pr$V0610==5)] = 5
# replace yschooling = 6 if pr$V6007==1 & pr$V0610==6
pr$yschooling[which(pr$V6007==1 & pr$V0610==6)] = 6
# 
# replace yschooling = 5 if pr$V6007==2 & pr$V0609==3
pr$yschooling[which(pr$V6007==2 & pr$V0609==3)] = 5
# replace yschooling = 6 if pr$V6007==2 & pr$V0610==1
pr$yschooling[which(pr$V6007==2 & pr$V0610==1)] = 6
# replace yschooling = 7 if pr$V6007==2 & pr$V0610==2
pr$yschooling[which(pr$V6007==2 & pr$V0610==2)] = 7
# replace yschooling = 8 if pr$V6007==2 & pr$V0610==3
pr$yschooling[which(pr$V6007==2 & pr$V0610==3)] = 8
# replace yschooling = 9 if pr$V6007==2 & (pr$V0610==4 | pr$V0610==5)
pr$yschooling[which(pr$V6007==2 & (pr$V0610==4 | pr$V0610==5))] = 9
# 
# replace yschooling = 9 if pr$V6007==3 & pr$V0609==3
pr$yschooling[which(pr$V6007==3 & pr$V0609==3)] = 9
# replace yschooling = 10 if pr$V6007==3 & pr$V0610==1
pr$yschooling[which(pr$V6007==3 & pr$V0610==1)] = 10
# replace yschooling = 11 if pr$V6007==3 & pr$V0610==2
pr$yschooling[which(pr$V6007==3 & pr$V0610==2)] = 11
# replace yschooling = 12 if pr$V6007==3 & pr$V0610==3
pr$yschooling[which(pr$V6007==3 & pr$V0610==3)] = 12
# replace yschooling = 13 if pr$V6007==3 & pr$V0610==4
pr$yschooling[which(pr$V6007==3 & pr$V0610==4)] = 13
# 
# replace yschooling = 1 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==1
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==1)] = 1
# replace yschooling = 2 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==2
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==2)] = 2
# replace yschooling = 3 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==3
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==3)] = 3
# replace yschooling = 4 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==4
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==4)] = 4
# replace yschooling = 5 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==5
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==5)] = 5
# replace yschooling = 6 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==6
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==6)] = 6
# replace yschooling = 7 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==7
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==7)] = 7
# replace yschooling = 8 if (pr$V6007==4 | pr$V6007==6) & pr$V0610==8
pr$yschooling[which((pr$V6007==4 | pr$V6007==6) & pr$V0610==8)] = 8
# replace yschooling = 9 if pr$V6007==4 & pr$V0610==0
pr$yschooling[which(pr$V6007==4 & pr$V0610==0)] = 9
# 
# replace yschooling = 8 if (pr$V6007==5 | pr$V6007==7) & pr$V0609==3
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0609==3)] = 8
# replace yschooling = 9 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==1
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==1)] = 9
# replace yschooling = 10 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==2
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==2)] = 10
# replace yschooling = 11 if (pr$V6007==5 | pr$V6007==7) & pr$V0610==3
pr$yschooling[which((pr$V6007==5 | pr$V6007==7) & pr$V0610==3)] = 11
# replace yschooling = 12 if pr$V6007==5 & pr$V0610==4
pr$yschooling[which(pr$V6007==5 & pr$V0610==4)] = 12
# 
# replace yschooling = 12 if pr$V6007==8 & pr$V0609==3
pr$yschooling[which(pr$V6007==8 & pr$V0609==3)] = 12
# replace yschooling = 13 if pr$V6007==8 & pr$V0610==1
pr$yschooling[which(pr$V6007==8 & pr$V0610==1)] = 13
# replace yschooling = 14 if pr$V6007==8 & pr$V0610==2
pr$yschooling[which(pr$V6007==8 & pr$V0610==2)] = 14
# replace yschooling = 15 if pr$V6007==8 & pr$V0610==3
pr$yschooling[which(pr$V6007==8 & pr$V0610==3)] = 15
# replace yschooling = 16 if pr$V6007==8 & pr$V0610==4
pr$yschooling[which(pr$V6007==8 & pr$V0610==4)] = 16
# replace yschooling = 17 if pr$V6007==8 & pr$V0610==5
pr$yschooling[which(pr$V6007==8 & pr$V0610==5)] = 17
# replace yschooling = 18 if pr$V6007==8 & pr$V0610==6
pr$yschooling[which(pr$V6007==8 & pr$V0610==6)] = 18
# 
# 
# replace yschooling=. if age<5
# pr$yschooling[which(pr$age<5)] = NA
recode.educ = function(y){
  if(is.na(y)){return(NA)}
  if(y<5){return("No education, preschool")}
  if(y<9){return("Primary")}
  if(y<12){return("Secondary")}
  return("Higher")
}
pr$educ = sapply(pr$yschooling,recode.educ)
keep <- c("wealth","weights","urban.rural","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
          ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
          ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting","npl","np20","ext"
)
prNames <- names(pr)
namesDiff <- setdiff(keep,prNames)
if(length(namesDiff)>0){
  for(y in 1:length(namesDiff)){
    pr[namesDiff[y]] <- NA
    message(paste("Missing variable",namesDiff[y]))
  } 
}
data.total <- pr[keep]
data.total$filename <- "Brazil"

library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(WDI)
library(varhandle)
require(zoo)

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))
# 
# data.total$whipple <- substr(as.character(data.total$age),nchar(as.character(data.total$age)),nchar(as.character(data.total$age)))
# data.total$whipple05 <- data.total$whipple %in% c("0","5")
# mean(data.total$whipple05,na.rm=TRUE)*500
# 
# p20 <- subset(data.total,p20==TRUE & age>=15 & age<=49)
# mean(p20$whipple05,na.rm=TRUE)*500
# nonp20 <- subset(data.total,p20==FALSE  & age>=15 & age<=49)
# mean(nonp20$whipple05,na.rm=TRUE)*500
# 
# children <- subset(data.total,!is.na(age.months))
# children$whipple <- substr(as.character(children$age.months),nchar(as.character(children$age.months)),nchar(as.character(children$age.months)))
# children$whipple06 <- children$whipple %in% c("0","6")
# mean(children$whipple06,na.rm=TRUE)*500
# p20.kids <- subset(children,p20==TRUE)
# mean(p20.kids$whipple06,na.rm=TRUE)*500
# nonp20.kids <- subset(children,p20==FALSE)
# mean(nonp20.kids$whipple06,na.rm=TRUE)*500

pop.confidence <- function(x,y,w,pop){
  ct <- crosstab(x,y,weight=w,prop.t=TRUE,drop.levels=FALSE)
  props <- ct$prop.tbl
  cv <- sd(w,na.rm=TRUE)/mean(w,na.rm=TRUE)
  deft <- cv*cv+1
  n <- ct$total.n
  SEs <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(props*(1-props)))
  corrected.SEs <- SEs*deft
  low.end <- (props-(2*corrected.SEs))*pop
  low.end <- pmax(low.end,0)
  estimate.point <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
      low = low.end
      ,estimate = estimate.point
      ,high = high.end
    )
  )
}

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SI.POV.NAHC"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

dat <- dat[c("iso3c","year","SI.POV.NAHC")]
names(dat) <- c("iso3","year","hc")
dat$hc <- dat$hc/100
dat <- dat[order(dat$year),]
dat <- dat[order(dat$iso3),]
colname <- "hc"
dat <- ddply(dat,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival 
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(dat) <- c("iso3","year","pl.hc","pl.hc.original")
countryMeta <- join(countryMeta,dat,by=c("iso3","year"))

newNames <- c("p20.rural"
              ,"p20.urban"
              ,"p80.rural"
              ,"p80.urban"
              ,"p80.over25.noeduc"
              ,"p80.over25.primary"
              ,"p80.over25.secondary"
              ,"p80.over25.higher"
              ,"p20.over25.noeduc"
              ,"p20.over25.primary"
              ,"p20.over25.secondary"
              ,"p20.over25.higher"
              ,"p80.o25.m.noeduc"
              ,"p80.o25.m.primary"
              ,"p80.o25.m.secondary"
              ,"p80.o25.m.higher"
              ,"p20.o25.m.noeduc"
              ,"p20.o25.m.primary"
              ,"p20.o25.m.secondary"
              ,"p20.o25.m.higher"
              ,"p80.o25.f.noeduc"
              ,"p80.o25.f.primary"
              ,"p80.o25.f.secondary"
              ,"p80.o25.f.higher"
              ,"p20.o25.f.noeduc"
              ,"p20.o25.f.primary"
              ,"p20.o25.f.secondary"
              ,"p20.o25.f.higher"
              ,"p20.male"
              ,"p20.female"
              ,"p80.male"
              ,"p80.female"
              ,"p20.male.head"
              ,"p20.female.head"
              ,"p80.male.head"
              ,"p80.female.head"
              ,"p80.unregistered"
              ,"p80.registered"
              ,"p20.unregistered"
              ,"p20.registered"
              ,"p80.notstunted"
              ,"p80.stunted"
              ,"p20.notstunted"
              ,"p20.stunted"
              ,"ext.rural"
              ,"ext.urban"
              ,"n.ext.rural"
              ,"n.ext.urban"
              ,"n.ext.over25.noeduc"
              ,"n.ext.over25.primary"
              ,"n.ext.over25.secondary"
              ,"n.ext.over25.higher"
              ,"ext.over25.noeduc"
              ,"ext.over25.primary"
              ,"ext.over25.secondary"
              ,"ext.over25.higher"
              ,"ext.male"
              ,"ext.female"
              ,"n.ext.male"
              ,"n.ext.female"
              ,"ext.male.head"
              ,"ext.female.head"
              ,"n.ext.male.head"
              ,"n.ext.female.head"
              ,"n.ext.unregistered"
              ,"n.ext.registered"
              ,"ext.unregistered"
              ,"ext.registered"
              ,"n.ext.notstunted"
              ,"n.ext.stunted"
              ,"ext.notstunted"
              ,"ext.stunted"
              ,"p20.male.stunted"
              ,"p20.female.stunted"
              ,"p20.male.notstunted"
              ,"p20.female.notstunted"
              ,"p80.male.stunted"
              ,"p80.female.stunted"
              ,"p80.male.notstunted"
              ,"p80.female.notstunted"
              ,"p80.maternal.deaths"
              ,"p80.no.maternal.deaths"
              ,"p20.maternal.deaths"
              ,"p20.no.maternal.deaths"
              ,"male.stunted"
              ,"female.stunted"
              ,"male.notstunted"
              ,"female.notstunted"
              ,"surveyed.pop"
              ,"surveyed.households"
              ,"surveyed.men"
              ,"surveyed.women"
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  surveyed.pop <- nrow(dat)
  surveyed.households <- length(unique(dat$household))
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  over25.male <- subset(over25,sex=="Male")
  over25.female <- subset(over25,sex=="Female")
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  surveyed.pop <- nrow(dat)
  countryMeta$surveyed.pop[which(countryMeta$filename==this.filename)] <- surveyed.pop
  surveyed.households <- nrow(unique(data.frame(dat)[c("cluster","household")]))
  countryMeta$surveyed.households[which(countryMeta$filename==this.filename)] <- surveyed.households
  surveyed.men <- nrow(men)
  countryMeta$surveyed.men[which(countryMeta$filename==this.filename)] <- surveyed.men
  surveyed.women <- nrow(women)
  countryMeta$surveyed.women[which(countryMeta$filename==this.filename)] <- surveyed.women
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
      subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$p80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Urban-ext
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Educ-P20
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$p20,over25$weights,this.pop.over25)
      countryMeta$p80.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Educ-ext
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$ext,over25$weights,this.pop.over25)
      countryMeta$n.ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Sex-ext
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-ext
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    # #Maternal deaths
    # dat$maternal <- dat$maternal.deaths > 0 
    # if(length(dat$maternal[which(!is.na(dat$maternal))])!=0){
    #   confidence.tab <- pop.confidence(dat$maternal,dat$p20,dat$weights,this.pop)
    #   countryMeta$p80.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
    #   countryMeta$p80.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
    #   countryMeta$p20.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
    #   countryMeta$p20.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0}) 
    # }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition p20
    under5$stunted <- (under5$child.height.age <= -2) & (under5$child.height.age > -6)
    under5$stunted[which(is.na(under5$stunting))] <- NA
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition ext
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$n.ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition by gender
    under5.male <- subset(under5,sex=="Male")
    under5.female <- subset(under5,sex=="Female")
    if(length(under5.male$stunted[which(!is.na(under5.male$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.male$stunted,under5.male$p20,under5.male$weights,this.pop.under5.male)
      countryMeta$p80.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0}) 
    }
    if(length(under5.female$stunted[which(!is.na(under5.female$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.female$stunted,under5.female$p20,under5.female$weights,this.pop.under5.female)
      countryMeta$p80.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Educ-P20 by gender
    if(length(over25$educ[which(!is.na(over25.male$educ))])!=0){
      confidence.tab <- pop.confidence(over25.male$educ,over25.male$p20,over25.male$weights,this.pop.over25.male)
      countryMeta$p80.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    if(length(over25$educ[which(!is.na(over25.female$educ))])!=0){
      confidence.tab <- pop.confidence(over25.female$educ,over25.female$p20,over25.female$weights,this.pop.over25.female)
      countryMeta$p80.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
  }
}

countryMeta$female.notstunted <- countryMeta$p80.female.notstunted+countryMeta$p20.female.notstunted
countryMeta$female.stunted <- countryMeta$p80.female.stunted+countryMeta$p20.female.stunted
countryMeta$male.notstunted <- countryMeta$p80.male.notstunted+countryMeta$p20.male.notstunted
countryMeta$male.stunted <- countryMeta$p80.male.stunted+countryMeta$p20.male.stunted

countryMeta <- subset(countryMeta,filename=="Brazil")

write.csv(countryMeta,"bycountry_tabs_Brazil.csv",row.names=FALSE,na="")

br.data.total <- data.total
save(br.data.total,file="br.data.total.RData")
