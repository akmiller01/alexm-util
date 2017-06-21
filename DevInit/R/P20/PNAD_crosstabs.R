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
#   ,"V0216" #Use bathroom or...
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

wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

# povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

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
pr$birth.reg[which(pr$V0408==4)] = 1

# povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
povcalcut = 0.0687
povperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)

pr$p20 <- (pr$wealth < povperc)

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

recode.educ1 <- function(x){
  if(is.na(x)){return(NA)}
  else if(x %in% c(10,11,12,13)){return("No education, preschool")}
  else if(x %in% c(1,4,6)){return("Primary")}
  else if(x %in% c(2,3,5,7)){return("Secondary")}
  else if(x %in% c(8,9)){return("Higher")}
  else{return(NA)}
}
recode.educ2 <- function(x){
  if(is.na(x)){return(NA)}
  else if(x %in% c(7,8,9)){return("No education, preschool")}
  else if(x %in% c(1,3,6)){return("Primary")}
  else if(x %in% c(10)){return("Secondary")}
  else if(x %in% c(2,4,5,11)){return("Higher")}
  else{return(NA)}
}
pr$attends <- NA
pr$attends[which(pr$V0602==2)] = 1
pr$attends[which(pr$V0602==4)] = 0
pr$attended <- NA
pr$attended[which(pr$V0606==2)] = 1
pr$attended[which(pr$V0606==4)] = 0
pr$educ1 <- sapply(pr$V6007,recode.educ1)
pr$educ2 <- sapply(pr$V6003,recode.educ2)
pr$educ <- pr$educ1
pr$educ[which(is.na(pr$educ))] <- pr$educ2[which(is.na(pr$educ))]
pr$educ[which(pr$attends==0 & pr$attended==0)] <- "No education, preschool"
pr$educ <- factor(pr$educ
                  ,levels = c("No education, preschool","Primary","Secondary","Higher")
)
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
