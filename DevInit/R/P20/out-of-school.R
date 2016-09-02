####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(WDI)
library(varhandle)
require(zoo)

wd <- "D:/Documents/Data/MICSmeta/"
setwd(wd)

all.years <- read.csv("all.years.csv",as.is=TRUE)
all.isos <- read.csv("D:/Documents/Data/DHS map/isos.csv")

source("C:/git/alexm-util/DevInit/R/P20/povcal_api2.R")

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

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.files(wd,pattern="..hr..dt",include.dirs=TRUE,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% all.years$filename){
    message(basename(dir))
    year <- subset(all.years,filename==basename(dir))$year
    iso3 <- subset(all.isos,cc==country)$iso3
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
#     pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.dta"))
#     var.labs <- data.frame(names(pr),attributes(pr)[7])
#     View(var.labs)
    
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000

    povcalcut <- povcal(tolower(iso2),year)[["p20"]]
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    
    pr$p20 <- (pr$wealth < povperc)
    
    #Rename educ var
    names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
      else if(x==0 | tolower(x)=="no education, preschool" | tolower(x)=="no education"){return("No education, preschool")}
      else if(x==1 | tolower(x)=="incomplete primary"){return("Incomplete primary")}
      else if(x==2 | tolower(x)=="complete primary" ){return("Complete primary")}
      else if(x==3 | tolower(x)=="incomplete secondary"){return("Incomplete secondary")}
      else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
      else if(x==5 | tolower(x)=="higher"){return("Higher")}
      else{return(NA)}
    }
    pr$educ <- sapply(pr$educ,recode.educ)
    names(pr)[which(names(pr)=="hv110")] <- "still.in.school"
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    
    keep <- c("cluster","household","weights","age","sex","wealth","educ","still.in.school","p20")
    prNames <- names(pr)
    namesDiff <- setdiff(keep,prNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        pr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- pr[keep]
    data$filename <- hrBase
    data$year <- year
    data$iso3 <- iso3
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

oos <- rbindlist(dataList)
setwd("D:/Documents/Data/P20 baseline")
save(oos,file="oos.RData")
# load("oos.RData")

oos <- subset(oos,age<25 & age>=5)
oos.complete <- oos[complete.cases(oos$still.in.school)]
complete.isos <- unique(oos.complete$iso3)
oos <- subset(oos, iso3 %in% complete.isos)
oos$still.in.school <- tolower(oos$still.in.school)
oos$still.in.school[which(oos$still.in.school==0)] <- "Not currently enrolled"
oos$still.in.school[which(oos$still.in.school==1)] <- "Currently enrolled"
oos$still.in.school[which(oos$still.in.school==9)] <- NA
oos$still.in.school[which(oos$still.in.school=="no")] <- "Not currently enrolled"
oos$still.in.school[which(oos$still.in.school=="yes")] <- "Currently enrolled"

oos$attainment <- NA
oos$attainment[which(oos$educ=="Secondary" | oos$educ=="Higher")] <- TRUE
oos$attainment[which(!is.na(oos$educ) & oos$educ!="Higher" & oos$educ!="Secondary")] <- FALSE

oos$p20.str <- NA
oos$p20.str[which(oos$p20==TRUE)] <- "P20"
oos$p20.str[which(oos$p20==FALSE)] <- "Non-P20"

#Only interested in enrollable population
graduated <- data.table(oos)
graduated <- graduated[,.(percent=weighted.mean(attainment,weights,na.rm=TRUE)),by=.(iso3,year)]
graduated <- graduated[complete.cases(graduated),]

pop <- read.csv("undesa.pop.csv")
cc <- read.csv("country-codes.csv")
cc <- cc[c("ISO3166.1.Alpha.3","ISO3166.1.numeric")]
names(cc) <- c("iso3","LocID")
pop <- subset(pop,Variant=="Medium" & Sex=="Both" & AgeGrpStart<25 & AgeGrpStart>0)
pop <- merge(pop,cc,by="LocID")
pop <- data.table(pop)
setnames(pop,"Time","year")
pop <- pop[,.(pop=sum(Value)*1000),by=.(iso3,year)]

crossTabs <- list()
library(descr)
options(descr.plot = FALSE)

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
# conform <- function(complete,incomplete){
#   return(
#     pmax(
#       incomplete[
#         match(
#           rownames(complete)
#           ,rownames(incomplete)
#         )
#         ,
#         match(
#           colnames(complete)
#           ,colnames(incomplete)
#         )
#         ]
#       ,0
#       ,na.rm=TRUE
#     )
#   )
# }

iso3s <- unique(oos$iso3)
for(this.iso3 in iso3s){
  #Only subset of ungraduated
  dat <- subset(oos,iso3==this.iso3 & attainment==FALSE)
  years <- unique(dat$year)
  for(this.year in years){
    graduated.percentage <- subset(graduated,year==this.year & iso3==this.iso3)$percent
    if(this.iso3=="ZAR"){this.iso3="COD"}
    if(this.iso3=="TMP"){this.iso3="TLS"}
    message(paste(this.iso3,this.year))
    this.pop <- subset(pop,year==this.year & iso3==this.iso3)$pop*(1-graduated.percentage)
    dat <- subset(dat,year==this.year)
    #Still in school
    if(length(dat$still.in.school[which(!is.na(dat$still.in.school))])!=0){
      confidence.tab <- pop.confidence(dat$still.in.school,dat$p20.str,dat$weights,this.pop)
      if(is.null(crossTabs[[paste0(this.iso3,this.year)]])){
        crossTabs[[paste0(this.iso3,this.year)]] <- confidence.tab
      }else{
        crossTabs[[paste0(this.iso3,this.year)]]$low <- crossTabs[[paste0(this.iso3,this.year)]]$low + conform(crossTabs[[paste0(this.iso3,this.year)]]$low,confidence.tab$low)
        crossTabs[[paste0(this.iso3,this.year)]]$estimate <- crossTabs[[paste0(this.iso3,this.year)]]$estimate + conform(crossTabs[[paste0(this.iso3,this.year)]]$estimate,confidence.tab$estimate)
        crossTabs[[paste0(this.iso3,this.year)]]$hoosh <- crossTabs[[paste0(this.iso3,this.year)]]$hoosh + conform(crossTabs[[paste0(this.iso3,this.year)]]$hoosh,confidence.tab$hoosh)
      }  
    }
  }
}

library(openxlsx)

#Create workbook
wb <- createWorkbook("crosstabs")

crossNames <- names(crossTabs)
for(i in 1:length(crossNames)){
  crossName <- crossNames[i]
  crossTab <- crossTabs[[i]]
  tabNames <- names(crossTab)
  for(j in 1:length(crossTab)){
    this.tab <- crossTab[[j]]
    this.tabname <- paste0(crossName,".",tabNames[j])
    addWorksheet(wb,this.tabname)
    writeData(wb,sheet=this.tabname,this.tab,colNames=TRUE,rowNames=TRUE)
  }
}

saveWorkbook(wb, "still-in-school.xlsx", overwrite = TRUE)

