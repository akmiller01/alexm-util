####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(WDI)
library(varhandle)
require(zoo)

# wd <- "D:/Documents/Data/MICSmeta/"
# setwd(wd)
# 
# all.years <- read.csv("all.years.csv",as.is=TRUE)
# all.isos <- read.csv("D:/Documents/Data/DHS map/isos.csv")
# 
# ####Run function####
# # set our working directory, change this if using on another machine
# wd <- "D:/Documents/Data/DHSauto/"
# setwd(wd)
# 
# # List out all the directories in our wd, this is where our data is contained
# # dirs <- list.files(wd,pattern="ughr..dt",include.dirs=TRUE,full.names=TRUE)
# dirs <- list.files(wd,pattern="..hr..dt",include.dirs=TRUE,full.names=TRUE)
# 
# dataList <- list()
# dataIndex <- 1
# 
# # Loop through every dir
# for(i in 2:length(dirs)){
#   dir <- dirs[i]
#   # Pull some coded info out of the dir name
#   country <- tolower(substr(basename(dir),1,2))
#   recode <- tolower(substr(basename(dir),3,4))
#   phase <- as.integer(substr(basename(dir),5,5))
#   # For this analysis, we're only interested in individual member recodes, or "hr"
#   if(basename(dir) %in% all.years$filename){
#     year <- subset(all.years,filename==basename(dir))$year
#     iso3 <- subset(all.isos,cc==country)$iso3
#     message(basename(dir))
#     hrwd <- dir
#     if(!file_test(op="-d", hrwd)){next;}
#     
#     hrBase <- basename(hrwd)
#     iso2 <- toupper(substr(hrBase,1,2))
#     phase <- substr(hrBase,5,6)
#     
#     krwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"kr",phase,"dt/")
#     if(!file_test(op="-d", krwd)){next;}
#     
# #     kr <- read.dta(paste0(krwd,iso2,"KR",phase,"FL.dta"))
# #     var.labs <- data.frame(names(kr),attributes(kr)[7])
# #     View(var.labs)
#     
#     kr <- read.csv(paste0(krwd,iso2,"KR",phase,"FL.csv")
#                    ,na.strings="",as.is=TRUE,check.names=FALSE)
#     
#     #Rename cluster/hh var
#     names(kr)[which(names(kr)=="v001")] <- "cluster"
#     names(kr)[which(names(kr)=="v002")] <- "household"
#     
# #     names(kr)[which(names(kr)=="v190")] <- "wealth"
# #     kr$wealth <- tolower(kr$wealth)
# 
#     names(kr)[which(names(kr)=="v011")] <- "mother.birthdate.cmc"
#     names(kr)[which(names(kr)=="b3")] <- "child.birthdate.cmc"
#     names(kr)[which(names(kr)=="v008")] <- "interview.date.cmc"
#     kr$mother.age.m <- kr$interview.date.cmc-kr$mother.birthdate.cmc
#     kr$child.age.m <- kr$interview.date.cmc-kr$child.birthdate.cmc
#     kr$mother.age.at.birth <- (kr$mother.age.m-kr$child.age.m)/12
# 
#     
#     names(kr)[which(names(kr)=="v440")] <- "child.height.age"
#     if(typeof(kr$child.height.age)=="NULL"){
#       kr$child.height.age <- NA
#     }else{
#       kr$child.height.age <- kr$child.height.age/100
#     }
#     
#     #Rename sample.weights var
#     names(kr)[which(names(kr)=="v005")] <- "sample.weights"
#     kr$weights <- kr$sample.weights/1000000
#     
#     keep <- c("cluster","household","weights","child.height.age","mother.age.m","child.age.m","mother.age.at.birth")
#     krNames <- names(kr)
#     namesDiff <- setdiff(keep,krNames)
#     if(length(namesDiff)>0){
#       for(y in 1:length(namesDiff)){
#         kr[namesDiff[y]] <- NA
#         message(paste("Missing variable",namesDiff[y]))
#       } 
#     }
#     data <- kr[keep]
#     data$filename <- hrBase
#     data$year <- year
#     data$iso3 <- iso3
#     dataList[[dataIndex]] <- data
#     dataIndex <- dataIndex + 1
#   }
# }
# 
# ig <- rbindlist(dataList)
# 
# setwd("D:/Documents/Data/P20 baseline")

# save(ig,file="ig.RData")
load("ig.RData")

ig$stunted <- NA
# ig$stunted[which(ig$child.height.age<=-2)] <- TRUE
# ig$stunted[which(ig$child.height.age>-2 & ig$child.height.age<20)] <- FALSE
ig$stunted[which(ig$child.height.age<=-2)] <- "Stunted"
ig$stunted[which(ig$child.height.age>-2 & ig$child.height.age<20)] <- "Not stunted"

ig$under18.mom <- NA
ig$under18.mom[which(ig$mother.age.at.birth<18)] <- "Mother under 18"
ig$under18.mom[which(ig$mother.age.at.birth>=18)] <- "Mother 18 or older"

# ug.tab <- data.table(ug.ig)
# ug.tab <- ug.tab[,.(
#   child.stunting = weighted.mean(stunted,weights,na.rm=TRUE)*100
# )
# ,by=.(iso3,year,under18.mom)]

pop <- read.csv("undesa.pop.csv")
cc <- read.csv("country-codes.csv")
cc <- cc[c("ISO3166.1.Alpha.3","ISO3166.1.numeric")]
names(cc) <- c("iso3","LocID")
pop <- subset(pop,Variant=="Medium" & Sex=="Both" & AgeGrp=="0-4")
pop <- merge(pop,cc,by="LocID")
pop$under5.pop <- pop$Value*1000
setnames(pop,"Time","year")
pop <- pop[c("iso3","year","under5.pop")]

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

iso3s <- unique(ig$iso3)
for(this.iso3 in iso3s){
  dat <- subset(ig,iso3==this.iso3)
  years <- unique(dat$year)
  for(this.year in years){
    if(this.iso3=="ZAR"){this.iso3="COD"}
    if(this.iso3=="TMP"){this.iso3="TLS"}
    message(paste(this.iso3,this.year))
    this.pop <- subset(pop,year==this.year & iso3==this.iso3)$under5.pop
    dat <- subset(dat,year==this.year)
    #Intergenerational age vs. stunting
    if(length(dat$stunted[which(!is.na(dat$stunted))])!=0){
      confidence.tab <- pop.confidence(dat$stunted,dat$under18.mom,dat$weights,this.pop)
      if(is.null(crossTabs[[paste0(this.iso3,this.year)]])){
        crossTabs[[paste0(this.iso3,this.year)]] <- confidence.tab
      }else{
        crossTabs[[paste0(this.iso3,this.year)]]$low <- crossTabs[[paste0(this.iso3,this.year)]]$low + conform(crossTabs[[paste0(this.iso3,this.year)]]$low,confidence.tab$low)
        crossTabs[[paste0(this.iso3,this.year)]]$estimate <- crossTabs[[paste0(this.iso3,this.year)]]$estimate + conform(crossTabs[[paste0(this.iso3,this.year)]]$estimate,confidence.tab$estimate)
        crossTabs[[paste0(this.iso3,this.year)]]$high <- crossTabs[[paste0(this.iso3,this.year)]]$high + conform(crossTabs[[paste0(this.iso3,this.year)]]$high,confidence.tab$high)
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

saveWorkbook(wb, "intergenerational.xlsx", overwrite = TRUE)
