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
# dirs <- list.dirs(wd,full.names=TRUE)
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
#     kr <- read.csv(paste0(krwd,iso2,"KR",phase,"FL.csv")
#                    ,na.strings="",as.is=TRUE,check.names=FALSE)
#     
#     #Rename cluster/hh var
#     names(kr)[which(names(kr)=="v001")] <- "cluster"
#     names(kr)[which(names(kr)=="v002")] <- "household"
#     
#     names(kr)[which(names(kr)=="v190")] <- "wealth"
#     kr$wealth <- tolower(kr$wealth)
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
#     keep <- c("wealth","weights","child.height.age","cluster","household")
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
# stunting <- rbindlist(dataList)
# stunting$wealth[which(stunting$wealth==1)] <- "poorest"
# stunting$wealth[which(stunting$wealth==2)] <- "poorer"
# stunting$wealth[which(stunting$wealth==3)] <- "middle"
# stunting$wealth[which(stunting$wealth==4)] <- "richer"
# stunting$wealth[which(stunting$wealth==5)] <- "richest"
# 
setwd("D:/Documents/Data/P20 baseline")
# 
# save(stunting,file="stunting.micro.RData")
load("stunting.micro.RData")

stunting$category <- NA
stunting$category[which(stunting$wealth=="poorest")] <- "poorest"
stunting$category[which(stunting$wealth!="poorest")] <- "rest"
stunting <- stunting[complete.cases(stunting$category)]

stunting$stunted <- NA
stunting$stunted[which(stunting$child.height.age<=-2)] <- TRUE
stunting$stunted[which(stunting$child.height.age>-2 & stunting$child.height.age<20)] <- FALSE

stunt.tab <- data.table(stunting)
stunt.tab <- stunt.tab[,.(
  stunting = weighted.mean(stunted,weights,na.rm=TRUE)*100
  )
  ,by=.(iso3,year,category)]

stunt.tab <- reshape(stunt.tab,timevar="category",idvar=c("iso3","year"),direction="wide")
stunt.tab <- stunt.tab[complete.cases(stunt.tab$stunting.poorest),]

stunt.tab$latest <- rev(!duplicated(stunt.tab[,list(rev(stunt.tab$iso3))]))

write.csv(stunt.tab,"stunting-by-quintile.csv",na="",row.names=FALSE)
