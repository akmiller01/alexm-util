####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
require(zoo)

setwd("C:/Users/Alex/Documents/Data/P20/Meta")
povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")
maternal.varNames <- read.csv("mics_child_vars.csv",as.is=TRUE,na.strings="")

wd <- "C:/Users/Alex/Documents/Data/P20/MICS"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "C:/Users/Alex/Documents/Data/P20/MICS/Algeria_MICS4_Datasets"

uniqueAnswers <- c()

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(
    (hrBase %in% povcalcuts$filename) 
    & (hrBase %in% unique(varNames$filename))
    & (hrBase %in% unique(classes$filename))
    & (hrBase %in% unique(maternal.varNames$filename))
  ){
    message(hrBase)
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","wm.RData"))
    wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
    names(wm) <- tolower(names(wm))
    skilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==1 & filename==hrBase)$var
    unskilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==0 & filename==hrBase)$var
    
    #maternal
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "line"
    names(wm)[which(names(wm)=="wmweight")] <- "woman.weights"
    
    for(var in skilledBirthVars){
      message(var)
      uniqueAnswers <- c(uniqueAnswers,unique(unfactor(wm[,var])))
    }
    for(var in unskilledBirthVars){
      message(var)
      uniqueAnswers <- c(uniqueAnswers,unique(unfactor(wm[,var])))
    }
  }
}

uniqueAnswers <- unique(uniqueAnswers)