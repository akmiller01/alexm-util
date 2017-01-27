####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)


wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

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
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir

    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.dta"))
    var.labs <- data.frame(names(pr),attributes(pr)[7])
    names(var.labs) <- c("var","description")
    var.labs$filename <- basename(dir)
    
    dataList[[dataIndex]] <- var.labs
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(hrBase %in% povcalcuts$filename){
    
    message(hrBase) 
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    if(exists("ch")){rm(ch)}
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    load(paste0(dir,"/","ch.RData"))
    load(paste0(dir,"/","wm.RData"))
    
    var.labs <- data.frame(names(hl)[1:length(names(hl))-1],attributes(hl)$variable.labels)
    names(var.labs) <- c("var","description")
    var.labs$filename <- basename(dir)
    
    dataList[[dataIndex]] <- var.labs
    dataIndex <- dataIndex + 1
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)

var.total <- rbindlist(dataList)
write.csv(var.total,"total_vars.csv",row.names=FALSE)
