####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
options(descr.plot = FALSE)

weighted.table <- function(x,y,w){
  return(
    data.frame(crosstab(x,y,weight=w,prop.t=TRUE)$prop.tbl)
  )
}

psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 

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

setwd("D:/Documents/Data/P20_2013/meta")

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE,na.strings="")
filenames <- povcalcuts$filename

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

####Run function####
# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
occList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  if(basename(dir) %in% filenames){
    hrwd <- basename(dir)
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    # pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.dta"))
    # var.labs <- data.frame(names(pr),attributes(pr)[7])
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=povcalcut)
    pr$p20 <- (pr$wealth<povperc)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){message("IR WD invalid");return(NA);}
    
    # ir <- read.dta(paste0(irwd,iso2,"IR",phase,"FL.dta"))
    # var.labs <- data.frame(names(ir),attributes(ir)[7])
    ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    names(ir)[which(names(ir)=="v191")] <- "wealth"
    ir$wealth <- ir$wealth/100000
    #Rename sample.weights var
    names(ir)[which(names(ir)=="v005")] <- "sample.weights"
    ir$weights <- ir$sample.weights/1000000
    
    ir$p20 <- (ir$wealth<povperc)
    
    #Rename partners occupation var
    names(ir)[which(names(ir)=="v705")] <- "partner.occ"
    if(typeof(ir$partner.occ)=="NULL"){partner.occ.missing<-TRUE}else{partner.occ.missing<-FALSE}
    
    #Rename occupation var
    names(ir)[which(names(ir)=="v714")] <- "working"
    if(typeof(ir$working)=="NULL"){working.missing<-TRUE}else{working.missing<-FALSE}
    
    #Rename age var
    names(ir)[which(names(ir)=="v012")] <- "age"
    
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    
    #Recode IR level
    recode.partner.working <- function(xv,v504v){
      results <- c()
      for(i in 1:length(xv)){
        x <- xv[i]
        v504 <- v504v[i]
        if(is.na(x)){result <- NA}
        else if(is.null(x)){result <- NULL}
        else if(x==98 | x==99 | tolower(x)=="don't know" | tolower(x)=="missing"){result <- NA}
        else if(x==0 | tolower(x)=="did not work"){result <- 0}
        else if(!is.na(v504)){result <- 1}
        else{result <- 0}
        results <- c(results,result)
      }
      return(results)
    }
    if(!partner.occ.missing){
      ir$partner.working <- recode.partner.working(ir$partner.occ,ir$v504) 
    }else{
      ir$partner.working <- NA
    }
    recode.working <- function(x){
      if(is.na(x)){return(NA)}
      else if(is.null(x)){return(NULL)}
      else if(x==9 | tolower(x)=="missing"){return(NA)}
      else if(x==0 | tolower(x)=="no"){return(0)}
      else if(x==1 | tolower(x)=="yes"){return(1)}
      else{return(NA)}
    }
    if(!working.missing){
      ir$working <- sapply(ir$working,recode.working)
    }else{
      ir$working <- NA
    }
    
    ir$percent.working <- psum(ir$partner.working,ir$working,na.rm=TRUE)/2
    
    data <- data.table(ir)[,.(
      percent.working = weighted.mean(percent.working,weights,na.rm=TRUE)
      ,women.working = weighted.mean(working,weights,na.rm=TRUE)
      ,men.working = weighted.mean(partner.working,weights,na.rm=TRUE)
    ),by=.(p20)]

    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    
    womans.occ <- weighted.table(ir$v717,ir$p20,ir$weights)
    names(womans.occ) <- c("field","p20","woman.occupation")
    if(!partner.occ.missing){
      mans.occ <- weighted.table(ir$partner.occ,ir$p20,ir$weights)
      names(mans.occ) <- c("field","p20","man.occupation")
      occupations <- merge(womans.occ,mans.occ,by=c("field","p20"),all=TRUE)
    }else{
      occupations <- womans.occ
      occupations$man.occupation <- NA
    }
    occupations$filename <- hrBase
    occList[[dataIndex]] <- occupations
    
    dataIndex <- dataIndex + 1
  }
}

total <- rbindlist(dataList)
occupations <- rbindlist(occList)
setwd("D:/Documents/Data/DHSmeta2")

pop <- povcalcuts[c("filename","female.15.49","male.15.49")]
pop$pop.15.49 <- psum(pop$female.15.49,pop$male.15.49,na.rm=TRUE)
total <- merge(total,pop,by="filename")
occupations <- merge(occupations,pop,by="filename")
global <- total[,.(
  percent.working = weighted.mean(percent.working,pop.15.49,na.rm=TRUE)
  ,women.working = weighted.mean(women.working,female.15.49,na.rm=TRUE)
  ,men.working = weighted.mean(men.working,male.15.49,na.rm=TRUE)
),by=.(p20)]
global.occ <- occupations[,.(
  man.occupation = weighted.mean(man.occupation,male.15.49,na.rm=TRUE)
  ,woman.occupation = weighted.mean(woman.occupation,female.15.49,na.rm=TRUE)
),by=.(p20,field)]

write.csv(total,"employment-nationally.csv",na="",row.names=FALSE)
write.csv(global,"employment-globally.csv",na="",row.names=FALSE)
write.csv(occupations,"occupations-nationally.csv",na="",row.names=FALSE)
write.csv(global.occ,"occupations-globally.csv",na="",row.names=FALSE)
