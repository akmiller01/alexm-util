####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
require(zoo)

wd <- "C:/Users/Alex/Documents/Data/P20/Meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
asean <- c(
  "idhr63dt"
  ,"khhr72dt"
  ,"mmhr71dt"
  ,"phhr61dt"
  ,"Lao People's Democratic Republic_LSIS_Datasets"
  ,"Thailand_MICS4_Datasets"
  ,"Viet Nam_MICS5_Datasets"
)

filename <- c(
  "idhr63dt"
  ,"khhr72dt"
  ,"phhr61dt"
  ,"Lao People's Democratic Republic_LSIS_Datasets"
  ,"Thailand_MICS4_Datasets"
  ,"Viet Nam_MICS5_Datasets"
)

asean.r20 <- c(
  0.2911
  ,0.0979
  ,0.2693
  ,0.3386
  ,0.0033
  ,0.0857
)

asean.p20.df <- data.frame(filename,asean.r20)
povcalcuts <- subset(povcalcuts,filename %in% asean)
povcalcuts <- merge(povcalcuts,asean.p20.df,all.x=TRUE)

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

psum <- function(...,na.rm=TRUE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm)
} 

####Run function####
# set our working directory, change this if using on another machine
wd <- "C:/Users/Alex/Documents/Data/P20/DHS/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
rdatas <- list.files(path=wd,pattern="*.RData",full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 1:length(rdatas)){
  rdata <- rdatas[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(rdata),1,2))
  recode <- tolower(substr(basename(rdata),3,4))
  phase <- tolower(substr(basename(rdata),5,6))
  povcal_filename <- paste0(country,recode,phase,"dt")
  if(povcal_filename %in% povcalcuts$filename){
    message(povcal_filename)
    # load(rdata)
    # hr <- data
    # remove(data)
    
    ###Maternal
    ir_path <- paste0(country,"ir",phase,"fl.RData")
    load(ir_path)
    ir <- data.frame(data)
    remove(data)
    
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    names(ir)[which(names(ir)=="v003")] <- "line"
    names(ir)[which(names(ir)=="v201")] <- "ceb"
    ir$woman.weights <- ir$v005/1000000
    if(typeof(ir$v206)=="NULL" & typeof(ir$v207)=="NULL"){
      ir$cdead <- NA
    }else{
      ir$cdead <- psum(ir$v206,ir$v207,na.rm=TRUE) 
    }
    ###Skilled attendant_1
    if(typeof(ir$m3a_1)=="NULL" & 
       typeof(ir$m3b_1)=="NULL" & 
       typeof(ir$m3c_1)=="NULL" & 
       typeof(ir$m3d_1)=="NULL" & 
       typeof(ir$m3e_1)=="NULL" & 
       typeof(ir$m3f_1)=="NULL"){
      ir$skilled.attendant_1 <- NA
    }else{
      ir$skilled.attendant_1 <- psum(
        (ir$m3a_1==1 | tolower(ir$m3a_1)=="yes")
        ,(ir$m3b_1==1 | tolower(ir$m3b_1)=="yes")
        ,(ir$m3c_1==1 | tolower(ir$m3c_1)=="yes")
        ,(ir$m3d_1==1 | tolower(ir$m3d_1)=="yes")
        ,(ir$m3e_1==1 | tolower(ir$m3e_1)=="yes")
        ,(ir$m3f_1==1 | tolower(ir$m3f_1)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_1 <- 6-psum(
        typeof(ir$m3a_1)=="NULL" | sum(is.na(ir$m3a_1))==nrow(ir),
        typeof(ir$m3b_1)=="NULL" | sum(is.na(ir$m3b_1))==nrow(ir),
        typeof(ir$m3c_1)=="NULL" | sum(is.na(ir$m3c_1))==nrow(ir),
        typeof(ir$m3d_1)=="NULL" | sum(is.na(ir$m3d_1))==nrow(ir),
        typeof(ir$m3e_1)=="NULL" | sum(is.na(ir$m3e_1))==nrow(ir),
        typeof(ir$m3f_1)=="NULL" | sum(is.na(ir$m3f_1))==nrow(ir)
      )
      if(skilled.sum_1>0){
        ir$skilled.attendant.missing_1 <-  psum(
          is.na(ir$m3a_1)
          ,is.na(ir$m3b_1)
          ,is.na(ir$m3c_1)
          ,is.na(ir$m3d_1)
          ,is.na(ir$m3e_1)
          ,is.na(ir$m3f_1)
        )>skilled.sum_1
        ir$skilled.attendant_1[which(ir$skilled.attendant.missing_1==TRUE)] <- NA
      }else{
        ir$skilled.attendant_1 <- NA
      }
      ir$skilled.attendant_1[which(ir$b8_01>5)] <- NA
    }
    ###Skilled attendant_2
    if(typeof(ir$m3a_2)=="NULL" & 
       typeof(ir$m3b_2)=="NULL" & 
       typeof(ir$m3c_2)=="NULL" & 
       typeof(ir$m3d_2)=="NULL" & 
       typeof(ir$m3e_2)=="NULL" & 
       typeof(ir$m3f_2)=="NULL"){
      ir$skilled.attendant_2 <- NA
    }else{
      ir$skilled.attendant_2 <- psum(
        (ir$m3a_2==1 | tolower(ir$m3a_2)=="yes")
        ,(ir$m3b_2==1 | tolower(ir$m3b_2)=="yes")
        ,(ir$m3c_2==1 | tolower(ir$m3c_2)=="yes")
        ,(ir$m3d_2==1 | tolower(ir$m3d_2)=="yes")
        ,(ir$m3e_2==1 | tolower(ir$m3e_2)=="yes")
        ,(ir$m3f_2==1 | tolower(ir$m3f_2)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_2 <- 6-psum(
        typeof(ir$m3a_2)=="NULL" | sum(is.na(ir$m3a_2))==nrow(ir),
        typeof(ir$m3b_2)=="NULL" | sum(is.na(ir$m3b_2))==nrow(ir),
        typeof(ir$m3c_2)=="NULL" | sum(is.na(ir$m3c_2))==nrow(ir),
        typeof(ir$m3d_2)=="NULL" | sum(is.na(ir$m3d_2))==nrow(ir),
        typeof(ir$m3e_2)=="NULL" | sum(is.na(ir$m3e_2))==nrow(ir),
        typeof(ir$m3f_2)=="NULL" | sum(is.na(ir$m3f_2))==nrow(ir)
      )
      if(skilled.sum_2>0){
        ir$skilled.attendant.missing_2 <-  psum(
          is.na(ir$m3a_2)
          ,is.na(ir$m3b_2)
          ,is.na(ir$m3c_2)
          ,is.na(ir$m3d_2)
          ,is.na(ir$m3e_2)
          ,is.na(ir$m3f_2)
        )>skilled.sum_2
        ir$skilled.attendant_2[which(ir$skilled.attendant.missing_2==TRUE)] <- NA
      }else{
        ir$skilled.attendant_2 <- NA
      }
      ir$skilled.attendant_2[which(ir$b8_02>5)] <- NA
    }
    ###Skilled attendant_3
    if(typeof(ir$m3a_3)=="NULL" & 
       typeof(ir$m3b_3)=="NULL" & 
       typeof(ir$m3c_3)=="NULL" & 
       typeof(ir$m3d_3)=="NULL" & 
       typeof(ir$m3e_3)=="NULL" & 
       typeof(ir$m3f_3)=="NULL"){
      ir$skilled.attendant_3 <- NA
    }else{
      ir$skilled.attendant_3 <- psum(
        (ir$m3a_3==1 | tolower(ir$m3a_3)=="yes")
        ,(ir$m3b_3==1 | tolower(ir$m3b_3)=="yes")
        ,(ir$m3c_3==1 | tolower(ir$m3c_3)=="yes")
        ,(ir$m3d_3==1 | tolower(ir$m3d_3)=="yes")
        ,(ir$m3e_3==1 | tolower(ir$m3e_3)=="yes")
        ,(ir$m3f_3==1 | tolower(ir$m3f_3)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_3 <- 6-psum(
        typeof(ir$m3a_3)=="NULL" | sum(is.na(ir$m3a_3))==nrow(ir),
        typeof(ir$m3b_3)=="NULL" | sum(is.na(ir$m3b_3))==nrow(ir),
        typeof(ir$m3c_3)=="NULL" | sum(is.na(ir$m3c_3))==nrow(ir),
        typeof(ir$m3d_3)=="NULL" | sum(is.na(ir$m3d_3))==nrow(ir),
        typeof(ir$m3e_3)=="NULL" | sum(is.na(ir$m3e_3))==nrow(ir),
        typeof(ir$m3f_3)=="NULL" | sum(is.na(ir$m3f_3))==nrow(ir)
      )
      if(skilled.sum_3>0){
        ir$skilled.attendant.missing_3 <-  psum(
          is.na(ir$m3a_3)
          ,is.na(ir$m3b_3)
          ,is.na(ir$m3c_3)
          ,is.na(ir$m3d_3)
          ,is.na(ir$m3e_3)
          ,is.na(ir$m3f_3)
        )>skilled.sum_3
        ir$skilled.attendant_3[which(ir$skilled.attendant.missing_3==TRUE)] <- NA
      }else{
        ir$skilled.attendant_3 <- NA
      }
      ir$skilled.attendant_3[which(ir$b8_03>5)] <- NA
    }
    ###Skilled attendant_4
    if(typeof(ir$m3a_4)=="NULL" & 
       typeof(ir$m3b_4)=="NULL" & 
       typeof(ir$m3c_4)=="NULL" & 
       typeof(ir$m3d_4)=="NULL" & 
       typeof(ir$m3e_4)=="NULL" & 
       typeof(ir$m3f_4)=="NULL"){
      ir$skilled.attendant_4 <- NA
    }else{
      ir$skilled.attendant_4 <- psum(
        (ir$m3a_4==1 | tolower(ir$m3a_4)=="yes")
        ,(ir$m3b_4==1 | tolower(ir$m3b_4)=="yes")
        ,(ir$m3c_4==1 | tolower(ir$m3c_4)=="yes")
        ,(ir$m3d_4==1 | tolower(ir$m3d_4)=="yes")
        ,(ir$m3e_4==1 | tolower(ir$m3e_4)=="yes")
        ,(ir$m3f_4==1 | tolower(ir$m3f_4)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_4 <- 6-psum(
        typeof(ir$m3a_4)=="NULL" | sum(is.na(ir$m3a_4))==nrow(ir),
        typeof(ir$m3b_4)=="NULL" | sum(is.na(ir$m3b_4))==nrow(ir),
        typeof(ir$m3c_4)=="NULL" | sum(is.na(ir$m3c_4))==nrow(ir),
        typeof(ir$m3d_4)=="NULL" | sum(is.na(ir$m3d_4))==nrow(ir),
        typeof(ir$m3e_4)=="NULL" | sum(is.na(ir$m3e_4))==nrow(ir),
        typeof(ir$m3f_4)=="NULL" | sum(is.na(ir$m3f_4))==nrow(ir)
      )
      if(skilled.sum_4>0){
        ir$skilled.attendant.missing_4 <-  psum(
          is.na(ir$m3a_4)
          ,is.na(ir$m3b_4)
          ,is.na(ir$m3c_4)
          ,is.na(ir$m3d_4)
          ,is.na(ir$m3e_4)
          ,is.na(ir$m3f_4)
        )>skilled.sum_4
        ir$skilled.attendant_4[which(ir$skilled.attendant.missing_4==TRUE)] <- NA
      }else{
        ir$skilled.attendant_4 <- NA
      }
      ir$skilled.attendant_4[which(ir$b8_04>5)] <- NA
    }
    ###Skilled attendant_5
    if(typeof(ir$m3a_5)=="NULL" & 
       typeof(ir$m3b_5)=="NULL" & 
       typeof(ir$m3c_5)=="NULL" & 
       typeof(ir$m3d_5)=="NULL" & 
       typeof(ir$m3e_5)=="NULL" & 
       typeof(ir$m3f_5)=="NULL"){
      ir$skilled.attendant_5 <- NA
    }else{
      ir$skilled.attendant_5 <- psum(
        (ir$m3a_5==1 | tolower(ir$m3a_5)=="yes")
        ,(ir$m3b_5==1 | tolower(ir$m3b_5)=="yes")
        ,(ir$m3c_5==1 | tolower(ir$m3c_5)=="yes")
        ,(ir$m3d_5==1 | tolower(ir$m3d_5)=="yes")
        ,(ir$m3e_5==1 | tolower(ir$m3e_5)=="yes")
        ,(ir$m3f_5==1 | tolower(ir$m3f_5)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_5 <- 6-psum(
        typeof(ir$m3a_5)=="NULL" | sum(is.na(ir$m3a_5))==nrow(ir),
        typeof(ir$m3b_5)=="NULL" | sum(is.na(ir$m3b_5))==nrow(ir),
        typeof(ir$m3c_5)=="NULL" | sum(is.na(ir$m3c_5))==nrow(ir),
        typeof(ir$m3d_5)=="NULL" | sum(is.na(ir$m3d_5))==nrow(ir),
        typeof(ir$m3e_5)=="NULL" | sum(is.na(ir$m3e_5))==nrow(ir),
        typeof(ir$m3f_5)=="NULL" | sum(is.na(ir$m3f_5))==nrow(ir)
      )
      if(skilled.sum_5>0){
        ir$skilled.attendant.missing_5 <-  psum(
          is.na(ir$m3a_5)
          ,is.na(ir$m3b_5)
          ,is.na(ir$m3c_5)
          ,is.na(ir$m3d_5)
          ,is.na(ir$m3e_5)
          ,is.na(ir$m3f_5)
        )>skilled.sum_5
        ir$skilled.attendant_5[which(ir$skilled.attendant.missing_5==TRUE)] <- NA
      }else{
        ir$skilled.attendant_5 <- NA
      }
      ir$skilled.attendant_5[which(ir$b8_05>5)] <- NA
    }
    ###Skilled attendant_6
    if(typeof(ir$m3a_6)=="NULL" & 
       typeof(ir$m3b_6)=="NULL" & 
       typeof(ir$m3c_6)=="NULL" & 
       typeof(ir$m3d_6)=="NULL" & 
       typeof(ir$m3e_6)=="NULL" & 
       typeof(ir$m3f_6)=="NULL"){
      ir$skilled.attendant_6 <- NA
    }else{
      ir$skilled.attendant_6 <- psum(
        (ir$m3a_6==1 | tolower(ir$m3a_6)=="yes")
        ,(ir$m3b_6==1 | tolower(ir$m3b_6)=="yes")
        ,(ir$m3c_6==1 | tolower(ir$m3c_6)=="yes")
        ,(ir$m3d_6==1 | tolower(ir$m3d_6)=="yes")
        ,(ir$m3e_6==1 | tolower(ir$m3e_6)=="yes")
        ,(ir$m3f_6==1 | tolower(ir$m3f_6)=="yes")
        ,na.rm=TRUE
      )>=1
      skilled.sum_6 <- 6-psum(
        typeof(ir$m3a_6)=="NULL" | sum(is.na(ir$m3a_6))==nrow(ir),
        typeof(ir$m3b_6)=="NULL" | sum(is.na(ir$m3b_6))==nrow(ir),
        typeof(ir$m3c_6)=="NULL" | sum(is.na(ir$m3c_6))==nrow(ir),
        typeof(ir$m3d_6)=="NULL" | sum(is.na(ir$m3d_6))==nrow(ir),
        typeof(ir$m3e_6)=="NULL" | sum(is.na(ir$m3e_6))==nrow(ir),
        typeof(ir$m3f_6)=="NULL" | sum(is.na(ir$m3f_6))==nrow(ir)
      )
      if(skilled.sum_6>0){
        ir$skilled.attendant.missing_6 <-  psum(
          is.na(ir$m3a_6)
          ,is.na(ir$m3b_6)
          ,is.na(ir$m3c_6)
          ,is.na(ir$m3d_6)
          ,is.na(ir$m3e_6)
          ,is.na(ir$m3f_6)
        )>skilled.sum_6
        ir$skilled.attendant_6[which(ir$skilled.attendant.missing_6==TRUE)] <- NA
      }else{
        ir$skilled.attendant_6 <- NA
      }
      ir$skilled.attendant_6[which(ir$b8_06>5)] <- NA
    }
    ir$all.births <- psum(!is.na(ir$skilled.attendant_1),!is.na(ir$skilled.attendant_2),!is.na(ir$skilled.attendant_3),!is.na(ir$skilled.attendant_4)
                          ,!is.na(ir$skilled.attendant_5),!is.na(ir$skilled.attendant_6))
    ir$skilled.births <- psum(ir$skilled.attendant_1,ir$skilled.attendant_2,ir$skilled.attendant_3,ir$skilled.attendant_4
                              ,ir$skilled.attendant_5,ir$skilled.attendant_6,na.rm=TRUE)
    maternal.deaths <- function(df){
      maternal.deathsV <- c()
      for(i in 1:nrow(df)){
        maternal.deaths <- 0
        for(j in 1:20){
          num <- sprintf("%02d", j)
          pregVar <- paste0("mm9_",num)
          if(typeof(df[[pregVar]])!="NULL"){
            preg <- df[[pregVar]][i]
            if(!is.na(preg)){
              if(preg==2 |
                 preg==3 |
                 preg==5 |
                 preg==6 |
                 tolower(preg)=="died while pregnant" |
                 tolower(preg)=="died during delivery" |
                 tolower(preg)=="6 weeks after delivery" |
                 tolower(preg)=="2 months after delivery"
              ){
                maternal.deaths <- maternal.deaths + 1
              }
            }
          }else{
            next;
          }
        }
        maternal.deathsV <- c(maternal.deathsV,maternal.deaths)
      }
      return(maternal.deathsV)
    }
    ir$maternal.deaths <- maternal.deaths(ir)
    maternal.sum <- 20-psum(
      typeof(ir$mm9_01)=="NULL" | sum(is.na(ir$mm9_01))==nrow(ir),
      typeof(ir$mm9_02)=="NULL" | sum(is.na(ir$mm9_02))==nrow(ir),
      typeof(ir$mm9_03)=="NULL" | sum(is.na(ir$mm9_03))==nrow(ir),
      typeof(ir$mm9_04)=="NULL" | sum(is.na(ir$mm9_04))==nrow(ir),
      typeof(ir$mm9_05)=="NULL" | sum(is.na(ir$mm9_05))==nrow(ir),
      typeof(ir$mm9_06)=="NULL" | sum(is.na(ir$mm9_06))==nrow(ir),
      typeof(ir$mm9_07)=="NULL" | sum(is.na(ir$mm9_07))==nrow(ir),
      typeof(ir$mm9_08)=="NULL" | sum(is.na(ir$mm9_08))==nrow(ir),
      typeof(ir$mm9_09)=="NULL" | sum(is.na(ir$mm9_09))==nrow(ir),
      typeof(ir$mm9_10)=="NULL" | sum(is.na(ir$mm9_10))==nrow(ir),
      typeof(ir$mm9_11)=="NULL" | sum(is.na(ir$mm9_11))==nrow(ir),
      typeof(ir$mm9_12)=="NULL" | sum(is.na(ir$mm9_12))==nrow(ir),
      typeof(ir$mm9_13)=="NULL" | sum(is.na(ir$mm9_13))==nrow(ir),
      typeof(ir$mm9_14)=="NULL" | sum(is.na(ir$mm9_14))==nrow(ir),
      typeof(ir$mm9_15)=="NULL" | sum(is.na(ir$mm9_15))==nrow(ir),
      typeof(ir$mm9_16)=="NULL" | sum(is.na(ir$mm9_16))==nrow(ir),
      typeof(ir$mm9_17)=="NULL" | sum(is.na(ir$mm9_17))==nrow(ir),
      typeof(ir$mm9_18)=="NULL" | sum(is.na(ir$mm9_18))==nrow(ir),
      typeof(ir$mm9_19)=="NULL" | sum(is.na(ir$mm9_19))==nrow(ir),
      typeof(ir$mm9_20)=="NULL" | sum(is.na(ir$mm9_20))==nrow(ir)
    )
    if(maternal.sum>0){
      ir$maternal.missing <- psum(
        is.na(ir$mm9_01),is.na(ir$mm9_02),is.na(ir$mm9_03),is.na(ir$mm9_04),is.na(ir$mm9_05),is.na(ir$mm9_06),is.na(ir$mm9_07),is.na(ir$mm9_08),
        is.na(ir$mm9_09),is.na(ir$mm9_10),is.na(ir$mm9_11),is.na(ir$mm9_12),is.na(ir$mm9_13),is.na(ir$mm9_14),is.na(ir$mm9_15),is.na(ir$mm9_16),
        is.na(ir$mm9_17),is.na(ir$mm9_18),is.na(ir$mm9_19),is.na(ir$mm9_20)
      )>=maternal.sum
      ir$maternal.deaths[which(ir$maternal.missing==TRUE)] <- NA
    }else{
      ir$maternal.deaths <- NA
    }
    
    irKeep <- c(
      "cluster"
      ,"household"
      ,"line"
      ,"ceb"
      ,"cdead"
      ,"all.births"
      ,"skilled.births"
      ,"maternal.deaths"
      ,"woman.weights"
    )
    irNames <- names(ir)
    namesDiff <- setdiff(irKeep,irNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ir[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    ir <- ir[irKeep]
    
    hr_path <- paste0(country,"hr",phase,"fl.RData")
    load(hr_path)
    hr <- data.frame(data)
    remove(data)
    
    #Rename fridge var
    names(hr)[which(names(hr)=="hv206")] <- "electricity"
    names(hr)[which(names(hr)=="hv209")] <- "fridge"
    names(hr)[which(names(hr)=="hv001")] <- "cluster"
    names(hr)[which(names(hr)=="hv002")] <- "household"
    
    keep <- c("electricity","fridge","cluster","household")
    hr <- hr[keep]
    
    pr_path <- paste0(country,"pr",phase,"fl.RData")
    load(pr_path)
    pr <- data.frame(data)
    remove(data)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    pr$urban <- NA
    pr$urban[which(pr$urban.rural==1)] <- 1
    pr$urban[which(pr$urban.rural==2)] <- 0
    
    #Toilets
    improved <- c(10,11,12,21,22,41)
    unimproved <- c(13,14,15,20,23,30,31,42,43,51,96)
    pr$toilet.unimproved <- NA
    pr$toilet.unimproved[which(pr$hv205 %in% improved )] <- 0
    pr$toilet.unimproved[which(pr$hv205 %in% unimproved)] <- 1
    if(typeof(pr$hv225)!="NULL" & sum(is.na(pr$hv225))!=nrow(pr)){
      message(paste(country,"has share var"))
      pr$toilet.unimproved[which(pr$hv225==1)] <- 1
    }
    
    #Rename educ var
    names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(x==8 | x==9){return(NA)}
      else if(x==0 | x==1){return("No education, preschool")}
      else if(x==2 | x==3 ){return("Primary")}
      else if(x==4){return("Secondary")}
      else if(x==5){return("Higher")}
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
    names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    #Join hr
    pr <- merge(pr,hr,by=c("cluster","household"),all.x=TRUE)
    
    #Join IR
    pr <- merge(pr,ir,by=c("cluster","household","line"),all.x=TRUE)
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    ###Registration for Togo
    if(country=="tg"){
      pr$birth.cert <- NULL
      kr_path <- paste0(country,"kr",phase,"fl.RData")
      load(kr_path)
      kr <- data.frame(data)
      remove(data)
      kr$line <- kr$s225c
      kr$cluster <- kr$v001
      kr$household <- kr$v002
      kr$birth.cert <- NA
      kr$birth.cert[which(kr$s225e==1)] <- "yes, birth certificate seen"
      kr$birth.cert[which(kr$s225e==2)] <- "yes, birth certificate not seen"
      kr$birth.cert[which(kr$s225e==3)] <- "no"
      kr$birth.cert[which(kr$s225e==8)] <- "no"
      kr$birth.cert[which(is.na(kr$s225e))] <- "no"
      
      kr$birth.reg <- NA
      kr$birth.reg[which(kr$s225f==0)] <- "no"
      kr$birth.reg[which(kr$s225f==1)] <- "yes"
      kr$birth.reg[which(kr$s225f==8)] <- "no"
      kr$birth.reg[which(is.na(kr$s225f))] <- "no"
      #Registered by virtue of having certificate
      kr$birth.reg[which(kr$s225e==1)] <- "yes"
      kr$birth.reg[which(kr$s225e==2)] <- "yes"
      keep <- c("cluster","household","line","birth.reg","birth.cert")
      pr <- merge(pr,kr,by=c("cluster","household","line"),all.x=TRUE)
    }
    
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
    names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.weights <- pr$weights
    
    povcalcut <- subset(povcalcuts,filename==povcal_filename)$asean.r20
    np20cut <- 0.2
    extcut <- subset(povcalcuts,filename==povcal_filename)$extreme
    cuts <- c(povcalcut,np20cut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$asean.r20 <- (pr$wealth < povperc[1])
    pr$np20 <- (pr$wealth < povperc[2])
    pr$ext <- (pr$wealth < povperc[3])
    
    mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
    mothers <- mothers[complete.cases(mothers),]
    names(mothers) <- c("cluster","household","mother.line","mother.bmi")
    pr <- join(
      pr
      ,mothers
      ,by=c("cluster","household","mother.line")
    )
    
    keep <- c("wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","np20","asean.r20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","ext","all.births","skilled.births","maternal.deaths","woman.weights"
              ,"toilet.unimproved", "fridge", "electricity"
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
    data$filename <- povcal_filename
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

setwd("C:/Users/Alex/Documents/Data/P20/Meta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")
maternal.varNames <- read.csv("mics_child_vars.csv",as.is=TRUE,na.strings="")

recode.birth.vars <- function(x,skilled){
  # if(is.factor(x)){
  #   str <- trimws(tolower(unfactor(x)))
  # }else{
  str <- trimws(tolower(x))
  # }
  
  if(is.na(str)){
    return(NA)
  }else if(str=="" | str==9 | str=="?"){
    return(NA)
  }else if(str=="missing"){
    return(FALSE)
  }else if(!skilled){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

code.skilled <- function(skilled.column,skilled=TRUE){
  sapply(skilled.column,recode.birth.vars,skilled)
}

recode.asset <- function(xV,x1V=rep(NA),x2V=rep(NA)){
  result <- c()
  for(i in 1:length(xV)){
    x <- tolower(xV[i])
    x1 <- tolower(x1V[i])
    if(length(x1)<=0){x1 = rep(NA)}
    x2 <- tolower(x2V[i])
    if(length(x2)<=0){x2 = rep(NA)}
    
    if(x %in% missing.vals & x1 %in% missing.vals & x2 %in% missing.vals){
      result <- c(result,NA)
    }else{
      result <- c(result,min(sum(x %in% yes.vals,x1 %in% yes.vals,x2 %in% yes.vals,na.rm=TRUE),1))
    }
  }
  return(result)
}

wd <- "C:/Users/Alex/Documents/Data/P20/MICS"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "C:/Users/Alex/Documents/Data/P20/MICS/Algeria_MICS4_Datasets"

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
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    if(exists("ch")){rm(ch)}
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    load(paste0(dir,"/","ch.RData"))
    load(paste0(dir,"/","wm.RData"))
    hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    ch <- data.frame(ch,as.is=TRUE,check.names=FALSE)
    wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    names(ch) <- tolower(names(ch))
    names(wm) <- tolower(names(wm))
    
    file.varName <- subset(varNames,filename==hrBase)
    
    attendedVar <- subset(file.varName,match=="attended")$varName
    gradeVar <- subset(file.varName,match=="grade")$varName
    schoolVar <- subset(file.varName,match=="school")$varName
    
    share.toiletsVar <- subset(file.varName,match=="share.toilets")$varName
    toiletsVar <- subset(file.varName,match=="toilets")$varName
    fridgeVar <- subset(file.varName,match=="fridge")$varName
    
    ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
    attended.classes <- subset(classes,filename==hrBase & type=="attended")
    urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
    school.classes <- subset(classes,filename==hrBase & type=="school")
    
    missing.vals <- subset(ynm.classes,is.na(ynm))$value
    no.vals <- subset(ynm.classes,ynm==0)$value
    yes.vals <- subset(ynm.classes,ynm==1)$value
    
    missing.attended <- subset(attended.classes,is.na(attended))$value
    no.attended <- subset(attended.classes,attended==0)$value
    yes.attended <- subset(attended.classes,attended==1)$value
    
    missing.level <- subset(school.classes,is.na(level))$value
    none.level <- subset(school.classes,level=="none")$value
    preschool.level <- subset(school.classes,level=="preschool")$value
    primary.level <- subset(school.classes,level=="primary")$value
    secondary.level <- subset(school.classes,level=="secondary")$value
    higher.level <- subset(school.classes,level=="higher")$value
    
    skilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==1 & filename==hrBase)$var
    unskilledBirthVars <- subset(maternal.varNames,match=="attendant" &  skilled==0 & filename==hrBase)$var
    
    toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
    
    #maternal
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "line"
    names(wm)[which(names(wm)=="wmweight")] <- "woman.weights"
    
    wm$skilled.attendant <- NA
    for(var in skilledBirthVars){
      message(var)
      wm$skilled.attendant <- wm$skilled.attendant | code.skilled(wm[,var])
    }
    wm$unskilled.attendant <- NA
    for(var in unskilledBirthVars){
      message(var)
      wm$unskilled.attendant <- wm$unskilled.attendant & code.skilled(wm[,var],skilled=FALSE)
    }
    wm$skilled.births <- NA
    wm$skilled.births[which(wm$skilled.attendant==TRUE)] <- 1
    wm$skilled.births[which(wm$unskilled.attendant==FALSE)] <- 0
    wm$all.births <- NA
    wm$all.births[which(wm$skilled.attendant==TRUE)] <- 1
    wm$all.births[which(wm$unskilled.attendant==FALSE)] <- 1
    
    #Rename wealth var
    if(typeof(hh$wlthscor)=="NULL" | typeof(hh$wlthscor)=="logical" | length(hh$wlthscor[which(!is.na(hh$wlthscor))])==0){
      if(typeof(hh$wscore)=="NULL" | typeof(hh$wscore)=="logical" | length(hh$wscore[which(!is.na(hh$wscore))])==0){
        message("Wealth missing!");
        if(hrBase=="Djibouti MICS 2006 SPSS Datasets"){
          message("Building wealth index")
        }else{
          return(NA)
        }
      }else{
        names(hh)[which(names(hh)=="wscore")] <- "wealth"
      }
    }else{
      names(hh)[which(names(hh)=="wlthscor")] <- "wealth"
    }
    
    #Rename sample.weights var
    names(hh)[which(names(hh)=="hhweight")] <- "weights"
    
    #Rename urban var
    names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
    if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
    
    #Rename educ var
    names(hl)[which(names(hl)==attendedVar)] <- "attended"
    names(hl)[which(names(hl)==schoolVar)] <- "school"
    names(hl)[which(names(hl)==gradeVar)] <- "grade"
    
    #Rename age var
    if(max(hl$hl6,na.rm=TRUE)<45){
      names(hl)[which(names(hl)=="hl5")] <- "age"
    }else{
      names(hl)[which(names(hl)=="hl6")] <- "age"
    }
    
    
    #Rename sex var
    names(hl)[which(names(hl)=="hl4")] <- "sex"
    
    #Rename head var
    hl$head <- tolower(substr(hl$hl3,1,4)) %in% c("chef","head")
    
    #Rename child vars
    names(ch)[which(names(ch)=="br1")] <- "birth.cert"
    names(ch)[which(names(ch)=="br2")] <- "birth.reg"
    names(ch)[which(names(ch)=="cage")] <- "age.months"
    names(ch)[which(names(ch)=="chweight")] <- "child.weights"
    names(ch)[which(names(ch)=="an3")] <- "weight.kg"
    names(ch)[which(names(ch)=="an4a")] <- "standing.lying"
    names(ch)[which(names(ch)=="haz2")] <- "child.height.age"
    
    if(hrBase %in% c("Guinea Bissau_MICS5_Datasets","Vanuatu MICS 2007 SPSS Datasets","Sao Tome and Principe_MICS5_Datasets") ){
      ch$birth.reg.coded <- 0
      ch$birth.reg.coded[which(ch$birth.cert %in% c(1,2))] <- 1
      ch$birth.reg.coded[which(ch$birth.reg %in% c(1))] <- 1
      ch$birth.reg <- ch$birth.reg.coded
    }
    if(hrBase=="Somalia MICS 2006 SPSS Datasets"){
      ch$birth.reg <- 0
      # ch$birth.reg[which(is.na(ch$br2a))] <- NA
      ch$birth.reg[which(ch$birth.cert %in% c(1,2))] <- 1
      ch$birth.reg[which(ch$br2a %in% c(1,2,3))] <- 1
    }
    
    #code female bmi
    if(typeof(wm$anw4)!="NULL" & typeof(wm$anw5)!="NULL"){
      wm$anw4[which(wm$anw4==99.9)] <- NA
      wm$anw5[which(wm$anw5==999.9)] <- NA
      wm$anw5 <- wm$anw5/100
      wm$woman.bmi <- wm$anw4/(wm$anw5*wm$anw5)
    }
    
    #Rename cluster/hh var
    names(hl)[which(names(hl)=="hh1")] <- "cluster"
    names(hl)[which(names(hl)=="hh2")] <- "household"
    names(hl)[which(names(hl)=="hl1")] <- "line"
    names(hl)[which(names(hl)=="ln")] <- "line"
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    names(hh)[which(names(hh)=="hh7")] <- "region"
    names(ch)[which(names(ch)=="hh1")] <- "cluster"
    names(ch)[which(names(ch)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="ln")] <- "line"
    names(ch)[which(names(ch)=="uf6")] <- "mother.line"
    
    recode.educ <- function(attendedV,schoolV,gradeV){
      educV <- c()
      for(i in 1:length(attendedV)){
        attended <- tolower(attendedV[i])
        school <- tolower(schoolV[i])
        if(length(school)<=0){
          school <- NA
        }
        grade <- gradeV[i]
        ###Ignore factor grades for now... We need to code these out in the metavars
        if(is.factor(grade)){
          grade <- NA
        }
        if(!is.na(grade)){
          if(grade>90){grade<-NA}
        }
        if(attended %in% missing.attended){
          if(school %in% missing.level){
            if(is.na(grade)){
              #missing all three
              educ <- NA
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else{
            #missing attended, but not level
            if(is.na(grade)){
              #has level, but not grade
              if(school %in% preschool.level | school %in% none.level){
                educ <- 0
              }else if(school %in% primary.level){
                educ <- 1
              }else if(school %in% secondary.level){
                educ <- 2
              }else if(school %in% higher.level){
                educ <- 3
              }else{
                educ <- NA
              }
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }
        }else if(attended %in% no.attended){
          #No education
          educ <- 0
        }else{
          if(school %in% missing.level){
            if(is.na(grade)){
              #has attended, but has no level or grade
              educ <- NA
            }else{
              #has attended, missing level, but not missing grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else if(school %in% preschool.level | school %in% none.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- 0
            }
          } else if(school %in% primary.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade<5){
              #Incomplete primary
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- NA
            }
          } else if(school %in% secondary.level){
            #(in)complete secondary
            educ <- 2
          } else if(school %in% higher.level){
            #(in)complete higher
            educ <- 3
          }else if(grade>=5 & grade<7){
            educ <- 1
          }else if(grade>=7 & grade<9){
            educ <- 2
          }else if(grade>9){
            educ <- 3
          }else if(grade<5){
            #not at least 5 years of some other schooling
            educ <- 0
          } else{
            #missing grade with preschool, primary, or other
            educ <- NA
          }
        }
        educV <- c(educV,educ)
      }
      return(educV)
    }
    
    hl$educ <- recode.educ(hl$attended,hl$school,hl$grade)
    
    head <- subset(hl,head==1)
    names(head)[which(names(head)=="sex")] <- "head.sex"
    names(head)[which(names(head)=="age")] <- "head.age"
    keep <- c("cluster","household","head.sex","head.age")
    head <- head[keep]
    hh <- join(
      hh
      ,head
      ,by=c("cluster","household")
    )
    
    recode.urban.rural <- function(x){
      item <- subset(urban.rural.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$urban[1]}
    }
    hh$urban <- sapply(hh$urban.rural,recode.urban.rural)
    
    if(hrBase=="Djibouti MICS 2006 SPSS Datasets"){
      #Make a wealth index
      source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")
      
      catvars = c(
        "ws1" #Drinking water
        ,"ws2" #Other water
        ,"ws5" #Water treated
        ,"ws8" #Toilet type
        ,"ws9" #Toilet shared
        ,"hc3" #Floor
        ,"hc4" #Roof
        ,"hc5" #Wall
        ,"hc6" #Fuel
        ,"hc9a" #Assets
        ,"hc9b"
        ,"hc9c"
        ,"hc9d"
        ,"hc9e"
        ,"hc9f"
        ,"hc10a"
        ,"hc10b"
        ,"hc10c"
        ,"hc10d"
        ,"hc10e"
        ,"hc10f"
        
      )
      
      numvars = c(
        "ws3" #Time to fetch water
        ,"hc2" #sleeping rooms
      )
      
      urbanvar = "urban"
      hh <- data.frame(hh)
      
      hh$missingCount = rowSums(is.na(hh[c(catvars,numvars)]))
      hh.wealth <- hh
      # hh.wealth = subset(hh,missingCount<length(c(catvars,numvars)))
      
      hh.wealth <- wealth(hh.wealth,catvars,numvars,urbanvar)
      keep <- c("cluster","household","wealth")
      hh.wealth <- hh.wealth[keep]
      hh.wealth$wealth <- hh.wealth$wealth*-1
      hh <- merge(hh,hh.wealth,all.x=TRUE)
    }
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$asean.r20
    np20cut <- 0.2
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,np20cut,extcut)
    povperc <- weighted.percentile(hh$wealth,hh$weights,prob=cuts)
    
    hh$asean.r20 <- (hh$wealth < povperc[1])
    hh$np20 <- (hh$wealth < povperc[2])
    hh$ext <- (hh$wealth < povperc[3])
    
    #check fridge var
    if(length(fridgeVar)<=0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
    if(!(fridge.missing)){
      hh$fridge <- recode.asset(hh[[fridgeVar[1]]],hh[[fridgeVar[2]]],hh[[fridgeVar[3]]])
    }
    #Rename toilets var
    names(hh)[which(names(hh)==toiletsVar)] <- "toilets"
    if(typeof(hh$toilets)=="NULL"){message("No toilets!");hh$toilets<-NA}
    #Rename share toilets var
    names(hh)[which(names(hh)==share.toiletsVar)] <- "share.toilets"
    if(typeof(hh$share.toilets)=="NULL" | typeof(hh$share.toilets)=="logical" | length(hh$share.toilets[which(!is.na(hh$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
    code.toilets <- function(toiletsV,share.toiletsV,share.toilets.missing){
      inade.toilets <- c()
      for(i in 1:length(toiletsV)){
        toilets <- tolower(toiletsV[i])
        share.toilets <- tolower(share.toiletsV[i])
        item <- subset(toilets.classes,value==toilets)
        if(share.toilets.missing){
          share.toilets = 0
        }
        if(is.na(share.toilets)){
          share.toilets = 0
        }
        if(share.toilets %in% missing.vals){
          share.toilets = 0
        }
        if(share.toilets %in% yes.vals){
          inade.toilet = 1
        }else{
          inade.toilet = item$inadequate[1]
        }
        inade.toilets <- c(inade.toilets,inade.toilet)
      }
      return(inade.toilets)
    }
    hh$toilet.unimproved <- code.toilets(hh$toilets,hh$share.toilets,share.toilets.missing)
    if(is.factor(hh$hc8a)){
      hh$electricity <- unfactor(hh$hc8a)
    }else{
      hh$electricity <- hh$hc8a
    }

    wmkeep <- c(
      "cluster"
      ,"household"
      ,"line"
      ,"skilled.births"
      ,"all.births"
      ,"ceb"
      ,"cdead"
      ,"woman.bmi"
      ,"woman.weights"
    )
    wmNames <- names(wm)
    namesDiff <- setdiff(wmkeep,wmNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        wm[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    wm <- wm[wmkeep]
    
    hl <- join(
      hl
      ,wm
      ,by=c("cluster","household","line")
    )
    
    names(wm) <- c(
      "cluster"
      ,"household"
      ,"mother.line"
      ,"skilled.births"
      ,"all.births"
      ,"ceb"
      ,"cdead"
      ,"mother.bmi"
      ,"woman.weights"
    )
    
    ch <- join(
      ch
      ,wm
      ,by=c("cluster","household","mother.line")
    )
    
    chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
                ,"child.height.age","mother.bmi")
    chNames <- names(ch)
    namesDiff <- setdiff(chkeep,chNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ch[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    ch <- ch[chkeep]
    
    hl <- join(
      hl
      ,ch
      ,by=c("cluster","household","line")
    )
    
    
    hhkeep <- c("wealth","weights","urban","region","cluster","household","head.sex","head.age","asean.r20","np20","ext","toilet.unimproved","fridge","electricity")
    hhNames <- names(hh)
    namesDiff <- setdiff(hhkeep,hhNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hh[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hh <- hh[hhkeep]
    hl <- join(
      hl
      ,hh
      ,by=c("cluster","household")
    )
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    keep <- c("wealth","weights","urban","region","educ","age","sex","cluster","household","head.sex","head.age","asean.r20","np20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","ext","all.births","skilled.births","maternal.deaths","woman.weights"
              ,"toilet.unimproved","fridge","electricity"
    )
    hlNames <- names(hl)
    namesDiff <- setdiff(keep,hlNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hl[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hl <- hl[keep]
    hl$filename <- hrBase
    dataList[[dataIndex]] <- hl
    dataIndex <- dataIndex + 1
  }
}

data.total <- rbindlist(dataList,fill=TRUE)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data.total$educ <- sapply(data.total$educ,recode.educ)
data.total$educ <- factor(data.total$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

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

data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
data.total$ageCategory <- factor(data.total$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
data.total$head.ageCategory <- factor(data.total$head.ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#3 - registered
#6 - other
#8 - dk
# birth.cert.missing <- c(NA,"dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
# birth.cert.no <- c("registered",0,2,3,"neither certificate or registered","no","non","has only hospital card")
birth.cert.missing <- c(NA)
birth.cert.no <- c("registered",0,2,3,"neither certificate or registered","no","non","has only hospital card","dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))

birth.reg.missing <- c(NA,9)
birth.reg.no <- c("no","non",0,"dk","missing","nsp","manquant",8,2)
birth.reg.yes <- c("yes","oui",1)
#count registrations if birth.cert var reveals it to be so
birth.cert.registered <- c(1,2,3,"registered","has only hospital card",birth.cert.yes)
birth.cert.not.registered <- c(0,"neither certificate or registered","no","non","dk","don't know",6,8,9,"missing","nsp","manquant","no sabe")
if(is.factor(data.total$birth.reg)){
  data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
}else{
  data.total$birth.reg.coded <- data.total$birth.reg
}
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- as.numeric(data.total$birth.reg.coded)

if(is.factor(data.total$birth.cert)){
  data.total$birth.cert <- unfactor(data.total$birth.cert)
}
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$birth.reg[which(data.total$birth.cert==1)] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$stunting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"
# 
# data.total$stunted <- (data.total$child.height.age> -6) & (data.total$child.height.age<= -2)
# data.total$stunted <- data.total$child.height.age<= -2

data.total$stunting <- factor(data.total$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))

data.total$fridge[which(data.total$fridge==9)] <- NA
data.total$electricity[which(data.total$electricity==9)] <- NA
data.total$electricity[which(data.total$electricity=="Missing")] <- NA
data.total$electricity[which(data.total$electricity=="Yes")] <- 1
data.total$electricity[which(data.total$electricity=="No")] <- 0

wd <- "C:/Users/Alex/Documents/Data/P20/Meta"
setwd(wd)

save(data.total,file="asean_tab_data.RData")
