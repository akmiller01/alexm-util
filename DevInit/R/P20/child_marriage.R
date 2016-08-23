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

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SI.POV.NAHC"

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
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){next;}
    
    ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    #Rename sample.weights var
    names(ir)[which(names(ir)=="v005")] <- "sample.weights"
    ir$weights <- ir$sample.weights/1000000
    
    names(ir)[which(names(ir)=="v191")] <- "wealth"
    ir$wealth <- ir$wealth/100000
    
    #Rename educ var
    names(ir)[which(names(ir)=="v149")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
      else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
      else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
      else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
      else if(x==5 | tolower(x)=="higher"){return("Higher")}
      else{return(NA)}
    }
    ir$educ <- sapply(ir$educ,recode.educ)
    
    #Rename age var
    names(ir)[which(names(ir)=="v012")] <- "age"
    
    names(ir)[which(names(ir)=="v301")] <- "method"
    names(ir)[which(names(ir)=="v221")] <- "interval"
    names(ir)[which(names(ir)=="v512")] <- "cohab.age"
    names(ir)[which(names(ir)=="v613")] <- "ideal.children"
    
    #Rename cluster/hh var
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    cuts <- c(povcalcut,np20cut)
    povperc <- weighted.percentile(ir$wealth,ir$weights,prob=cuts)
    
    ir$p20 <- (ir$wealth < povperc[1])
    ir$np20 <- (ir$wealth < povperc[2])
    
    
    keep <- c("wealth","weights","educ","age","cluster","household","p20","np20"
              ,"method","interval","cohab.age","ideal.children"
    )
    irNames <- names(ir)
    namesDiff <- setdiff(keep,irNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ir[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- ir[keep]
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

data.total <- rbindlist(dataList)

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

setwd("D:/Documents/Data/MICSmeta")
cm <- data.total
save(cm,file="child.marriage.RData")

cm$cm <- cm$cohab.age<18
cm$interval[which(cm$interval==996)] <- NA
cm$small.interval <- cm$interval<=12

cm$ideal.children[which(cm$ideal.children>90)] <- NA
cm$ideal.cat <- NA
cm$ideal.cat[which(cm$ideal.children==0)] <- "0 children"
cm$ideal.cat[which(cm$ideal.children>0 & cm$ideal.children<=3)] <- "1 - 3 children"
cm$ideal.cat[which(cm$ideal.children>3 & cm$ideal.children<=6)] <- "3 - 6 children"
cm$ideal.cat[which(cm$ideal.children>6)] <- "7+ children"
cm$ideal.cat <- factor(cm$ideal.cat,levels=c("0 children","1 - 3 children","3 - 6 children","7+ children"))

library(descr)
# Stop crosstab from plotting everything
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
conform <- function(complete,incomplete){
  return(
    pmax(
      incomplete[
        match(
          rownames(complete)
          ,rownames(incomplete)
        )
        ,
        match(
          colnames(complete)
          ,colnames(incomplete)
        )
        ]
      ,0
      ,na.rm=TRUE
    )
  )
}
countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

crossTabs <- list()

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(cm,filename==this.filename)
  dat <- dat[complete.cases(dat$cm),]
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15.male <- this.pop.under5.male + subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.under15.female <- this.pop.under5.female + subset(countryMeta,filename==this.filename)$female.5.14
    this.pop.under15 <- this.pop.under15.male + this.pop.under15.female
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.over15.male <- this.pop.male - this.pop.under15.male
    this.pop.over15.female <- this.pop.female - this.pop.under15.female
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    #Educ-CM
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$cm,over25$weights,this.pop.over25)
      if(is.null(crossTabs[["over25.educcm"]])){
        crossTabs[["over25.educcm"]] <- confidence.tab
      }else{
        crossTabs[["over25.educcm"]]$low <- crossTabs[["over25.educcm"]]$low + conform(crossTabs[["over25.educcm"]]$low,confidence.tab$low)
        crossTabs[["over25.educcm"]]$estimate <- crossTabs[["over25.educcm"]]$estimate + conform(crossTabs[["over25.educcm"]]$estimate,confidence.tab$estimate)
        crossTabs[["over25.educcm"]]$high <- crossTabs[["over25.educcm"]]$high + conform(crossTabs[["over25.educcm"]]$high,confidence.tab$high)
      }  
    }
    #Age-cm
    if(length(dat$ageCategory[which(!is.na(dat$ageCategory))])!=0){
      confidence.tab <- pop.confidence(dat$ageCategory,dat$cm,dat$weights,this.pop)
      if(is.null(crossTabs[["ageCategorycm"]])){
        crossTabs[["ageCategorycm"]] <- confidence.tab
      }else{
        crossTabs[["ageCategorycm"]]$low <- crossTabs[["ageCategorycm"]]$low + conform(crossTabs[["ageCategorycm"]]$low,confidence.tab$low)
        crossTabs[["ageCategorycm"]]$estimate <- crossTabs[["ageCategorycm"]]$estimate + conform(crossTabs[["ageCategorycm"]]$estimate,confidence.tab$estimate)
        crossTabs[["ageCategorycm"]]$high <- crossTabs[["ageCategorycm"]]$high + conform(crossTabs[["ageCategorycm"]]$high,confidence.tab$high)
      }  
    }
    #Educ-ideal
    if(length(over25$educ[which(!is.na(over25$educ))])!=0 & length(over25$ideal.cat[which(!is.na(over25$ideal.cat))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$ideal.cat,over25$weights,this.pop.over25)
      if(is.null(crossTabs[["over25.educideal"]])){
        crossTabs[["over25.educideal"]] <- confidence.tab
      }else{
        crossTabs[["over25.educideal"]]$low <- crossTabs[["over25.educideal"]]$low + conform(crossTabs[["over25.educideal"]]$low,confidence.tab$low)
        crossTabs[["over25.educideal"]]$estimate <- crossTabs[["over25.educideal"]]$estimate + conform(crossTabs[["over25.educideal"]]$estimate,confidence.tab$estimate)
        crossTabs[["over25.educideal"]]$high <- crossTabs[["over25.educideal"]]$high + conform(crossTabs[["over25.educideal"]]$high,confidence.tab$high)
      }  
    }
    #Educ-interval
    if(length(over25$educ[which(!is.na(over25$educ))])!=0 & length(over25$small.interval[which(!is.na(over25$small.interval))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$small.interval,over25$weights,this.pop.over25)
      if(is.null(crossTabs[["over25.educinterval"]])){
        crossTabs[["over25.educinterval"]] <- confidence.tab
      }else{
        crossTabs[["over25.educinterval"]]$low <- crossTabs[["over25.educinterval"]]$low + conform(crossTabs[["over25.educinterval"]]$low,confidence.tab$low)
        crossTabs[["over25.educinterval"]]$estimate <- crossTabs[["over25.educinterval"]]$estimate + conform(crossTabs[["over25.educinterval"]]$estimate,confidence.tab$estimate)
        crossTabs[["over25.educinterval"]]$high <- crossTabs[["over25.educinterval"]]$high + conform(crossTabs[["over25.educinterval"]]$high,confidence.tab$high)
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

saveWorkbook(wb, "CM_crosstabs.xlsx", overwrite = TRUE)