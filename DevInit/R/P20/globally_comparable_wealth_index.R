####Function and setup####
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

setwd("D:/Documents/Data/P20_2013/meta")

headcounts <- read.csv("headcounts.csv",as.is=TRUE,na.strings="")
filenames <- headcounts$filename

setwd("D:/Documents/Data/DHSmeta/")
classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)

catvars <- c("ade.wall","ade.floor","ade.water","ade.toilets","hed","tv","phone","car","fridge")
numvars <- c("sleeping.rooms")

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
#   if(recode=="hr" & phase>=5){
  if(basename(dir) %in% filenames){
    hrwd <- basename(dir)
    message(hrwd)
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
    water.classes <- subset(classes,filename==hrBase & type=="water")
    floor.classes <- subset(classes,filename==hrBase & type=="floor")
    wall.classes <- subset(classes,filename==hrBase & type=="wall")
    
    hr <- read.csv(paste0(hrwd,"/",iso2,"HR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
    if(!file_test(op="-d", irwd)){message("IR WD invalid");return(NA);}
    
    ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    #Rename wealth var
    names(hr)[which(names(hr)=="hv271")] <- "wealth"
    hr$wealth <- hr$wealth/100000
    if(typeof(hr$wealth)=="NULL" | typeof(hr$wealth)=="logical" | length(hr$wealth[which(!is.na(hr$wealth))])==0){message("Wealth missing!");return(NA)}
    
    #Rename survey year var
    names(hr)[which(names(hr)=="hv007")] <- "year"
    
    #Rename sample.weights var
    names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
    hr$weights <- hr$sample.weights/1000000
    
    #Rename urban var
    names(hr)[which(names(hr)=="hv025")] <- "urban.rural"
    
    #Rename car/truck var
    names(hr)[which(names(hr)=="hv212")] <- "car"
    if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
    
    #Rename fridge var
    names(hr)[which(names(hr)=="hv209")] <- "fridge"
    if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
    
    #Rename phone var
    names(hr)[which(names(hr)=="hv221")] <- "phone"
    if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
    
    #Rename tv var
    names(hr)[which(names(hr)=="hv208")] <- "tv"
    if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
    
    if(sum(car.missing,fridge.missing,phone.missing,tv.missing)>1){
      return(NA)
    }
    
    #Rename wall var
    names(hr)[which(names(hr)=="hv214")] <- "wall"
    if(typeof(hr$wall)=="NULL"){message("Missing wall!");hr$wall<-NA}
    
    #Rename floor var
    names(hr)[which(names(hr)=="hv213")] <- "floor"
    if(typeof(hr$floor)=="NULL"){message("Missing floor!");hr$floor<-NA}
    
    #Rename sleeping rooms var
    if(typeof(hr$hv216)=="NULL" | typeof(hr$hv216)=="logical" | length(hr$hv216[which(!is.na(hr$hv216))])==0){
      if(typeof(hr$sh40)=="NULL" | typeof(hr$sh40)=="logical" | length(hr$sh40[which(!is.na(hr$sh40))])==0){
        hr$sleeping.rooms <- NA
      }else{
        names(hr)[which(names(hr)=="sh40")] <- "sleeping.rooms"
        hr[which(hr$sleeping.rooms==99),] <- NA  
      }
    }else{
      names(hr)[which(names(hr)=="hv216")] <- "sleeping.rooms"
      hr[which(hr$sleeping.rooms==99),] <- NA 
    }
    
    #Rename members var
    names(hr)[which(names(hr)=="hv009")] <- "members"
    
    #Rename drinking water var
    names(hr)[which(names(hr)=="hv201")] <- "water"
    if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}
    
    #Rename toilets var
    names(hr)[which(names(hr)=="hv205")] <- "toilets"
    if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}
    
    #Rename share toilets var
    names(hr)[which(names(hr)=="hv225")] <- "share.toilets"
    if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
    
    #Rename partners occupation var
    names(ir)[which(names(ir)=="v705")] <- "partner.occ"
    if(typeof(ir$partner.occ)=="NULL"){partner.occ.missing<-TRUE}else{partner.occ.missing<-FALSE}
    
    #Rename partners education var
    names(ir)[which(names(ir)=="v729")] <- "partner.educ"
    if(typeof(ir$partner.educ)=="NULL"){partner.educ.missing<-TRUE}else{partner.educ.missing<-FALSE}
    
    #Rename occupation var
    names(ir)[which(names(ir)=="v714")] <- "working"
    if(typeof(ir$working)=="NULL"){working.missing<-TRUE}else{working.missing<-FALSE}
    
    #Rename educ var
    names(ir)[which(names(ir)=="v149")] <- "educ"
    
    #Rename age var
    names(ir)[which(names(ir)=="v012")] <- "age"
    
    #Rename head var
    names(ir)[which(names(ir)=="v150")] <- "head"
    
    
    #Rename cluster/hh var
    names(ir)[which(names(ir)=="v001")] <- "cluster"
    names(ir)[which(names(ir)=="v002")] <- "household"
    names(hr)[which(names(hr)=="hv001")] <- "cluster"
    names(hr)[which(names(hr)=="hv002")] <- "household"
    
    
    #Recode IR level to hr
    recode.partner.working <- function(x){
      if(is.na(x)){return(NA)}
      else if(is.null(x)){return(NULL)}
      else if(x==98 | x==99 | tolower(x)=="don't know" | tolower(x)=="missing"){return(NA)}
      else if(x==0 | tolower(x)=="did not work"){return(0)}
      else{return(1)}
    }
    if(!partner.occ.missing){
      ir$partner.working <- sapply(ir$partner.occ,recode.partner.working) 
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
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(is.null(x)){return(NULL)}
      else if(is.factor(x)){
        if(tolower(x)=="no education" | tolower(x)=="incomplete primary"){return(0)}
        else{return(1)}
      }else{
        if(x<=1){return(0)}
        else if(x>=8){return(NA)}
        else if(x>=2){return(1)} 
      }
    }
    if(!partner.educ.missing){
      ir$partner.educ <- sapply(ir$partner.educ,recode.educ) 
    }else{
      ir$partner.educ <- NA
    }
    ir$educ <- sapply(ir$educ,recode.educ)
    
    members <- hr[c("cluster","household","members")]
    
    ir <- join(
      ir
      ,members
      ,by=c("cluster","household")
    )
    
    calc.hed <- function(membersV,workersV,adults.completed.primaryV){
      hedV <- c()
      for(i in 1:length(membersV)){
        members <- membersV[i]
        workers <- workersV[i]
        adults.completed.primary <- adults.completed.primaryV[i]
        
        if(workers<1){
          workers=1
        }
        hed = ((members/workers)>3 & adults.completed.primary==0)
        hedV <- c(hedV,hed)
      }
      return(hedV)
    }
    
    ir.table <- data.table(ir)
    hed.member <- ir.table[,.(members=mean(members,na.rm=TRUE)), by=.(cluster,household)]
    if(!(working.missing)){
      if(!(partner.occ.missing)){
        #Neither are missing
        hed.worker <- ir.table[,.(workers=sum(working,na.rm=TRUE)+sum(partner.working,na.rm=TRUE)), by=.(cluster,household)]
      }else{
        #Just partner occ is missing
        hed.worker <- ir.table[,.(workers=sum(working,na.rm=TRUE)), by=.(cluster,household)]
      }
    }else if(!(partner.occ.missing)){
      #Working is missing, but partner occ is not
      hed.worker <- ir.table[,.(workers=sum(partner.working,na.rm=TRUE)), by=.(cluster,household)]
    }else{
      #Both are missing
      hed.worker <- ir.table[,.(workers=sum(members,na.rm=TRUE)), by=.(cluster,household)]
      hed.worker$workers <- 1
    }
    if(!partner.educ.missing){
      hed.primary <- ir.table[,.(adults.completed.primary=sum(educ==1,na.rm=TRUE)+sum(partner.educ==1,na.rm=TRUE)), by=.(cluster,household)]
    }else{
      hed.primary <- ir.table[,.(adults.completed.primary=sum(educ==1,na.rm=TRUE)), by=.(cluster,household)]
    }
    
    hed <- join_all(list(hed.member,hed.worker,hed.primary),by=c("cluster","household"),type="full")
    missing.workers <- hed[which(hed$workers<=0),]$workers
    if(length(missing.workers)>0){
      hed[which(hed$workers<=0),]$workers <- 1
    }
    
    hed <- transform(hed
                     ,hed = calc.hed(members,workers,adults.completed.primary)
    )
    
    #Recode HR level vars
    keep <- c("cluster","household","hed")
    hed <- hed[keep]
    hr <- join(
      hr
      ,hed
      ,by=c("cluster","household")
    )
    
    ###One worker assumption for households outside of IR
    calc.hed.hr <- function(df){
      hedV <- c()
      for(i in 1:nrow(df)){
        members <- df$members[i]
        workers <- 1
        adults.completed.primary <- 0
        
        for(j in 1:99){
          ageVar <- paste0("hv105_",j)
          educVar <- paste0("hv106_",j)
          if(typeof(df[[ageVar]])!="NULL"){
            age <- df[[ageVar]][i]
            educ <- df[[educVar]][i]
            if(!is.na(age)){
              if(age>=15 & age<=49){
                if(!is.na(educ)){
                  if(recode.educ(educ)==1){
                    adults.completed.primary <- adults.completed.primary + 1
                  }
                }
              }
            }
          }else{
            break;
          }
        }
        
        hed = ((members/workers)>3 & adults.completed.primary==0)
        hedV <- c(hedV,hed)
      }
      return(hedV)
    }
    hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
    
    recode.wall <- function(x){
      item <- subset(wall.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$inadequate[1]}
    }
    hr$inade.wall <- sapply(hr$wall,recode.wall)
    hr$ade.wall <- !hr$inade.wall
    
    recode.floor <- function(x){
      item <- subset(floor.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$inadequate[1]}
    }
    hr$inade.floor <- sapply(hr$floor,recode.floor)
    hr$ade.floor <- !hr$inade.floor
    
    recode.urban.rural <- function(x){
      if(is.null(x)){return(NA)}
      else if(is.na(x)){return(NA)}
      else if(tolower(x)=="urban" | x==1){return(1)}
      else if(tolower(x)=="rural" | x==2){return(0)}
      else{return(NA)}
    }
    hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
    
    code.inade.water <- function(urbanV,waterV){
      inade.water <- c()
      for(i in 1:length(urbanV)){
        urban <- urbanV[i]
        water <- tolower(waterV[i])
        item <- subset(water.classes,value==water)
        if(nrow(item)==0){
          inade.water <- c(inade.water,NA)
        }else{
          if(urban==1){
            inade.water <- c(inade.water,item$urban.inadequate[1])
          }else if(urban==0){
            inade.water <- c(inade.water,item$rural.inadequate[1])
          }else{
            inade.water <- c(inade.water,NA)
          } 
        }
      }
      return(inade.water)
    }
    
    hr$inade.water <- code.inade.water(hr$urban,hr$water)
    hr$ade.water <- !hr$inade.water
    
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
        if(share.toilets==1 | share.toilets=="yes"){
          inade.toilet = 1
        }else{
          inade.toilet = item$inadequate[1]
        }
        inade.toilets <- c(inade.toilets,inade.toilet)
      }
      return(inade.toilets)
    }
    hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets,share.toilets.missing)
    hr$ade.toilets <- !hr$inade.toilets
    
    recode.asset <- function(x){
      if(is.null(x)){return(NA)}
      else if(is.na(x) | x==9){return(NA)}
      else if(x==1 | tolower(x)=="yes"){return(1)}
      else if(x==0 | tolower(x)=="no"){return(0)}
      else{return(NA)}
    }
    
    ###Replication method
    #Calc wealth where half of households own tv
    if(!(tv.missing)){
      hr$tv <- sapply(hr$tv,recode.asset)
    }
    
    #Calc wealth where half of households own fridge
    if(!(fridge.missing)){
      hr$fridge <- sapply(hr$fridge,recode.asset)
    }
    
    #Calc wealth where half of households own car
    if(!(car.missing)){
      hr$car <- sapply(hr$car,recode.asset)
    }
    
    #Calc wealth where half of households own phone
    if(!(phone.missing)){
      hr$phone <- sapply(hr$phone,recode.asset)
    }
    
    keep = c("weights","urban","household","cluster","wealth","ade.wall","wall","ade.floor","floor","members","sleeping.rooms","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
    hrNames <- names(hr)
    namesDiff <- setdiff(keep,hrNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    gcw.data <- hr[keep]
    setnames(gcw.data,"wealth","old.wealth")
#     catvars <- c("ade.wall","wall","ade.floor","floor","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
#     numvars <- NULL
#     gcw.wealth <- wealth(gcw.data,catvars,numvars,"urban")
#     correlation <- cor(gcw.wealth$old.wealth,gcw.wealth$wealth)
#     if(correlation<0){
#       gcw.wealth$wealth <- gcw.wealth$wealth*-1
#     }
#     message(cor(gcw.wealth$old.wealth,gcw.wealth$wealth))
#     keep = c("weights","urban","household","cluster","old.wealth","wealth","ade.wall","wall","ade.floor","floor","members","sleeping.rooms","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
#     gcw.wealth <- gcw.wealth[keep]
#     gcw.wealth$filename <- hrBase
#     dataList[[dataIndex]] <- gcw.wealth
    gcw.data$filename <- hrBase
    dataList[[dataIndex]] <- gcw.data
    dataIndex <- dataIndex + 1 
  }
}

setwd("D:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")
  
####Run function####

# set our working directory, change this if using on another machine
wd <- "D:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

for(i in 2:length(dirs)){
  dir <- dirs[i]
  if(basename(dir) %in% filenames){
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    if(typeof(hh$hh1)!="NULL"){
      message(basename(dir))
      
      hr <- data.frame(hh,as.is=TRUE,check.names=FALSE)
      ir <- data.frame(hl,as.is=TRUE,check.names=FALSE)
      hrBase <- hh$filename
      
      file.varName <- subset(varNames,filename==hrBase)
      
      attendedVar <- subset(file.varName,match=="attended")$varName
      gradeVar <- subset(file.varName,match=="grade")$varName
      schoolVar <- subset(file.varName,match=="school")$varName
      
      share.toiletsVar <- subset(file.varName,match=="share.toilets")$varName
      toiletsVar <- subset(file.varName,match=="toilets")$varName
      
      carVar <- subset(file.varName,match=="car")$varName
      fridgeVar <- subset(file.varName,match=="fridge")$varName
      phoneVar <- subset(file.varName,match=="phone")$varName
      tvVar <- subset(file.varName,match=="tv")$varName
      
      toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
      water.classes <- subset(classes,filename==hrBase & type=="water")
      floor.classes <- subset(classes,filename==hrBase & type=="floor")
      wall.classes <- subset(classes,filename==hrBase & type=="wall")
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
      
      #Rename wealth var
      if(typeof(hr$wlthscor)=="NULL" | typeof(hr$wlthscor)=="logical" | length(hr$wlthscor[which(!is.na(hr$wlthscor))])==0){
        if(typeof(hr$wscore)=="NULL" | typeof(hr$wscore)=="logical" | length(hr$wscore[which(!is.na(hr$wscore))])==0){
          message("Wealth missing!");
        }else{
          names(hr)[which(names(hr)=="wscore")] <- "wealth"
        }
      }else{
        names(hr)[which(names(hr)=="wlthscor")] <- "wealth"
      }
      
      
      #Rename survey year var
      names(hr)[which(names(hr)=="hh5y")] <- "year"
      
      #Rename sample.weights var
      names(hr)[which(names(hr)=="hhweight")] <- "weights"
      
      #Rename urban var
      names(hr)[which(names(hr)=="hh6")] <- "urban.rural"
      if(typeof(hr$urban.rural)=="NULL"){message("No urban.rural!");hr$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}
      
      #check car/truck var
      if(length(carVar)<=0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
      
      #check fridge var
      if(length(fridgeVar)<=0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
      
      #check phone var
      if(length(phoneVar)<=0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
      
      #check tv var
      if(length(tvVar)<=0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
      
      #Rename wall var
      names(hr)[which(names(hr)=="hc5")] <- "wall"
      if(typeof(hr$wall)=="NULL"){message("No wall!");hr$wall<-NA}
      
      #Rename floor var
      names(hr)[which(names(hr)=="hc3")] <- "floor"
      if(typeof(hr$floor)=="NULL"){message("No floor!");hr$floor<-NA}
      
      #Rename drinking water var
      names(hr)[which(names(hr)=="ws1")] <- "water"
      if(typeof(hr$water)=="NULL"){message("No water!");hr$water<-NA}
      
      #Rename toilets var
      names(hr)[which(names(hr)==toiletsVar)] <- "toilets"
      if(typeof(hr$toilets)=="NULL"){message("No toilets!");hr$toilets<-NA}
      
      #Rename share toilets var
      names(hr)[which(names(hr)==share.toiletsVar)] <- "share.toilets"
      if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
      
      #Rename sleeping rooms var
      names(hr)[which(names(hr)=="hc2")] <- "sleeping.rooms"
      if(typeof(hr$sleeping.rooms)=="NULL"){message("No sleeping.rooms!");hr$sleeping.rooms<-NA}
      
      #Rename members var
      names(hr)[which(names(hr)=="hh11")] <- "members"
      
      #Rename educ var
      names(ir)[which(names(ir)==attendedVar)] <- "attended"
      names(ir)[which(names(ir)==schoolVar)] <- "school"
      names(ir)[which(names(ir)==gradeVar)] <- "grade"
      
      #Rename age var
      names(ir)[which(names(ir)=="hl5")] <- "age"
      
      #Rename head var
      names(ir)[which(names(ir)=="hl3")] <- "head"
      
      #Rename cluster/hh var
      names(ir)[which(names(ir)=="hh1")] <- "cluster"
      names(ir)[which(names(ir)=="hh2")] <- "household"
      names(hr)[which(names(hr)=="hh1")] <- "cluster"
      names(hr)[which(names(hr)=="hh2")] <- "household"
      
      
      #Recode IR level to hr
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
                if(grade>=5){
                  educ <- 1
                }else{
                  educ <- 0
                }
              }
            }else{
              #missing attended, but not level
              if(is.na(grade)){
                #has level, but not grade
                if(school %in% secondary.level | school %in% higher.level){
                  educ <- 1
                }else if(school %in% preschool.level | school %in% none.level){
                  educ <- 0
                }else{
                  educ <- NA
                }
              }else{
                #missing attended and level, but not grade
                if(grade>=5){
                  educ <- 1
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
                if(grade>=5){
                  educ <- 1
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
                educ <- NA
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
              educ <- 1
            } else if(school %in% higher.level){
              #(in)complete higher
              educ <- 1
            }else if(grade>=5){
              #at least 5 years of some other schooling
              educ <- 1
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
      
      ir$educ <- recode.educ(ir$attended,ir$school,ir$grade)
      
      members <- hr[c("cluster","household","members")]
      
      ir <- join(
        ir
        ,members
        ,by=c("cluster","household")
      )
      
      calc.hed <- function(membersV,adults.completed.primaryV){
        hedV <- c()
        for(i in 1:length(membersV)){
          members <- membersV[i]
          workers <- 1
          adults.completed.primary <- adults.completed.primaryV[i]
          
          hed = ((members/workers)>3 & adults.completed.primary==0)
          hedV <- c(hedV,hed)
        }
        return(hedV)
      }
      
      ir.table <- data.table(ir)
      hed <- ir.table[,.(
        members=mean(members,na.rm=TRUE)
        ,adults.completed.primary=sum(educ==1,na.rm=TRUE)
      ), by=.(cluster,household)]
      
      hed <- transform(hed
                       ,hed = calc.hed(members,adults.completed.primary)
      )
      
      hed <- data.frame(hed)
      
      #Recode HR level vars
      keep <- c("cluster","household","hed")
      hed <- hed[keep]
      hr <- join(
        hr
        ,hed
        ,by=c("cluster","household")
      )
      
      ###One worker assumption for households outside of IR
      ###Skipping for MICS now, since helevel/hhlevel can vary   
      #   calc.hed.hr <- function(df){
      #     hedV <- c()
      #     for(i in 1:nrow(df)){
      #       members <- df$members[i]
      #       head.educ <- tolower(df$helevel[i])
      #       workers <- 1
      #       adults.completed.primary <- 0
      #       
      #       if(
      #         head.educ %in% secondary.level | head.educ %in% higher.level
      #          ){adults.completed.primary <- 1}
      #       
      #       hed = ((members/workers)>3 & adults.completed.primary==0)
      #       hedV <- c(hedV,hed)
      #     }
      #     return(hedV)
      #   }
      #   hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
      
      recode.wall <- function(x){
        item <- subset(wall.classes,value==tolower(x))
        if(nrow(item)==0){return(NA)}
        else{item$inadequate[1]}
      }
      hr$inade.wall <- sapply(hr$wall,recode.wall)
      hr$ade.wall <- !hr$inade.wall
      
      recode.floor <- function(x){
        item <- subset(floor.classes,value==tolower(x))
        if(nrow(item)==0){return(NA)}
        else{item$inadequate[1]}
      }
      hr$inade.floor <- sapply(hr$floor,recode.floor)
      hr$ade.floor <- !hr$inade.floor
      
      hr <- transform(hr
                      ,crowded = ((members/sleeping.rooms)>3)
      )
      
      recode.urban.rural <- function(x){
        item <- subset(urban.rural.classes,value==tolower(x))
        if(nrow(item)==0){return(NA)}
        else{item$urban[1]}
      }
      hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
      
      code.inade.water <- function(urbanV,waterV){
        inade.water <- c()
        for(i in 1:length(urbanV)){
          urban <- urbanV[i]
          water <- tolower(waterV[i])
          item <- subset(water.classes,value==water)
          if(nrow(item)==0){
            inade.water <- c(inade.water,NA)
          }else{
            if(is.na(urban)){
              #Assume stricter codebook?
              inade.water <- c(inade.water,item$urban.inadequate[1])
            }else if(urban==1){
              inade.water <- c(inade.water,item$urban.inadequate[1])
            }else if(urban==0){
              inade.water <- c(inade.water,item$rural.inadequate[1])
            }else{
              inade.water <- c(inade.water,NA)
            } 
          }
        }
        return(inade.water)
      }
      
      hr$inade.water <- code.inade.water(hr$urban,hr$water)
      hr$ade.water <- !hr$inade.water
      
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
      hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets,share.toilets.missing)
      hr$ade.toilets <- !hr$inade.toilets
      
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
      
      ###Replication method
      #Calc wealth where half of households own tv
      if(!(tv.missing)){
        hr$tv <- recode.asset(hr[[tvVar[1]]],hr[[tvVar[2]]],hr[[tvVar[3]]])
      }
      
      #Calc wealth where half of households own fridge
      if(!(fridge.missing)){
        hr$fridge <- recode.asset(hr[[fridgeVar[1]]],hr[[fridgeVar[2]]],hr[[fridgeVar[3]]])
      }
      
      #Calc wealth where half of households own car
      if(!(car.missing)){
        hr$car <- recode.asset(hr[[carVar[1]]],hr[[carVar[2]]],hr[[carVar[3]]])
      }
      
      #Calc wealth where half of households own phone
      if(!(phone.missing)){
        hr$phone <- recode.asset(hr[[phoneVar[1]]],hr[[phoneVar[2]]],hr[[phoneVar[3]]])
      }
      
      
      keep = c("weights","urban","household","cluster","wealth","ade.wall","wall","ade.floor","floor","members","sleeping.rooms","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
      hrNames <- names(hr)
      namesDiff <- setdiff(keep,hrNames)
      if(length(namesDiff)>0){
        for(y in 1:length(namesDiff)){
          hr[namesDiff[y]] <- NA
          message(paste("Missing variable",namesDiff[y]))
        } 
      }
      gcw.data <- hr[keep]
      setnames(gcw.data,"wealth","old.wealth")
  #     catvars <- c("ade.wall","wall","ade.floor","floor","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
  #     numvars <- NULL
  #     gcw.wealth <- wealth(gcw.data,catvars,numvars,"urban")
  #     correlation <- cor(gcw.wealth$old.wealth,gcw.wealth$wealth)
  #     if(correlation<0){
  #       gcw.wealth <- gcw.wealth*-1
  #     }
  #     message(cor(gcw.wealth$old.wealth,gcw.wealth$wealth))
  #     keep = c("weights","urban","household","cluster","old.wealth","wealth","ade.wall","wall","ade.floor","floor","members","sleeping.rooms","ade.water","water","ade.toilets","toilets","share.toilets","hed","tv","phone","car","fridge")
  #     gcw.wealth <- gcw.wealth[keep]
  #     gcw.wealth$filename <- hrBase
  #     dataList[[dataIndex]] <- gcw.wealth
      gcw.data$filename <- hrBase
      dataList[[dataIndex]] <- gcw.data
      dataIndex <- dataIndex + 1 
    }
  }
}

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)

gcw <- rbindlist(dataList,fill=TRUE)
save(gcw,file="gcw.RData")
load("gcw.RData")

gcw$share.toilets[which(tolower(gcw$share.toilets)=="no")] <- 0
gcw$share.toilets[which(tolower(gcw$share.toilets)=="non")] <- 0
gcw$share.toilets[which(tolower(gcw$share.toilets)=="yes")] <- 1
gcw$share.toilets[which(tolower(gcw$share.toilets)=="oui")] <- 1
gcw$share.toilets[which(tolower(gcw$share.toilets)=="sí")] <- 1
gcw$share.toilets[which(substr(tolower(gcw$share.toilets),1,1)=="s")] <- 1
gcw$share.toilets[which(tolower(gcw$share.toilets)=="yes, public")] <- 1
gcw$share.toilets[which(tolower(gcw$share.toilets)=="yes, other household only")] <- 1
gcw$share.toilets[which(tolower(gcw$share.toilets)=="missing")] <- NA
gcw$share.toilets[which(tolower(gcw$share.toilets)=="manquant")] <- NA
gcw$share.toilets[which(tolower(gcw$share.toilets)==9)] <- NA

delete.negative <- function(m){
  dummy.columns <- colnames(m)
  
  delete.index <- c(grep("FALSE",dummy.columns,ignore.case=TRUE) 
    ,grep("9",dummy.columns)
    ,grep("0",dummy.columns,ignore.case=TRUE)
    ,grep("DK",dummy.columns)
    ,grep("missing",dummy.columns,ignore.case=TRUE)
    ,grep("manquant",dummy.columns,ignore.case=TRUE)
  )
  
  m <- m[,-delete.index]
  return(m)
}

dummyList <- list()
gcw$ade.wall <- factor(gcw$ade.wall)
dummyList[[1]] <- model.matrix( ~ ade.wall - 1, data=gcw)
gcw$wall <- factor(gcw$wall)
dummyList[[2]] <- model.matrix( ~ wall - 1, data=gcw)
gcw$ade.floor <- factor(gcw$ade.floor)
dummyList[[3]] <- model.matrix( ~ ade.floor - 1, data=gcw)
gcw$floor <- factor(gcw$floor)
dummyList[[4]] <- model.matrix( ~ floor - 1, data=gcw)
gcw$ade.water <- factor(gcw$ade.water)
dummyList[[5]] <- model.matrix( ~ ade.water - 1, data=gcw)
gcw$water <- factor(gcw$water)
dummyList[[6]] <- model.matrix( ~ water - 1, data=gcw)
gcw$ade.toilets <- factor(gcw$ade.toilets)
dummyList[[7]] <- model.matrix( ~ ade.toilets - 1, data=gcw)
gcw$toilets <- factor(gcw$toilets)
dummyList[[8]] <- model.matrix( ~ toilets - 1, data=gcw)
gcw$share.toilets <- factor(gcw$share.toilets)
dummyList[[9]] <- model.matrix( ~ share.toilets - 1, data=gcw)
gcw$hed <- factor(gcw$hed)
dummyList[[10]] <- model.matrix( ~ hed - 1, data=gcw)
gcw$tv <- factor(gcw$tv)
dummyList[[11]] <- model.matrix( ~ tv - 1, data=gcw)
gcw$phone <- factor(gcw$phone)
dummyList[[12]] <- model.matrix( ~ phone - 1, data=gcw)
gcw$car <- factor(gcw$car)
dummyList[[13]] <- model.matrix( ~ car - 1, data=gcw)
gcw$fridge <- factor(gcw$fridge)
dummyList[[14]] <- model.matrix( ~ fridge - 1, data=gcw)

rm(gcw)
gc()


dummies <- dummyList[[1]]
dummyList[[1]] <- NULL
gc()

while(length(dummyList)>6){
  if(nrow(dummyList[[1]])<=nrow(dummies)){
    dummies <- cbind(dummies, dummyList[[1]][match(rownames(dummies),rownames(dummyList[[1]])),])  
  }else{
    dummies <- cbind(dummyList[[1]],dummies[match(rownames(dummyList[[1]]),rownames(dummies)),])  
  }
  dummyList[[1]] <- NULL
  gc()
}

save(dummies,file="first.half.dummies.RData")
rm(dummies)
gc()

dummies2 <- dummyList[[1]]
dummyList[[1]] <- NULL
gc()

while(length(dummyList)>0){
  if(nrow(dummyList[[1]])<=nrow(dummies2)){
    dummies2 <- cbind(dummies2, dummyList[[1]][match(rownames(dummies2),rownames(dummyList[[1]])),])  
  }else{
    dummies2 <- cbind(dummyList[[1]],dummies2[match(rownames(dummyList[[1]]),rownames(dummies2)),])  
  }
  dummyList[[1]] <- NULL
  gc()
}

rm(dummyList)
gc()

load("first.half.dummies.RData")
gc()
if(nrow(dummies2)<=nrow(dummies)){
  dummies <- cbind(dummies, dummies2[match(rownames(dummies),rownames(dummies2)),])  
}else{
  dummies <- cbind(dummies2,dummies[match(rownames(dummies2),rownames(dummies)),])  
}
rm(dummies2)
gc()

dummies <- delete.negative(dummies)

save(dummies,file="dummies.RData")
load("dummies.RData")
load("gcw.RData")
