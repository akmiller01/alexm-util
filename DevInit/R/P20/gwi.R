# setwd("D:/Documents/Data/DHSmeta/")
# classes <- read.csv("global_cwi_classes.csv",na.strings=c("","NAN"),as.is=TRUE)
# 
# gwi.dhs <- function(hrwd){
#   if(!file_test(op="-d", hrwd)){message("HR WD invalid");return(NA);}
#   
#   hrBase <- basename(hrwd)
#   iso2 <- toupper(substr(hrBase,1,2))
#   phase <- substr(hrBase,5,6)
#   
#   toilets.classes <- subset(classes,filename==hrBase & type=="toilets")
#   water.classes <- subset(classes,filename==hrBase & type=="water")
#   floor.classes <- subset(classes,filename==hrBase & type=="floor")
#   wall.classes <- subset(classes,filename==hrBase & type=="wall")
#   if(nrow(wall.classes)==0){stop("Missing from codebook!")}
#   if(nrow(water.classes)==0){stop("Missing from codebook!")}
#   if(nrow(floor.classes)==0){stop("Missing from codebook!")}
#   if(nrow(toilets.classes)==0){stop("Missing from codebook!")}
#   
#   hr <- read.csv(paste0(hrwd,"/",iso2,"HR",phase,"FL.csv")
#                  ,na.strings="",as.is=TRUE,check.names=FALSE)
#   
#   irwd <- paste0("D:/Documents/Data/DHSauto/",tolower(iso2),"ir",phase,"dt/")
#   if(!file_test(op="-d", irwd)){message("IR WD invalid");return(NA);}
#   
#   ir <- read.csv(paste0(irwd,iso2,"IR",phase,"FL.csv")
#                  ,na.strings="",as.is=TRUE,check.names=FALSE)
#   
#   #Rename wealth var
#   names(hr)[which(names(hr)=="hv271")] <- "wealth"
#   hr$wealth <- hr$wealth/100000
#   if(typeof(hr$wealth)=="NULL" | typeof(hr$wealth)=="logical" | length(hr$wealth[which(!is.na(hr$wealth))])==0){message("Wealth missing!");return(NA)}
#   
#   #Rename survey year var
#   names(hr)[which(names(hr)=="hv007")] <- "year"
#   
#   #Rename sample.weights var
#   names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
#   hr$weights <- hr$sample.weights/1000000
#   
#   #Rename urban var
#   names(hr)[which(names(hr)=="hv025")] <- "urban.rural"
#   
#   #Rename car/truck var
#   names(hr)[which(names(hr)=="hv212")] <- "car"
#   if(typeof(hr$car)=="NULL" | typeof(hr$car)=="logical" | length(hr$car[which(!is.na(hr$car))])==0){message("Car missing!");car.missing<-TRUE}else{car.missing<-FALSE}
#   
#   #Rename fridge var
#   names(hr)[which(names(hr)=="hv209")] <- "fridge"
#   if(typeof(hr$fridge)=="NULL" | typeof(hr$fridge)=="logical" | length(hr$fridge[which(!is.na(hr$fridge))])==0){message("Fridge missing!");fridge.missing<-TRUE}else{fridge.missing<-FALSE}
#   
#   #Rename phone var
#   names(hr)[which(names(hr)=="hv221")] <- "phone"
#   if(typeof(hr$phone)=="NULL" | typeof(hr$phone)=="logical" | length(hr$phone[which(!is.na(hr$phone))])==0){message("Phone missing!");phone.missing<-TRUE}else{phone.missing<-FALSE}
#   
#   #Rename tv var
#   names(hr)[which(names(hr)=="hv208")] <- "tv"
#   if(typeof(hr$tv)=="NULL" | typeof(hr$tv)=="logical" | length(hr$tv[which(!is.na(hr$tv))])==0){message("TV missing!");tv.missing<-TRUE}else{tv.missing<-FALSE}
#   
#   if(sum(car.missing,fridge.missing,phone.missing,tv.missing)>1){
#     return(NA)
#   }
#   
#   #Rename wall var
#   names(hr)[which(names(hr)=="hv214")] <- "wall"
#   if(typeof(hr$wall)=="NULL"){message("Missing wall!");hr$wall<-NA}
#   
#   #Rename floor var
#   names(hr)[which(names(hr)=="hv213")] <- "floor"
#   if(typeof(hr$floor)=="NULL"){message("Missing floor!");hr$floor<-NA}
#   
#   #Rename sleeping rooms var
#   if(typeof(hr$hv216)=="NULL" | typeof(hr$hv216)=="logical" | length(hr$hv216[which(!is.na(hr$hv216))])==0){
#     if(typeof(hr$sh40)=="NULL" | typeof(hr$sh40)=="logical" | length(hr$sh40[which(!is.na(hr$sh40))])==0){
#       hr$sleeping.rooms <- NA
#     }else{
#       names(hr)[which(names(hr)=="sh40")] <- "sleeping.rooms"
#       hr[which(hr$sleeping.rooms==99),] <- NA  
#     }
#   }else{
#     names(hr)[which(names(hr)=="hv216")] <- "sleeping.rooms"
#     hr[which(hr$sleeping.rooms==99),] <- NA 
#   }
#   
#   #Rename members var
#   names(hr)[which(names(hr)=="hv009")] <- "members"
#   
#   #Rename drinking water var
#   names(hr)[which(names(hr)=="hv201")] <- "water"
#   if(typeof(hr$water)=="NULL"){message("Missing water!");hr$water<-NA}
#   
#   #Rename toilets var
#   names(hr)[which(names(hr)=="hv205")] <- "toilets"
#   if(typeof(hr$toilets)=="NULL"){message("Missing toilets!");hr$toilets<-NA}
#   
#   #Rename share toilets var
#   names(hr)[which(names(hr)=="hv225")] <- "share.toilets"
#   if(typeof(hr$share.toilets)=="NULL" | typeof(hr$share.toilets)=="logical" | length(hr$share.toilets[which(!is.na(hr$share.toilets))])==0){share.toilets.missing<-TRUE}else{share.toilets.missing<-FALSE}
#   
#   #Rename partners occupation var
#   names(ir)[which(names(ir)=="v705")] <- "partner.occ"
#   if(typeof(ir$partner.occ)=="NULL"){partner.occ.missing<-TRUE}else{partner.occ.missing<-FALSE}
#   
#   #Rename partners education var
#   names(ir)[which(names(ir)=="v729")] <- "partner.educ"
#   if(typeof(ir$partner.educ)=="NULL"){partner.educ.missing<-TRUE}else{partner.educ.missing<-FALSE}
#   
#   #Rename occupation var
#   names(ir)[which(names(ir)=="v714")] <- "working"
#   if(typeof(ir$working)=="NULL"){working.missing<-TRUE}else{working.missing<-FALSE}
#   
#   #Rename educ var
#   names(ir)[which(names(ir)=="v149")] <- "educ"
#   
#   #Rename age var
#   names(ir)[which(names(ir)=="v012")] <- "age"
#   
#   #Rename head var
#   names(ir)[which(names(ir)=="v150")] <- "head"
#   
#   
#   #Rename cluster/hh var
#   names(ir)[which(names(ir)=="v001")] <- "cluster"
#   names(ir)[which(names(ir)=="v002")] <- "household"
#   names(hr)[which(names(hr)=="hv001")] <- "cluster"
#   names(hr)[which(names(hr)=="hv002")] <- "household"
#   
#   
#   #Recode IR level to hr
#   recode.partner.working <- function(x){
#     if(is.na(x)){return(NA)}
#     else if(is.null(x)){return(NULL)}
#     else if(x==98 | x==99 | tolower(x)=="don't know" | tolower(x)=="missing"){return(NA)}
#     else if(x==0 | tolower(x)=="did not work"){return(0)}
#     else{return(1)}
#   }
#   if(!partner.occ.missing){
#     ir$partner.working <- sapply(ir$partner.occ,recode.partner.working) 
#   }else{
#     ir$partner.working <- NA
#   }
#   recode.working <- function(x){
#     if(is.na(x)){return(NA)}
#     else if(is.null(x)){return(NULL)}
#     else if(x==9 | tolower(x)=="missing"){return(NA)}
#     else if(x==0 | tolower(x)=="no"){return(0)}
#     else if(x==1 | tolower(x)=="yes"){return(1)}
#     else{return(NA)}
#   }
#   if(!working.missing){
#     ir$working <- sapply(ir$working,recode.working)
#   }else{
#     ir$working <- NA
#   }
#   recode.educ <- function(x){
#     if(is.na(x)){return(NA)}
#     else if(is.null(x)){return(NULL)}
#     else if(is.factor(x)){
#       if(tolower(x)=="no education" | tolower(x)=="incomplete primary"){return(0)}
#       else{return(1)}
#     }else{
#       if(x<=1){return(0)}
#       else if(x>=8){return(NA)}
#       else if(x>=2){return(1)} 
#     }
#   }
#   if(!partner.educ.missing){
#     ir$partner.educ <- sapply(ir$partner.educ,recode.educ) 
#   }else{
#     ir$partner.educ <- NA
#   }
#   ir$educ <- sapply(ir$educ,recode.educ)
#   
#   members <- hr[c("cluster","household","members")]
#   
#   ir <- join(
#     ir
#     ,members
#     ,by=c("cluster","household")
#   )
#   
#   calc.hed <- function(membersV,workersV,adults.completed.primaryV){
#     hedV <- c()
#     for(i in 1:length(membersV)){
#       members <- membersV[i]
#       workers <- workersV[i]
#       adults.completed.primary <- adults.completed.primaryV[i]
#       
#       if(workers<1){
#         workers=1
#       }
#       hed = ((members/workers)>3 & adults.completed.primary==0)
#       hedV <- c(hedV,hed)
#     }
#     return(hedV)
#   }
#   
#   ir.table <- data.table(ir)
#   hed.member <- ir.table[,.(members=mean(members,na.rm=TRUE)), by=.(cluster,household)]
#   if(!(working.missing)){
#     if(!(partner.occ.missing)){
#       #Neither are missing
#       hed.worker <- ir.table[,.(workers=sum(working,na.rm=TRUE)+sum(partner.working,na.rm=TRUE)), by=.(cluster,household)]
#     }else{
#       #Just partner occ is missing
#       hed.worker <- ir.table[,.(workers=sum(working,na.rm=TRUE)), by=.(cluster,household)]
#     }
#   }else if(!(partner.occ.missing)){
#     #Working is missing, but partner occ is not
#     hed.worker <- ir.table[,.(workers=sum(partner.working,na.rm=TRUE)), by=.(cluster,household)]
#   }else{
#     #Both are missing
#     hed.worker <- ir.table[,.(workers=sum(members,na.rm=TRUE)), by=.(cluster,household)]
#     hed.worker$workers <- 1
#   }
#   if(!partner.educ.missing){
#     hed.primary <- ir.table[,.(adults.completed.primary=sum(educ==1,na.rm=TRUE)+sum(partner.educ==1,na.rm=TRUE)), by=.(cluster,household)]
#   }else{
#     hed.primary <- ir.table[,.(adults.completed.primary=sum(educ==1,na.rm=TRUE)), by=.(cluster,household)]
#   }
#   
#   hed <- join_all(list(hed.member,hed.worker,hed.primary),by=c("cluster","household"),type="full")
#   missing.workers <- hed[which(hed$workers<=0),]$workers
#   if(length(missing.workers)>0){
#     hed[which(hed$workers<=0),]$workers <- 1
#   }
#   
#   hed <- transform(hed
#                    ,hed = calc.hed(members,workers,adults.completed.primary)
#   )
#   
#   #Recode HR level vars
#   keep <- c("cluster","household","hed")
#   hed <- hed[keep]
#   hr <- join(
#     hr
#     ,hed
#     ,by=c("cluster","household")
#   )
#   
#   ###One worker assumption for households outside of IR
#   calc.hed.hr <- function(df){
#     hedV <- c()
#     for(i in 1:nrow(df)){
#       members <- df$members[i]
#       workers <- 1
#       adults.completed.primary <- 0
#       
#       for(j in 1:99){
#         ageVar <- paste0("hv105_",j)
#         educVar <- paste0("hv106_",j)
#         if(typeof(df[[ageVar]])!="NULL"){
#           age <- df[[ageVar]][i]
#           educ <- df[[educVar]][i]
#           if(!is.na(age)){
#             if(age>=15 & age<=49){
#               if(!is.na(educ)){
#                 if(recode.educ(educ)==1){
#                   adults.completed.primary <- adults.completed.primary + 1
#                 }
#               }
#             }
#           }
#         }else{
#           break;
#         }
#       }
#       
#       hed = ((members/workers)>3 & adults.completed.primary==0)
#       hedV <- c(hedV,hed)
#     }
#     return(hedV)
#   }
#   hr[which(is.na(hr$hed)),]$hed <- calc.hed.hr(hr[which(is.na(hr$hed)),])
#   
#   recode.wall <- function(x){
#     item <- subset(wall.classes,value==tolower(x))
#     if(nrow(item)==0){return(NA)}
#     else{item$inadequate[1]}
#   }
#   hr$inade.wall <- sapply(hr$wall,recode.wall)
#   hr$ade.wall <- hr$inade.wall==0
#   
#   recode.floor <- function(x){
#     item <- subset(floor.classes,value==tolower(x))
#     if(nrow(item)==0){return(NA)}
#     else{item$inadequate[1]}
#   }
#   hr$inade.floor <- sapply(hr$floor,recode.floor)
#   hr$ade.floor <- hr$inade.floor==0
#   
#   recode.urban.rural <- function(x){
#     if(is.null(x)){return(NA)}
#     else if(is.na(x)){return(NA)}
#     else if(tolower(x)=="urban" | x==1){return(1)}
#     else if(tolower(x)=="rural" | x==2){return(0)}
#     else{return(NA)}
#   }
#   hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
#   
#   code.inade.water <- function(urbanV,waterV){
#     inade.water <- c()
#     for(i in 1:length(urbanV)){
#       urban <- urbanV[i]
#       water <- tolower(waterV[i])
#       item <- subset(water.classes,value==water)
#       if(nrow(item)==0){
#         inade.water <- c(inade.water,NA)
#       }else{
#         if(urban==1){
#           inade.water <- c(inade.water,item$urban.inadequate[1])
#         }else if(urban==0){
#           inade.water <- c(inade.water,item$rural.inadequate[1])
#         }else{
#           inade.water <- c(inade.water,NA)
#         } 
#       }
#     }
#     return(inade.water)
#   }
#   
#   hr$inade.water <- code.inade.water(hr$urban,hr$water)
#   hr$ade.water <- hr$inade.water==0
#   
#   code.toilets <- function(toiletsV,share.toiletsV,share.toilets.missing){
#     inade.toilets <- c()
#     for(i in 1:length(toiletsV)){
#       toilets <- tolower(toiletsV[i])
#       share.toilets <- tolower(share.toiletsV[i])
#       item <- subset(toilets.classes,value==toilets)
#       if(share.toilets.missing){
#         share.toilets = 0
#       }
#       if(is.na(share.toilets)){
#         share.toilets = 0
#       }
#       if(share.toilets==1 | share.toilets=="yes"){
#         inade.toilet = 1
#       }else{
#         inade.toilet = item$inadequate[1]
#       }
#       inade.toilets <- c(inade.toilets,inade.toilet)
#     }
#     return(inade.toilets)
#   }
#   hr$inade.toilets <- code.toilets(hr$toilets,hr$share.toilets,share.toilets.missing)
#   hr$ade.toilets <- hr$inade.toilets==0
# 
#   recode.asset <- function(x){
#     if(is.null(x)){return(NA)}
#     else if(is.na(x) | x==9){return(NA)}
#     else if(x==1 | tolower(x)=="yes"){return(TRUE)}
#     else if(x==0 | tolower(x)=="no"){return(FALSE)}
#     else{return(NA)}
#   }
#   
#   ###Replication method
#   #Calc wealth where half of households own tv
#   if(!(tv.missing)){
#     hr$tv <- sapply(hr$tv,recode.asset)
#   }
#   
#   #Calc wealth where half of households own fridge
#   if(!(fridge.missing)){
#     hr$fridge <- sapply(hr$fridge,recode.asset)
#   }
#   
#   #Calc wealth where half of households own car
#   if(!(car.missing)){
#     hr$car <- sapply(hr$car,recode.asset)
#   }
#   
#   #Calc wealth where half of households own phone
#   if(!(phone.missing)){
#     hr$phone <- sapply(hr$phone,recode.asset)
#   }
#   
#   #Crowding
#   hr <- transform(hr
#                   ,crowding = ((members/sleeping.rooms))
#   )
#   
#   #Flip HED
#   hr$led <- !hr$hed
#   
#   keep <- c("cluster","household","wealth","weights","urban","ade.wall","ade.floor","ade.water","ade.toilets"
#             ,"tv","fridge","car","phone","led","crowding"
#   )
#   hrNames <- names(hr)
#   namesDiff <- setdiff(keep,hrNames)
#   if(length(namesDiff)>0){
#     for(y in 1:length(namesDiff)){
#       hr[namesDiff[y]] <- NA
#       message(paste("Missing variable",namesDiff[y]))
#     } 
#   }
#   hr <- hr[keep]
#   hr$filename <- hrBase
#   
#   return(hr)
# }
# 
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
#   if(recode=="hr" & phase>=5){
#     message(basename(dir))
#     gwi <- gwi.dhs(dir) 
#     if(!is.na(gwi)){
#       dataList[[dataIndex]] <- gwi
#       dataIndex <- dataIndex + 1 
#     }
#   }
# }
# 
# dhs <- rbindlist(dataList)
# setnames(dhs,"wealth","original.wealth")
# dhs <- data.frame(dhs)
# dhs$crowding[which(is.infinite(dhs$crowding))] <- NA
# 
# save(dhs,file="D:/Documents/Data/P20_2013/dhs.wealth.RData")
load("D:/Documents/Data/P20_2013/dhs.wealth.RData")

source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

catvars <- c(6:14)
numvars <- c(15)

dhs.gwi <- wealth(dhs,catvars,numvars,"urban")
plot(original.wealth~wealth,data=dhs.gwi)
dhs.gwi <- dhs.gwi[order(dhs.gwi$wealth),]

setwd("D:/Documents/Data/P20_2013")
save(dhs.gwi,file="dhs.gwi.RData")
