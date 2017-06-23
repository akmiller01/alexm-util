library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(WDI)
library(varhandle)
require(zoo)

# Stop crosstab from plotting everything
options(descr.plot = FALSE)

setwd("D:/Documents/Data/P20_2013/meta")
load("total_triple.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

# totalWeights <- data.table(data.total)[,.(total.weights=sum(weights,na.rm=TRUE)),by=.(filename,p20)]
# ageTable <- data.table(data.total)[,.(age.weights=sum(weights,na.rm=TRUE)),by=.(filename,p20,age)]
# ageTable <- merge(ageTable,totalWeights,by=c("filename","p20"))
# ageTable$weighted.percent = ageTable$age.weights/ageTable$total.weights
# write.csv(ageTable,"ageTable.csv",row.names=FALSE,na="")
# 
# data.total$whipple <- substr(as.character(data.total$age),nchar(as.character(data.total$age)),nchar(as.character(data.total$age)))
# data.total$whipple05 <- data.total$whipple %in% c("0","5")
# mean(data.total$whipple05,na.rm=TRUE)*500
# 
# p20 <- subset(data.total,p20==TRUE & age>=15 & age<=49)
# mean(p20$whipple05,na.rm=TRUE)*500
# nonp20 <- subset(data.total,p20==FALSE  & age>=15 & age<=49)
# mean(nonp20$whipple05,na.rm=TRUE)*500
# 
# children <- subset(data.total,!is.na(age.months))
# children$whipple <- substr(as.character(children$age.months),nchar(as.character(children$age.months)),nchar(as.character(children$age.months)))
# children$whipple06 <- children$whipple %in% c("0","6")
# mean(children$whipple06,na.rm=TRUE)*500
# p20.kids <- subset(children,p20==TRUE)
# mean(p20.kids$whipple06,na.rm=TRUE)*500
# nonp20.kids <- subset(children,p20==FALSE)
# mean(nonp20.kids$whipple06,na.rm=TRUE)*500

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

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

indicator <- "SI.POV.NAHC"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

dat <- dat[c("iso3c","year","SI.POV.NAHC")]
names(dat) <- c("iso3","year","hc")
dat$hc <- dat$hc/100
dat <- dat[order(dat$year),]
dat <- dat[order(dat$iso3),]
colname <- "hc"
dat <- ddply(dat,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival 
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(dat) <- c("iso3","year","pl.hc","pl.hc.original")
countryMeta <- join(countryMeta,dat,by=c("iso3","year"))

newNames <- c("p20.rural"
              ,"p20.urban"
              ,"p80.rural"
              ,"p80.urban"
              ,"p80.over25.noeduc"
              ,"p80.over25.primary"
              ,"p80.over25.secondary"
              ,"p80.over25.higher"
              ,"p20.over25.noeduc"
              ,"p20.over25.primary"
              ,"p20.over25.secondary"
              ,"p20.over25.higher"
              ,"p80.o25.m.noeduc"
              ,"p80.o25.m.primary"
              ,"p80.o25.m.secondary"
              ,"p80.o25.m.higher"
              ,"p20.o25.m.noeduc"
              ,"p20.o25.m.primary"
              ,"p20.o25.m.secondary"
              ,"p20.o25.m.higher"
              ,"p80.o25.f.noeduc"
              ,"p80.o25.f.primary"
              ,"p80.o25.f.secondary"
              ,"p80.o25.f.higher"
              ,"p20.o25.f.noeduc"
              ,"p20.o25.f.primary"
              ,"p20.o25.f.secondary"
              ,"p20.o25.f.higher"
              ,"p20.male"
              ,"p20.female"
              ,"p80.male"
              ,"p80.female"
              ,"p20.male.head"
              ,"p20.female.head"
              ,"p80.male.head"
              ,"p80.female.head"
              ,"p80.unregistered"
              ,"p80.registered"
              ,"p20.unregistered"
              ,"p20.registered"
              ,"p80.notstunted"
              ,"p80.stunted"
              ,"p20.notstunted"
              ,"p20.stunted"
              ,"ext.rural"
              ,"ext.urban"
              ,"n.ext.rural"
              ,"n.ext.urban"
              ,"n.ext.over25.noeduc"
              ,"n.ext.over25.primary"
              ,"n.ext.over25.secondary"
              ,"n.ext.over25.higher"
              ,"ext.over25.noeduc"
              ,"ext.over25.primary"
              ,"ext.over25.secondary"
              ,"ext.over25.higher"
              ,"ext.male"
              ,"ext.female"
              ,"n.ext.male"
              ,"n.ext.female"
              ,"ext.male.head"
              ,"ext.female.head"
              ,"n.ext.male.head"
              ,"n.ext.female.head"
              ,"n.ext.unregistered"
              ,"n.ext.registered"
              ,"ext.unregistered"
              ,"ext.registered"
              ,"n.ext.notstunted"
              ,"n.ext.stunted"
              ,"ext.notstunted"
              ,"ext.stunted"
              ,"p20.male.stunted"
              ,"p20.female.stunted"
              ,"p20.male.notstunted"
              ,"p20.female.notstunted"
              ,"p80.male.stunted"
              ,"p80.female.stunted"
              ,"p80.male.notstunted"
              ,"p80.female.notstunted"
              ,"p80.maternal.deaths"
              ,"p80.no.maternal.deaths"
              ,"p20.maternal.deaths"
              ,"p20.no.maternal.deaths"
              ,"male.stunted"
              ,"female.stunted"
              ,"male.notstunted"
              ,"female.notstunted"
              ,"surveyed.pop"
              ,"surveyed.households"
              ,"surveyed.men"
              ,"surveyed.women"
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  surveyed.pop <- nrow(dat)
  surveyed.households <- length(unique(dat$household))
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  over25.male <- subset(over25,sex=="Male")
  over25.female <- subset(over25,sex=="Female")
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  surveyed.pop <- nrow(dat)
  countryMeta$surveyed.pop[which(countryMeta$filename==this.filename)] <- surveyed.pop
  surveyed.households <- nrow(unique(data.frame(dat)[c("cluster","household")]))
  countryMeta$surveyed.households[which(countryMeta$filename==this.filename)] <- surveyed.households
  surveyed.men <- nrow(men)
  countryMeta$surveyed.men[which(countryMeta$filename==this.filename)] <- surveyed.men
  surveyed.women <- nrow(women)
  countryMeta$surveyed.women[which(countryMeta$filename==this.filename)] <- surveyed.women
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
      subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$p80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Urban-ext
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Educ-P20
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$p20,over25$weights,this.pop.over25)
      countryMeta$p80.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Educ-ext
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$ext,over25$weights,this.pop.over25)
      countryMeta$n.ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Sex-ext
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-ext
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Maternal deaths
    dat$maternal <- dat$maternal.deaths > 0 
    if(length(dat$maternal[which(!is.na(dat$maternal))])!=0){
      confidence.tab <- pop.confidence(dat$maternal,dat$p20,dat$weights,this.pop)
      countryMeta$p80.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p80.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p20.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
      countryMeta$p20.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0}) 
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition p20
    under5$stunted <- (under5$child.height.age <= -2) & (under5$child.height.age > -6)
    under5$stunted[which(is.na(under5$stunting))] <- NA
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition ext
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$n.ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition by gender
    under5.male <- subset(under5,sex=="Male")
    under5.female <- subset(under5,sex=="Female")
    if(length(under5.male$stunted[which(!is.na(under5.male$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.male$stunted,under5.male$p20,under5.male$weights,this.pop.under5.male)
      countryMeta$p80.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0}) 
    }
    if(length(under5.female$stunted[which(!is.na(under5.female$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.female$stunted,under5.female$p20,under5.female$weights,this.pop.under5.female)
      countryMeta$p80.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Educ-P20 by gender
    if(length(over25$educ[which(!is.na(over25.male$educ))])!=0){
      confidence.tab <- pop.confidence(over25.male$educ,over25.male$p20,over25.male$weights,this.pop.over25.male)
      countryMeta$p80.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    if(length(over25$educ[which(!is.na(over25.female$educ))])!=0){
      confidence.tab <- pop.confidence(over25.female$educ,over25.female$p20,over25.female$weights,this.pop.over25.female)
      countryMeta$p80.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
  }
}

countryMeta$female.notstunted <- countryMeta$p80.female.notstunted+countryMeta$p20.female.notstunted
countryMeta$female.stunted <- countryMeta$p80.female.stunted+countryMeta$p20.female.stunted
countryMeta$male.notstunted <- countryMeta$p80.male.notstunted+countryMeta$p20.male.notstunted
countryMeta$male.stunted <- countryMeta$p80.male.stunted+countryMeta$p20.male.stunted

write.csv(countryMeta,"bycountry_tabs_2013.csv",row.names=FALSE,na="")