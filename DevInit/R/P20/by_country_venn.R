library(data.table)
library(ggplot2)
library(plyr)
library(Hmisc)
require(gtools)
library(varhandle)

setwd("C:/Users/Alex/Documents/Data/P20/Meta")
load("total_tab_data.RData")

all.isos <- read.csv("C:/Users/Alex/Documents/Data/P20/Meta/all.isos.csv",na.strings="")
dat.filenames <- unique(data.frame(data.total)[c("filename")])
dat.filenames <- merge(dat.filenames,all.isos,all.x=TRUE)
dat.isos <- unfactor(unique(dat.filenames$iso2))

surveyless <- read.csv("C:/git/alexm-util/DevInit/P20-vis/venn/raw_povcalnet.csv",na.strings="",as.is=TRUE)
surveyless <- subset(surveyless,!(iso2 %in% dat.isos))
# data_total_blank = data.total[0,]
# surveyless_survey <- rbind(surveyless,data_total_blank,fill=TRUE)
data.total <- rbind(data.total,surveyless,fill=TRUE)

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)
countryMeta <- transform(countryMeta,
                         under5 = male.under5+female.under5
                         ,over25 = female.25.plus+male.25.plus
                         ,female.25.49 = female.25.plus-female.49.plus
                         )
# countryMeta_blank = data.table(countryMeta[0,])
# surveyless_meta <- rbind(surveyless,countryMeta_blank,fill=TRUE)
countryMeta <- data.frame(rbind(data.table(countryMeta),surveyless,fill=TRUE))

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

triple.cross <- function(x.vals,y.vals,z.vals,w,comb.v){
  if(length(w)>1){
    x = weighted.mean(x.vals,w,na.rm=TRUE)
    y = weighted.mean(y.vals,w,na.rm=TRUE)
    z = weighted.mean(z.vals,w,na.rm=TRUE)
    
    xy = weighted.mean(x.vals&y.vals,w,na.rm=TRUE)
    yz = weighted.mean(y.vals&z.vals,w,na.rm=TRUE)
    zx = weighted.mean(z.vals&x.vals,w,na.rm=TRUE)
    
    xyz = weighted.mean(x.vals&y.vals&z.vals,w,na.rm=TRUE) 
  }else{
    x = x.vals[1]
    y = y.vals[1]
    z = z.vals[1]
    
    if("p20" %in% comb.v[c(1,2)] && "non.p20" %in% comb.v[c(1,2)]){
      xy = 0
    }else if("ext" %in% comb.v[c(1,2)] && "non.p20" %in% comb.v[c(1,2)]){
      xy = 0
    }else 
      # if("p20" %in% comb.v[c(1,2)] && "ext" %in% comb.v[c(1,2)])
        {
      xy = min(x.vals[1],y.vals[1])
    }
    
    if("p20" %in% comb.v[c(2,3)] && "non.p20" %in% comb.v[c(2,3)]){
      yz = 0
    }else if("ext" %in% comb.v[c(2,3)] && "non.p20" %in% comb.v[c(2,3)]){
      yz = 0
    }else 
      # if("p20" %in% comb.v[c(2,3)] && "ext" %in% comb.v[c(2,3)])
        {
      yz = min(y.vals[1],z.vals[1])
    }
    
    if("p20" %in% comb.v[c(1,3)] && "non.p20" %in% comb.v[c(1,3)]){
      zx = 0
    }else if("ext" %in% comb.v[c(1,3)] && "non.p20" %in% comb.v[c(1,3)]){
      zx = 0
    }else 
      # if("p20" %in% comb.v[c(1,3)] && "ext" %in% comb.v[c(1,3)])
        {
      zx = min(x.vals[1],z.vals[1])
    }
    
    if(("p20" %in% comb.v || "ext" %in% comb.v) && "non.p20" %in% comb.v){
      xyz = 0
    }else{
      xyz = min(x.vals[1],y.vals[1],z.vals[1])  
    }
  }
  
  return(
    list(
      x = x
      ,y = y
      ,z = z
      ,xy = xy
      ,yz = yz
      ,zx = zx
      ,xyz = xyz
    )
  )
}

opposite = function(x){
  if(is.na(x)){
    return(NA)
  }else if(x==0){
    return(1) 
  }else{
    return(max(1-x,0))
  }
}

venn.data <- list()
venn.data.index <- 1

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  popset <- subset(countryMeta,filename==this.filename)[c("pop.total","under5","over25","female.25.49","female.15.49")]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  if(nrow(dat)>0){
    indicators <- data.frame(list(
      urban = dat$urban==1
      ,rural = !dat$urban
      ,no.educ = dat$educ=="No education, preschool"
      ,primary = dat$educ=="Primary" | dat$educ=="Secondary" | dat$educ=="Higher"
      ,secondary = dat$educ=="Secondary" | dat$educ=="Higher"
      ,higher = dat$educ=="Higher"
      ,under5 = dat$age<5
      ,fiveto14 = dat$age>=5&dat$age<15
      ,fifteento49 = dat$age>=15&dat$age<=49
      ,gt49 = dat$age>49
      ,male = dat$sex=="Male"
      ,female = dat$sex=="Female"
      ,p20 = dat$p20
      ,non.p20 = sapply(dat$p20,opposite)
      ,ext = dat$ext
      ,no.birth.reg = dat$birth.reg==0
      ,stunted = dat$stunting!="Not stunted"
      ,no.skilled.attendant = dat$skilled.births==0
      ,blank1 = TRUE
      ,blank2 = TRUE
    ))
    caveats <- data.frame(list(
      urban = TRUE
      ,rural = TRUE
      ,no.educ = dat$age>=25
      ,primary = dat$age>=25
      ,secondary = dat$age>=25
      ,higher = dat$age>=25
      ,under5 = TRUE
      ,fiveto14 = TRUE
      ,fifteento49 = TRUE
      ,gt49 = TRUE
      ,male = TRUE
      ,female = TRUE
      ,p20 = TRUE
      ,non.p20 = TRUE
      ,ext = TRUE
      ,no.birth.reg = dat$age<5
      ,stunted = dat$age<5
      ,no.skilled.attendant = dat$sex=="Female"&dat$age>=15&dat$age<=49&dat$all.births>0
      ,blank1 = TRUE
      ,blank2 = TRUE
      ,weights = !is.na(dat$weights)
    ))
    combs <- combinations(n=length(names(indicators)),r=3,v=names(indicators))
    for(j in 1:nrow(combs)){
      comb.name = paste(combs[j,],collapse="_")
      comb.v = c(combs[j,1],combs[j,2],combs[j,3])
      valid.rows = caveats[combs[j,]][,1]&caveats[combs[j,]][,2]&caveats[combs[j,]][,3]&caveats["weights"]
      valid.data <- indicators[which(valid.rows==TRUE),]
      valid.weights <- dat$weights[which(valid.rows==TRUE)]
      venn.segments <- triple.cross(valid.data[combs[j,1]][[1]],valid.data[combs[j,2]][[1]],valid.data[combs[j,3]][[1]],valid.weights,comb.v)
      venn.df <- data.frame(list(filename=this.filename,indicator1=combs[j,1],indicator2=combs[j,2],indicator3=combs[j,3],venn.segments))
      if("no.educ" %in% combs[j,] | 
         "primary" %in% combs[j,] | 
         "secondary" %in% combs[j,] | 
         "higher" %in% combs[j,]){
        if("no.skilled.attendant" %in% combs[j,]){
          pop <- popset$female.25.49[1]
        }else{
          pop <- popset$over25[1] 
        }
      }else if("no.birth.reg" %in% combs[j,] | 
               "stunted" %in% combs[j,]){
        pop <- popset$under5[1] 
      }else if("no.skilled.attendant" %in% combs[j,]){
        pop <- popset$female.15.49[1] 
      }else{
        pop <- popset$pop.total[1]
      }
      if(is.na(pop)){pop <- popset$pop.total[1]}
      venn.df$pop <- pop
      venn.data[[venn.data.index]] <- venn.df
      venn.data.index <- venn.data.index + 1
    }
  }
}

all.venn <- rbindlist(venn.data)
# world <- all.venn[,.(
#   x = weighted.mean(x,pop,na.rm=TRUE)
#   ,y = weighted.mean(y,pop,na.rm=TRUE)
#   ,z = weighted.mean(z,pop,na.rm=TRUE)
#   ,xy = weighted.mean(xy,pop,na.rm=TRUE)
#   ,yz = weighted.mean(yz,pop,na.rm=TRUE)
#   ,zx = weighted.mean(zx,pop,na.rm=TRUE)
#   ,xyz = weighted.mean(xyz,pop,na.rm=TRUE)
#   ,pop = sum(as.numeric(pop),na.rm=TRUE)
# ),by=.(indicator1,indicator2,indicator3)]
# world$filename <- "world"
# all.venn <- rbind(all.venn,world)

all.isos <- read.csv("C:/Users/Alex/Documents/Data/P20/Meta/all.isos.csv",na.strings="")
all.isos <- rbind(all.isos,data.table(surveyless),fill=TRUE)

all.isos <- data.frame(all.isos)[c("filename","iso2","year")]

all.venn.joined <- join(all.venn,all.isos,by="filename")
all.venn.joined$iso2 <- unfactor(all.venn.joined$iso2)
# all.venn.joined$iso2[which(all.venn.joined$filename=="world")] <- "WD"
# all.venn.joined$year[which(all.venn.joined$filename=="world")] <- weighted.mean(all.venn.joined$year,all.venn.joined$pop,na.rm=TRUE)

all.venn.joined <- transform(all.venn.joined,
                             x = round(x*100)
                             ,y = round(y*100)
                             ,z = round(z*100)
                             ,xy = round(xy*100)
                             ,yz = round(yz*100)
                             ,zx = round(zx*100)
                             ,xyz = round(xyz*100)
)
names(all.venn.joined) <- c("filename","indicator1","indicator2","indicator3","X","Y","Z","X,Y","Y,Z","X,Z","X,Y,Z","pop","iso2","year")

setwd("C:/git/alexm-util/DevInit/P20-vis/venn/")
# setwd("C:/Users/Alex/Documents/Data/P20/Meta/venn/")

write.csv(all.venn.joined,"all.venn.joined2.csv",row.names=FALSE,na="")

missing_x <- data.table(all.venn.joined)[,.(missing=mean(is.na(X))),by=.(iso2,indicator1)]
setnames(missing_x,"indicator1","indicator")
missing_y <- data.table(all.venn.joined)[,.(missing=mean(is.na(Y))),by=.(iso2,indicator2)]
setnames(missing_y,"indicator2","indicator")
missing_z <- data.table(all.venn.joined)[,.(missing=mean(is.na(Z))),by=.(iso2,indicator3)]
setnames(missing_z,"indicator3","indicator")

quote <- function(x){
  return(paste0("'",x,"'"))
}

missing_data <- rbindlist(list(missing_x,missing_y,missing_z))
missing_data <- subset(missing_data,missing==1)
missing_data <- unique(missing_data)
isos <- unique(missing_data$iso2)
missing_list <- list()
missing_index <- 1
for(iso in isos){
  dat <- subset(missing_data,iso2==iso)
  indicators <- unique(dat$indicator)
  indicators <- sapply(indicators,quote)
  indicators_joined <- paste(indicators,collapse=",")
  df <- data.frame(iso2=iso,indicators=indicators_joined)
  missing_list[[missing_index]] <- df
  missing_index <- missing_index +1
}
all_missing <- rbindlist(missing_list)
write.csv(all_missing,"missing_data.csv",row.names=FALSE,na="")

dat <- read.csv("all.venn.joined2.csv",na.strings="",check.names=FALSE)
isos <- unique(dat$iso2)
for(iso in isos){
  sub <- subset(dat,iso2==iso)
  sub$iso2 <- NULL
  sub$year <- NULL
  write.csv(sub,paste0("upload_individual/data/",iso,".csv"),na="",row.names=FALSE)
}
