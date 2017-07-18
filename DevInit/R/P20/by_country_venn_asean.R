library(data.table)
library(ggplot2)
library(plyr)
library(Hmisc)
require(gtools)
library(varhandle)

setwd("C:/Users/Alex/Documents/Data/P20/Meta")
load("asean_tab_data.RData")
asean <- c(
  "idhr63dt"
  ,"khhr72dt"
  ,"mmhr71dt"
  ,"phhr61dt"
  ,"Lao People's Democratic Republic_LSIS_Datasets"
  ,"Thailand_MICS4_Datasets"
  ,"Viet Nam_MICS5_Datasets"
)
data.total <- subset(data.total,filename %in% asean)

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

write.csv(data.total,"asean_microdata.csv",na="",row.names=FALSE)

countryMeta <- read.csv("headcounts.csv",as.is=TRUE)
countryMeta <- subset(countryMeta,filename %in% asean)
countryMeta <- transform(countryMeta,
                         under5 = male.under5+female.under5
                         ,over15 = female.15.49+female.49.plus+male.15.49+male.49.plus
                         ,over25 = female.25.plus+male.25.plus
                         ,female.25.49 = female.25.plus-female.49.plus
                         )

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

# filenames <- countryMeta$filename
filenames <- asean
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  popset <- subset(countryMeta,filename==this.filename)
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
      ,no.educ.15 = dat$educ=="No education, preschool" #All 15 educs are new
      ,primary.15 = dat$educ=="Primary" | dat$educ=="Secondary" | dat$educ=="Higher" #New
      ,secondary.15 = dat$educ=="Secondary" | dat$educ=="Higher" #New
      ,higher.15 = dat$educ=="Higher" #New
      ,under5 = dat$age<5
      ,fiveto14 = dat$age>=5&dat$age<15
      ,fifteento49 = dat$age>=15&dat$age<=49
      ,gt49 = dat$age>49
      ,male = dat$sex=="Male"
      ,female = dat$sex=="Female"
      ,asean.r20 = dat$asean.r20 #New, replacing P20
      ,non.asean.r20 = sapply(dat$asean.r20,opposite) #New, replacing not P20
      ,ext = dat$ext
      ,np20 = dat$np20
      ,no.birth.reg = dat$birth.reg==0
      ,stunted = dat$stunting!="Not stunted"
      ,no.skilled.attendant = dat$skilled.births==0
      ,toilet.unimproved = dat$toilet.unimproved==1 #New
      ,electricity = dat$electricity==1 #New
      ,fridge = dat$fridge==1 #New
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
      ,no.educ.15 = dat$age>=15
      ,primary.15 = dat$age>=15
      ,secondary.15 = dat$age>=15
      ,higher.15 = dat$age>=15
      ,under5 = TRUE
      ,fiveto14 = TRUE
      ,fifteento49 = TRUE
      ,gt49 = TRUE
      ,male = TRUE
      ,female = TRUE
      ,asean.r20 = TRUE
      ,non.asean.r20 = TRUE
      ,ext = TRUE
      ,np20 = TRUE
      ,no.birth.reg = dat$age<5
      ,stunted = dat$age<5
      ,no.skilled.attendant = dat$sex=="Female"&dat$age>=15&dat$age<=49&dat$all.births>0
      ,toilet.unimproved = TRUE
      ,electricity = TRUE
      ,fridge = TRUE
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
      }else if("no.educ.15" %in% combs[j,] | 
               "primary.15" %in% combs[j,] | 
               "secondary.15" %in% combs[j,] | 
               "higher.15" %in% combs[j,]){
        if("no.skilled.attendant" %in% combs[j,]){
          pop <- popset$female.15.49[1]
        }else{
          pop <- popset$over15[1] 
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

all.isos <- read.csv("C:/Users/Alex/Documents/Data/P20/Meta/all.isos.csv",na.strings="")

all.isos <- data.frame(all.isos)[c("filename","iso2","year")]

all.venn.joined <- join(all.venn,all.isos,by="filename")
all.venn.joined$iso2 <- unfactor(all.venn.joined$iso2)

all.venn.joined <- transform(all.venn.joined,
                             x = round(x*100)
                             ,y = round(y*100)
                             ,z = round(z*100)
                             ,xy = round(xy*100)
                             ,yz = round(yz*100)
                             ,zx = round(zx*100)
                             ,xyz = round(xyz*100)
)
# all.venn.joined <- transform(all.venn.joined,
#                              x = x*100
#                              ,y = y*100
#                              ,z = z*100
#                              ,xy = xy*100
#                              ,yz = yz*100
#                              ,zx = zx*100
#                              ,xyz = xyz*100
# )
names(all.venn.joined) <- c("filename","indicator1","indicator2","indicator3","X","Y","Z","X,Y","Y,Z","X,Z","X,Y,Z","pop","iso2","year")

setwd("C:/Users/Alex/Documents/Data/P20/Meta/venn/")

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
