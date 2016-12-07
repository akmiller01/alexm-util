library(data.table)
library(Hmisc)
library(foreign)

setwd("D:/Documents/Data/DHSauto/ugpr60dt")
dat <- read.dta("UGPR60FL.dta")
dat.labs <- data.frame(names(dat),attributes(dat)[7])
dat <- data.table(dat)

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

setnames(dat,"hv024","region")
dat$weights <- dat$hv005/1000000
setnames(dat,"hv025","urban.rural")
setnames(dat,"hv104","sex")

dat$wealth <- dat$hv271/100000
hc <- 0.5359
wealth.cut <- weighted.percentile(dat$wealth,dat$weights,hc)
dat$p20 <- dat$wealth<=wealth.cut

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==8 | x==9){return(NA)}
  else if(x==0){return("No education, preschool")}
  else if(x==1){return("Primary")}
  else if(x==2){return("Secondary")}
  else{return("Higher")}
}

dat$educ <- sapply(dat$hv106,recode.educ)
dat$educ <- factor(dat$educ,levels=c("No education, preschool","Primary","Secondary","Higher"))

setnames(dat,"hv105","age")


keep <- c("wealth","weights","region","urban.rural","sex","age","p20","educ")
dat <- data.frame(dat)
dat <- dat[keep]
dat$urban.sex <- paste(dat$urban.rural,dat$sex,sep=".")
dat$region.sex <- paste(dat$region,dat$sex,sep=".")

setwd("D:/Documents/P20 Visualizations/UG Disagg")
library(descr)
jpeg("p20.sex.jpg")
ct <- crosstab(dat$p20,dat$sex,weight=dat$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"p20.sex.csv")
jpeg("p20.urban.jpg")
ct <- crosstab(dat$p20,dat$urban.rural,weight=dat$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"p20.urban.csv")
jpeg("p20.region.jpg")
ct <- crosstab(dat$p20,dat$region,weight=dat$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"p20.region.csv")
jpeg("p20.urban.sex.jpg")
ct <- crosstab(dat$p20,dat$urban.sex,weight=dat$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"p20.urban.sex.csv")

dat.25 <- subset(dat,age>=25)
dat.25$true <- TRUE
jpeg("educ.national.jpg")
ct <- crosstab(dat.25$educ,dat.25$true,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.national.csv")


jpeg("educ.sex.jpg")
ct <- crosstab(dat.25$educ,dat.25$sex,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.sex.csv")
jpeg("educ.urban.jpg")
ct <- crosstab(dat.25$educ,dat.25$urban.rural,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.urban.csv")
jpeg("educ.region.jpg")
ct <- crosstab(dat.25$educ,dat.25$region,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.region.csv")
jpeg("educ.urban.sex.jpg")
ct <- crosstab(dat.25$educ,dat.25$urban.sex,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.urban.sex.csv")
jpeg("educ.region.sex.jpg")
ct <- crosstab(dat.25$educ,dat.25$region.sex,weight=dat.25$weights,prop.t=TRUE,drop.levels=FALSE)
dev.off()
props <- ct$prop.tbl
write.csv(props,"educ.region.sex.csv")
