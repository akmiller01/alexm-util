library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)

wd <- "D:/Documents/Data/MICSmeta"
setwd(wd)
load("child.maternal.RData")
valid.vacc <- c(TRUE,"TRUE","Yes","Oui","Sí")
no.vacc <- c(FALSE,"FALSE","No","Non","No sabe")
missing.vacc <- c("DK",NA,"Missing","NSP","Manquant")
recode.vacc <- function(x){
  if(is.na(x)){return(NA)}
  else if(x %in% valid.vacc){return(1)}
  else if(x %in% no.vacc){return(0)}
  else if(x %in% missing.vacc){return(NA)}
  return(NA)
}
child.health$vacc <- sapply(child.health$any.vacc,recode.vacc)


# Stop crosstab from plotting everything
options(descr.plot = FALSE)

setwd("D:/Documents/Data/P20_2013/meta")

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

conform <- function(mat1,mat2){
  mat1.row <- length(rownames(mat1))
  mat2.row <- length(rownames(mat2))
  mat1.col <- length(colnames(mat1))
  mat2.col <- length(colnames(mat2))
  if(mat1.row>mat2.row){
    mat2 <- pmax(mat2[match(rownames(mat1),rownames(mat2)),],na.rm=TRUE,0)
  }else if(mat1.row<mat2.row){
    mat1 <- pmax(mat1[match(rownames(mat2),rownames(mat1)),],na.rm=TRUE,0)
  }
  if(mat1.col>mat2.col){
    mat2 <- pmax(mat2[,match(colnames(mat1),colnames(mat2))],na.rm=TRUE,0)
  }else if(mat1.col<mat2.col){
    mat1 <- pmax(mat1[,match(colnames(mat2),colnames(mat1))],na.rm=TRUE,0)
  }
  return(
    mat1+mat2
  )
}


countryMeta <- read.csv("headcounts.csv",as.is=TRUE)

countryMeta$filename[which(countryMeta$filename=="bfhr70dt")] <- "bfhr62dt"
countryMeta$filename[which(countryMeta$filename=="kehr7hdt")] <- "kehr70dt"
countryMeta$filename[which(countryMeta$filename=="mdhr6hdt")] <- "mdhr51dt"
countryMeta$filename[which(countryMeta$filename=="mwhr71dt")] <- "mwhr61dt"
countryMeta$filename[which(countryMeta$filename=="tzhr6adt")] <- "tzhr63dt"
countryMeta$filename[which(countryMeta$filename=="ughr72dt")] <- "ughr60dt"

crossTabs <- list()

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
  this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
  ch.pop <- this.pop.under5.female + this.pop.under5.male
  # wm.pop <- subset(countryMeta,filename==this.filename)$female.15.49
  ch <- subset(child.health,filename==this.filename)
  # wm <- subset(maternal.health,filename==this.filename)
  if(nrow(ch)>0){
    #any.vacc
    if(length(ch$vacc[which(!is.na(ch$vacc))])!=0){
      confidence.tab <- pop.confidence(ch$vacc,ch$p20,ch$weights,ch.pop)
      if(is.null(crossTabs[["ch.vacc"]])){
        crossTabs[["ch.vacc"]] <- confidence.tab
      }else{
        crossTabs[["ch.vacc"]]$low <- conform(crossTabs[["ch.vacc"]]$low,confidence.tab$low)
        crossTabs[["ch.vacc"]]$estimate <- conform(crossTabs[["ch.vacc"]]$estimate,confidence.tab$estimate)
        crossTabs[["ch.vacc"]]$high <- conform(crossTabs[["ch.vacc"]]$high,confidence.tab$high)
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

saveWorkbook(wb, "child_vacc_popweighted.xlsx", overwrite = TRUE)