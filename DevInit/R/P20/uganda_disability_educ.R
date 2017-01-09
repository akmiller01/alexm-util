library(Hmisc)
library(foreign)
library(descr)

setwd("D:/Documents/Data/DHSauto/ugpr60dt")

data <- read.dta("UGPR60FL.dta")
labs <- data.frame(names(data),attributes(data)$var.labels)

data$weights <- data$hv005/1000000

data$educ <- NA
data$educ[which(data$hv106==0)] <- "No education, preschool"
data$educ[which(data$hv106==1)] <- "Primary"
data$educ[which(data$hv106==2)] <- "Secondary"
data$educ[which(data$hv106==3)] <- "Higher"

data$educ <- factor(data$educ,levels=c("No education, preschool","Primary","Secondary","Higher"))

data$seeing <- NA
data$seeing[which(data$sh24==1)] <- "No difficulty"
data$seeing[which(data$sh24==2)] <- "Some difficulty"
data$seeing[which(data$sh24==3)] <- "A lot of difficulty"
data$seeing[which(data$sh24==4)] <- "Cannot do at all"

data$seeing <- factor(data$seeing,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

data$hearing <- NA
data$hearing[which(data$sh25==1)] <- "No difficulty"
data$hearing[which(data$sh25==2)] <- "Some difficulty"
data$hearing[which(data$sh25==3)] <- "A lot of difficulty"
data$hearing[which(data$sh25==4)] <- "Cannot do at all"

data$hearing <- factor(data$hearing,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

data$walking <- NA
data$walking[which(data$sh26==1)] <- "No difficulty"
data$walking[which(data$sh26==2)] <- "Some difficulty"
data$walking[which(data$sh26==3)] <- "A lot of difficulty"
data$walking[which(data$sh26==4)] <- "Cannot do at all"

data$walking <- factor(data$walking,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

data$remembering <- NA
data$remembering[which(data$sh27==1)] <- "No difficulty"
data$remembering[which(data$sh27==2)] <- "Some difficulty"
data$remembering[which(data$sh27==3)] <- "A lot of difficulty"
data$remembering[which(data$sh27==4)] <- "Cannot do at all"

data$remembering <- factor(data$remembering,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

data$self.care <- NA
data$self.care[which(data$sh28==1)] <- "No difficulty"
data$self.care[which(data$sh28==2)] <- "Some difficulty"
data$self.care[which(data$sh28==3)] <- "A lot of difficulty"
data$self.care[which(data$sh28==4)] <- "Cannot do at all"

data$self.care <- factor(data$self.care,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

data$communicating <- NA
data$communicating[which(data$sh29==1)] <- "No difficulty"
data$communicating[which(data$sh29==2)] <- "Some difficulty"
data$communicating[which(data$sh29==3)] <- "A lot of difficulty"
data$communicating[which(data$sh29==4)] <- "Cannot do at all"

data$communicating <- factor(data$communicating,levels=c("No difficulty","Some difficulty","A lot of difficulty","Cannot do at all"))

options(descr.plot = FALSE)
crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$educ,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$educ,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$educ,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$educ,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$educ,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$educ,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

library(openxlsx)

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_Education.xlsx", overwrite = TRUE)

data$urban <- NA
data$urban[which(data$hv025=="urban")] <- "Urban"
data$urban[which(data$hv025=="rural")] <- "Rural"

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$urban,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$urban,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$urban,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$urban,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$urban,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$urban,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_Urban.xlsx", overwrite = TRUE)

data$ageCat <- NA
data$ageCat[which(data$hv105>=5 & data$hv105<15)] <- "5 to 14"
data$ageCat[which(data$hv105>=15 & data$hv105<49)] <- "15 to 49"
data$ageCat[which(data$hv105>=49)] <- "49 and above"
data$ageCat <- factor(data$ageCat,levels=c("5 to 14","15 to 49","49 and above"))

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$ageCat,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$ageCat,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$ageCat,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$ageCat,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$ageCat,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$ageCat,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

# #Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_Age_Category.xlsx", overwrite = TRUE)

data$wealth <- data$hv271/100000
povcut <- 0.5359
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
povwealth <- weighted.percentile(data$wealth,data$weights,prob=povcut)
data$p20 <- NA
data$p20[which(data$wealth<=povwealth)] <- "P20"
data$p20[which(data$wealth>povwealth)] <- "Non-P20"

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$p20,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$p20,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$p20,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$p20,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$p20,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$p20,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

# #Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_P20_status.xlsx", overwrite = TRUE)

data$water <- NA
private <- c("borehole in yard/plot","protected well/spring in yard/plot","piped into dwelling","piped to yard/plot","bottled water")
public <- c("public borehole","protected public well/spring","public tap/standpipe")
other.inadequate <- c("unprotected well/spring in yard/plot","unprotected public well/spring","river/stream","pond/lake","dam","rainwater","tanker truck","vendor: cart with small tank")

data$water[which(data$hv201 %in% private)] <- "Private piped/well/bottled source"
data$water[which(data$hv201 %in% public)] <- "Public piped/well source"
data$water[which(data$hv201 %in% other.inadequate)] <- "Unprotected / inadequate source"

data$water <- factor(data$water,levels=c("Private piped/well/bottled source","Public piped/well source","Unprotected / inadequate source"))

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$water,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$water,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$water,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$water,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$water,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$water,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_Water_source.xlsx", overwrite = TRUE)

data$toilet <- NA
flush <- c(10,11,41,44)
imp.latrine <- c(21,22,23)
unimp.latrine <- c(20,24,25,43)
no.toilet <- c(30,31)

data$toilet[which(data$hv205 %in% flush)] <- "Flush/composting toilet"
data$toilet[which(data$hv205 %in% imp.latrine)] <- "Improved latrine"
data$toilet[which(data$hv205 %in% unimp.latrine)] <- "Unimproved latrine"
data$toilet[which(data$hv205 %in% no.toilet)] <- "No facility"

data$toilet <- factor(data$toilet,levels=c("Flush/composting toilet","Improved latrine","Unimproved latrine","No facility"))

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$toilet,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$toilet,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$toilet,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$toilet,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$toilet,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$toilet,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_toilet.xlsx", overwrite = TRUE)

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$hv219,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$hv219,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$hv219,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$hv219,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$hv219,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$hv219,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_head_sex.xlsx", overwrite = TRUE)

head <- subset(data,hv101=="head")
head$head.educ <- head$educ
keep <- c("hhid","head.educ")
head <- head[keep]
library(plyr)
data <- join(data,head,by="hhid")

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$head.educ,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$head.educ,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$head.educ,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$head.educ,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$head.educ,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$head.educ,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_head_educ.xlsx", overwrite = TRUE)

crossTabs <- list()
crossTabs[["Difficulty seeing"]] <- crosstab(data$seeing,data$hv024,weight=data$weights)$tab
crossTabs[["Difficulty hearing"]] <- crosstab(data$hearing,data$hv024,weight=data$weights)$tab
crossTabs[["Difficulty walking"]] <- crosstab(data$walking,data$hv024,weight=data$weights)$tab
crossTabs[["Difficulty remembering"]] <- crosstab(data$remembering,data$hv024,weight=data$weights)$tab
crossTabs[["Difficulty with self care"]] <- crosstab(data$self.care,data$hv024,weight=data$weights)$tab
crossTabs[["Difficulty communicating"]] <- crosstab(data$communicating,data$hv024,weight=data$weights)$tab

setwd("D:/Documents/Data/DHSmeta2")

#Create workbook
# wb <- createWorkbook("crosstabs")
# 
# crossNames <- names(crossTabs)
# for(i in 1:length(crossNames)){
#   crossName <- crossNames[i]
#   crossTab <- crossTabs[[i]]
#   addWorksheet(wb,crossName)
#   writeData(wb,sheet=crossName,crossTab,colNames=TRUE,rowNames=TRUE)
# }
# 
# saveWorkbook(wb, "Uganda_Disability_by_region.xlsx", overwrite = TRUE)

data$water.time <- data$hv204
data$water.time[which(data$hv204 %in% c(996,998))] <- NA
library(data.table)
data.tab <- data.table(data) 
water.tab <- data.tab[,.(average.ttw=weighted.mean(water.time,weights,na.rm=TRUE)),by=.(walking)]

data$female <- NA
data$female[which(data$hv104=="female")] <- 1
data$female[which(data$hv104=="male")] <- 0
blind <- subset(data,seeing=="Cannot do at all")
weighted.mean(blind$female,blind$weights,na.rm=TRUE)

# sexuality <- subset(data,(hv104==hv219 & hv101 %in% c("wife or husband","co-spouse")))
# sexuality.hh <- subset(data,hhid %in% sexuality$hhid)
# View(sexuality.hh[c("hhid","hv104","hv219","hv101","hv217")])
# write.csv(sexuality.hh[c("hhid","hv104","hv219","hv101","hv217")],"sexuality.csv",row.names=FALSE)
