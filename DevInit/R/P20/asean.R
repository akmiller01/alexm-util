library(foreign)
library(data.table)
library(reshape2)
library(varhandle)
library(stringdist)

wd <- "C:/Users/Alex/Documents/Data/ASEAN"
setwd(wd)

dhs.hrs <- c(
  "DHS/idhr63dt/IDHR63FL.DTA"
  ,"DHS/khhr72dt/KHHR72FL.DTA"
  ,"DHS/mmhr71dt/MMHR71FL.DTA"
  ,"DHS/phhr61dt/PHHR61FL.DTA"
  )

dhs.hr.data.list <- list()
dhs.hr.labs.list <- list()
dhs.hr.index <- 1

for(file in dhs.hrs){
  filename <- strsplit(file,"/")[[1]][2]
  message(filename)
  dat <- read.dta(file,convert.factors=FALSE)
  dat.labs <- data.frame(filename,var.name=names(dat),var.lab=attributes(dat)[7])
  dat$filename <- filename
  dhs.hr.data.list[[dhs.hr.index]] <- dat
  dhs.hr.labs.list[[dhs.hr.index]] <- dat.labs
  dhs.hr.index <- dhs.hr.index + 1
}

dhs.hr.data <- rbindlist(dhs.hr.data.list,fill=TRUE)
dhs.hr.labs <- rbindlist(dhs.hr.labs.list)

dhl <- melt(dhs.hr.labs,id.vars=c("var.name","filename"))
dhlw <- dcast(dhl,var.name~filename+variable)
dhlw$count <- rowSums(!is.na(dhlw)*1)
dhlw$na.labels <- rowSums(sapply(dhlw,grepl,pattern="na -",ignore.case=TRUE) | sapply(dhlw,grepl,pattern="na-",ignore.case=TRUE))
common.labels <- subset(dhlw,count==5 & na.labels==0)
dhs.hr.data <- data.frame(dhs.hr.data)[c("filename",unfactor(common.labels$var.name))]

dhs.irs <- c(
  "DHS/idir63dt/IDIR63FL.DTA"
  ,"DHS/khir72dt/KHIR72FL.DTA"
  ,"DHS/mmir71dt/MMIR71FL.DTA"
  ,"DHS/phir61dt/PHIR61FL.DTA"
)

dhs.ir.data.list <- list()
dhs.ir.labs.list <- list()
dhs.ir.index <- 1

for(file in dhs.irs){
  filename <- strsplit(file,"/")[[1]][2]
  message(filename)
  dat <- read.dta(file,convert.factors=FALSE)
  dat.labs <- data.frame(filename,var.name=names(dat),var.lab=attributes(dat)[7])
  dat$filename <- filename
  dhs.ir.data.list[[dhs.ir.index]] <- dat
  dhs.ir.labs.list[[dhs.ir.index]] <- dat.labs
  dhs.ir.index <- dhs.ir.index + 1
}

dhs.ir.data <- rbindlist(dhs.ir.data.list,fill=TRUE)
dhs.ir.labs <- rbindlist(dhs.ir.labs.list)

dil <- melt(dhs.ir.labs,id.vars=c("var.name","filename"))
dilw <- dcast(dil,var.name~filename+variable)
dilw$count <- rowSums(!is.na(dilw)*1)
dilw$na.labels <- rowSums(sapply(dilw,grepl,pattern="na -",ignore.case=TRUE) | sapply(dilw,grepl,pattern="na-",ignore.case=TRUE))
# dilw$dist1 <- stringdist(tolower(dilw$idir63dt_var.labels),tolower(dilw$khir72dt_var.labels))/pmax(nchar(dilw$idir63dt_var.labels),nchar(dilw$khir72dt_var.labels))
# dilw$dist2 <- stringdist(tolower(dilw$khir72dt_var.labels),tolower(dilw$mmir71dt_var.labels))/pmax(nchar(dilw$khir72dt_var.labels),nchar(dilw$mmir71dt_var.labels))
# dilw$dist3 <- stringdist(tolower(dilw$mmir71dt_var.labels),tolower(dilw$phir61dt_var.labels))/pmax(nchar(dilw$mmir71dt_var.labels),nchar(dilw$phir61dt_var.labels))
dilw$dist1 <- stringdist(tolower(dilw$idir63dt_var.labels),tolower(dilw$khir72dt_var.labels))
dilw$dist2 <- stringdist(tolower(dilw$khir72dt_var.labels),tolower(dilw$mmir71dt_var.labels))
dilw$dist3 <- stringdist(tolower(dilw$mmir71dt_var.labels),tolower(dilw$phir61dt_var.labels))
dilw$maxdist <- pmax(dilw$dist1,dilw$dist2,dilw$dist3)
common.labels <- subset(dilw,count==5 & na.labels==0 & maxdist<12)
common.largedist <- subset(dilw,count==5 & na.labels==0 & maxdist>=12)
dhs.ir.data <- data.frame(dhs.ir.data)[c("filename",unfactor(common.labels$var.name))]
