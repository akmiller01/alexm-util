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
  if(filename=="phhr61dt"){
    dat <- read.dta(file
                    ,convert.factors=FALSE
    )
  }else{
    dat <- read.dta(file,convert.factors=FALSE)
  }
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
dhs.hr.common.labels <- subset(dhlw,count==5 & na.labels==0)
dhs.hr.data <- data.frame(dhs.hr.data)[c("filename",unfactor(dhs.hr.common.labels$var.name))]

dhs.prs <- c(
  "DHS/idpr63dt/IDPR63FL.DTA"
  ,"DHS/khpr72dt/KHPR72FL.DTA"
  ,"DHS/mmpr71dt/MMPR71FL.DTA"
  ,"DHS/phpr61dt/PHPR61FL.DTA"
)

dhs.pr.data.list <- list()
dhs.pr.labs.list <- list()
dhs.pr.index <- 1

for(file in dhs.prs){
  filename <- strsplit(file,"/")[[1]][2]
  message(filename)
  if(filename=="phpr61dt"){
    dat <- read.dta(file
                    ,convert.factors=FALSE
    )
  }else{
    dat <- read.dta(file,convert.factors=FALSE)
  }
  
  dat.labs <- data.frame(filename,var.name=names(dat),var.lab=attributes(dat)[7])
  dat$filename <- filename
  dhs.pr.data.list[[dhs.pr.index]] <- dat
  dhs.pr.labs.list[[dhs.pr.index]] <- dat.labs
  dhs.pr.index <- dhs.pr.index + 1
}

dhs.pr.data <- rbindlist(dhs.pr.data.list,fill=TRUE)
dhs.pr.labs <- rbindlist(dhs.pr.labs.list)

dpl <- melt(dhs.pr.labs,id.vars=c("var.name","filename"))
dplw <- dcast(dpl,var.name~filename+variable)
dplw$count <- rowSums(!is.na(dplw)*1)
dplw$na.labels <- rowSums(sapply(dplw,grepl,pattern="na -",ignore.case=TRUE) | sapply(dplw,grepl,pattern="na-",ignore.case=TRUE))
# dplw$dist1 <- stringdist(tolower(dplw$idpr63dt_var.labels),tolower(dplw$khpr72dt_var.labels))/pmax(nchar(dplw$idpr63dt_var.labels),nchar(dplw$khpr72dt_var.labels))
# dplw$dist2 <- stringdist(tolower(dplw$khpr72dt_var.labels),tolower(dplw$mmpr71dt_var.labels))/pmax(nchar(dplw$khpr72dt_var.labels),nchar(dplw$mmpr71dt_var.labels))
# dplw$dist3 <- stringdist(tolower(dplw$mmpr71dt_var.labels),tolower(dplw$phpr61dt_var.labels))/pmax(nchar(dplw$mmpr71dt_var.labels),nchar(dplw$phpr61dt_var.labels))
dplw$dist1 <- stringdist(tolower(dplw$idpr63dt_var.labels),tolower(dplw$khpr72dt_var.labels))
dplw$dist2 <- stringdist(tolower(dplw$khpr72dt_var.labels),tolower(dplw$mmpr71dt_var.labels))
dplw$dist3 <- stringdist(tolower(dplw$mmpr71dt_var.labels),tolower(dplw$phpr61dt_var.labels))
dplw$maxdist <- pmax(dplw$dist1,dplw$dist2,dplw$dist3)
dhs.pr.common.labels <- subset(dplw,count==5 & na.labels==0 & maxdist<12)
common.largedist <- subset(dplw,count==5 & na.labels==0 & maxdist>=12)
dhs.pr.data <- data.frame(dhs.pr.data)[c("filename",unfactor(dhs.pr.common.labels$var.name))]

mics.hhs <- c(
  "MICS/Lao People's Democratic Republic LSIS 2011-12 SPSS Datasets/hh.sav"
  ,"MICS/Thailand MICS 2012-13 SPSS Datasets/hh.sav"
  ,"MICS/Viet Nam MICS 2013-14 SPSS Datasets/hh.sav"
)

mics.hh.data.list <- list()
mics.hh.labs.list <- list()
mics.hh.index <- 1

for(file in mics.hhs){
  filename <- strsplit(file,"/")[[1]][2]
  message(filename)
  dat <- read.spss(file
                   # ,use.value.labels=FALSE
                   )
  dat.labs <- data.frame(filename,var.name=names(dat),var.lab=attributes(dat)$variable.labels)
  dat <- data.frame(dat)
  dat$filename <- filename
  mics.hh.data.list[[mics.hh.index]] <- dat
  mics.hh.labs.list[[mics.hh.index]] <- dat.labs
  mics.hh.index <- mics.hh.index + 1
}

mics.hh.data <- rbindlist(mics.hh.data.list,fill=TRUE)
mics.hh.labs <- rbindlist(mics.hh.labs.list)

mhl <- melt(mics.hh.labs,id.vars=c("var.name","filename"))
mhlw <- dcast(mhl,var.name~filename+variable)
mhlw$count <- rowSums(!is.na(mhlw)*1)
scrambled <- c("HC8F","HC8G","HC8H","HC8I","HC8J","HC8K")
mics.hh.common.labels <- subset(mhlw,count==4 & !(var.name %in% scrambled))
mics.hh.data <- data.frame(mics.hh.data)[c("filename",unfactor(mics.hh.common.labels$var.name))]

mics.hls <- c(
  "MICS/Lao People's Democratic Republic LSIS 2011-12 SPSS Datasets/hl.sav"
  ,"MICS/Thailand MICS 2012-13 SPSS Datasets/hl.sav"
  ,"MICS/Viet Nam MICS 2013-14 SPSS Datasets/hl.sav"
)

mics.hl.data.list <- list()
mics.hl.labs.list <- list()
mics.hl.index <- 1

for(file in mics.hls){
  filename <- strsplit(file,"/")[[1]][2]
  message(filename)
  dat <- read.spss(file
                   # ,use.value.labels=FALSE
                   )
  dat.labs <- data.frame(filename,var.name=names(dat),var.lab=attributes(dat)$variable.labels)
  dat <- data.frame(dat)
  dat$filename <- filename
  mics.hl.data.list[[mics.hl.index]] <- dat
  mics.hl.labs.list[[mics.hl.index]] <- dat.labs
  mics.hl.index <- mics.hl.index + 1
}

mics.hl.data <- rbindlist(mics.hl.data.list,fill=TRUE)
mics.hl.labs <- rbindlist(mics.hl.labs.list)

mll <- melt(mics.hl.labs,id.vars=c("var.name","filename"))
mllw <- dcast(mll,var.name~filename+variable)
mllw$count <- rowSums(!is.na(mllw)*1)
mics.hl.common.labels <- subset(mllw,count==4)
mics.hl.data <- data.frame(mics.hl.data)[c("filename",unfactor(mics.hl.common.labels$var.name))]

# write.csv(mics.hh.common.labels,"mics.hh.common.csv")
# write.csv(dhs.hr.common.labels,"dhs.hr.common.csv")
mapping <- list(
  "HH1"="hv001",
  "HH2"="hv002",
  "HH10"="hv003",
  "hhweight"="hv005",
  "HH5M"="hv006",
  "HH5Y"="hv007",
  "HH11"="hv009",
  "HH12"="hv010",
  "HH14"="hv014",
  "PSU"="hv021",
  "stratum"="hv022",
  "HH7"="hv024",
  "HH6"="hv025",
  "WS8"="hv205",
  "HC8A"="hv206",
  "HC8B"="hv207",
  "HC8E"="hv209",
  "HC9C"="hv210",
  "HC9D"="hv211",
  "HC3"="hv213",
  "HC5"="hv214",
  "HC4"="hv215",
  "HC2"="hv216",
  "HC8D"="hv221",
  "HC6"="hv226",
  "WS6"="hv237",
  "WS7A"="hv237a",
  "WS7B"="hv237b",
  "WS7C"="hv237c",
  "WS7D"="hv237d",
  "WS7E"="hv237e",
  "WS7F"="hv237f",
  "WS7X"="hv237x",
  "WS7Z"="hv237z",
  "HC7"="hv241",
  "HC9B"="hv243a",
  "windex5"="hv270",
  "wscore"="hv271"
)
keep <- c("filename")
for(i in 1:length(mapping)){
  mics.name <- names(mapping)[i]
  dhs.name <- mapping[[i]]
  keep <- c(keep,dhs.name)
  names(mics.hh.data)[which(names(mics.hh.data)==mics.name)] <- dhs.name
}

mics.hh.data <- mics.hh.data[keep]
mics.hh.data <- unfactor(mics.hh.data)
dhs.hr.data <- dhs.hr.data[keep]
asean.hh <- rbind(mics.hh.data,dhs.hr.data)

asean.weight.tab <- data.table(asean.hh)[,.(weight.sum=sum(hv005)),by=.(filename)]
#Populations in 2015
filename <- c(
  "Lao People's Democratic Republic LSIS 2011-12 SPSS Datasets"
  ,"Thailand MICS 2012-13 SPSS Datasets"
  ,"Viet Nam MICS 2013-14 SPSS Datasets"
  ,"idhr63dt"
  ,"khhr72dt"
  ,"mmhr71dt"
  ,"phhr61dt"
  )
pops <- c(
  6802000
  ,67960000
  ,91700000
  ,257600000
  ,15580000
  ,53900000
  ,100700000
)
asean.weight.tab$pop <- pops
asean.weight.tab$adj.factor <- asean.weight.tab$weight.sum/asean.weight.tab$pop
asean.hh <- merge(asean.hh,asean.weight.tab,by="filename")
asean.hh$weight <- asean.hh$hv005/asean.hh$adj.factor

source("C:/git/alexm-util/DevInit/R/P20/wealth_pca.R")

catvars = c(
  "hv205",
  "hv206",
  "hv207",
  "hv209",
  "hv210",
  "hv211",
  # "hv213",
  # "hv214",
  # "hv215",
  "hv221",
  "hv226",
  "hv237",
  "hv241",
  "hv243a"
  )

numvars = c(
  "hv216"
  )

asean.hh$urban = NA
asean.hh$urban[which(tolower(asean.hh$hv025) %in% c("1","municipal","urban"))] = 1
asean.hh$urban[which(tolower(asean.hh$hv025) %in% c("2","non-municipal","rural","rural with road","rural without road"))] = 0
urbanvar = "urban"
save(asean.hh,catvars,numvars,urbanvar,file="pca_prep.RData")

asean.hh$missingCount = rowSums(is.na(asean.hh[c(catvars,numvars)]))
asean.hh = subset(asean.hh,missingCount<23 & weight>0)

asean.hh.wealth <- wealth(asean.hh,catvars,numvars,urbanvar)
save(asean.hh.wealth,file="asean.hh.wealth.Rdata")

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

r20 <- weighted.percentile(asean.hh.wealth$wealth,asean.hh.wealth$weight,.2)
asean.hh.wealth$r20 <- asean.hh.wealth$wealth <= r20
r20.tab <- data.table(asean.hh.wealth)[,.(
  r20=weighted.mean(r20,weight)
  ,avg.wealth=weighted.mean(wealth,weight)
  ),by=.(filename)]

View(r20.tab)

for(name in filename){
  print(name)
  print(describe(subset(asean.hh.wealth,filename==name)$wealth))
}

laos <- subset(asean.hh.wealth,filename=="Lao People's Democratic Republic LSIS 2011-12 SPSS Datasets")
View(laos[c(catvars,numvars,"wealth","r20","weight")])
indo <- subset(asean.hh.wealth,filename=="idhr63dt")
View(indo[c(catvars,numvars,"wealth","r20","weight")])

# Woman BMI
# Electrification
# Toilets
# Schooling for 18+

