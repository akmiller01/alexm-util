library(data.table)
library(readr)
library(reshape2)
library(Hmisc)
library(DataCombine)
library(quantmod)

wd <- "C:/Users/Alex/Documents/Data/GDELT"
setwd(wd)

colnames <- read.csv("C:/Users/Alex/Documents/Data/US data/2005/CSV.header.dailyupdates.txt", header = FALSE,
                     sep="\t",colClasses="character")

gdelt.list <- list()

today <- format(Sys.Date(), "%Y%m%d")

for(i in 1:30){
  yesterday <- format(Sys.Date()-i, "%Y%m%d")
  yestwd <- paste(wd,yesterday,sep="/")
  message(yesterday)
  if(file.exists(yestwd)){
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
  }
  else{
    dir.create(paste(wd,yesterday,sep="/"))
    setwd(yestwd)
    filename <- paste(yesterday,".export.CSV",sep="")
    zipname <- paste("http://data.gdeltproject.org/events/",
                     filename,".zip",sep="")
    zip <- tempfile()
    download.file(zipname,zip)
    unzip(zip, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
  }
  gdelt <- read_delim(filename,"\t",col_names=unlist(colnames[1,]))
  
  gdelt.list[[i]] <- gdelt
}

gdelt <- rbindlist(gdelt.list)
gdelt.tab <- data.table(gdelt)[,.(
  mentions = sum(NumMentions,na.rm=TRUE)
  ,avgTone=mean(AvgTone,na.rm=TRUE)
),by=.(SQLDATE,EventRootCode)]

mgdelt.tab <- melt(gdelt.tab,id.vars=c("SQLDATE","EventRootCode"),measure.vars=c("mentions","avgTone"))
wgdelt.tab <- dcast(mgdelt.tab,formula=SQLDATE~variable+EventRootCode)
wgdelt.tab[is.na(wgdelt.tab)] <- 0
wgdelt.tab$mentions <- rowSums(wgdelt.tab[,2:21])
wgdelt.tab$avgTone <- rowMeans(wgdelt.tab[,22:41])
wgdelt.tab[,2:21] <- wgdelt.tab[,2:21] / wgdelt.tab$mentions

wgdelt.tab$Date <- parse_date(wgdelt.tab$SQLDATE,format="%Y%m%d")
wgdelt.tab$SQLDATE <- NULL
gdelt <- wgdelt.tab
setwd(wd)
save(gdelt,file=paste0(today,"-",i,".RData"))

sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo",
           from = as.Date(min(gdelt$Date)), to = as.Date(max(gdelt$Date)))
gspc <- data.frame(get("GSPC", envir = sp500))
gspc$Date <- as.Date(rownames(gspc))
gspc <- slide(gspc,Var="GSPC.Open",slideBy=20)
gspc <- slide(gspc,Var="Date",slideBy=20)
gspc <- transform(gspc,pctChange = (GSPC.Open20-GSPC.Open)/GSPC.Open)

dat <- merge(gspc,gdelt,by="Date")

fit <- lm(pctChange~mentions_01+
            mentions_02+
            mentions_03+
            mentions_04+
            mentions_05+
            mentions_06+
            mentions_07+
            mentions_08+
            mentions_09+
            mentions_10+
            mentions_11+
            mentions_12+
            mentions_13+
            mentions_14+
            mentions_15+
            mentions_16+
            mentions_17+
            mentions_18+
            mentions_19+
            mentions_20+
            avgTone_01+
            avgTone_02+
            avgTone_03+
            avgTone_04+
            avgTone_05+
            avgTone_06+
            avgTone_07+
            avgTone_08+
            avgTone_09+
            avgTone_10+
            avgTone_11+
            avgTone_12+
            avgTone_13+
            avgTone_14+
            avgTone_15+
            avgTone_16+
            avgTone_17+
            avgTone_18+
            avgTone_19+
            avgTone_20
          ,data=dat)
summary(fit)
dat$yhat <- predict(fit,dat)
plot(yhat~pctChange,data=dat)
dat$directionCorrect = sign(dat$yhat)==sign(dat$pctChange)
mean(dat$directionCorrect,na.rm=TRUE)
dat$confidence <- round(abs(dat$yhat),3)
acc.tab <- data.table(dat)[,.(acc=mean(directionCorrect,na.rm=TRUE)),by="confidence"]
plot(acc.tab)

gdelt$yhat <- predict(fit,gdelt)
