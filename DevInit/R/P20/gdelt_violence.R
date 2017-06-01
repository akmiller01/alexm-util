library(plyr)
library(data.table)

wd <-"C:/Users/Alex/Documents/R/GDELT/"
setwd(wd)
colnames <- read.csv("CSV.header.dailyupdates.csv", header = FALSE,
                     sep="\t",colClasses="character")
data.list <- list()
data.index <- 1
for(i in 1:10){
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
  data <- read.csv(filename, header = FALSE,
                   sep="\t",colClasses="character",
                   col.names = colnames)
  data.list[[data.index]] <- data
  data.index <- data.index + 1
}

reports <- rbindlist(data.list)

df <- ddply(reports,.(SQLDATE,EventRootCode)
            ,function(x){
              return(nrow(x))
            })
names(df)[names(df)=="V1"] <- "value"

#Plot

dat <- subset(df,as.numeric(substr(SQLDATE,1,8))>=as.numeric(yesterday))
dat <- ddply(dat,.(SQLDATE),function(x){
  protests <- 0
  military <- 0
  other <- 0
  for(i in 1:nrow(x)){
    if(x$EventRootCode[i]=="14"){
      protests <- protests + x$value[i]
    }
    else if(x$EventRootCode[i]=="19"){
      military <- military + x$value[i]
    }
    else{
      other <- other + x$value[i]
    }
  }
  y <- data.frame(protests,military,other)
  return(y)
})
dat <- transform(dat,percentProtest=(protests/(other+protests+military))*100,percentMilitary=(military/(other+protests+military))*100)
names(dat)[names(dat)=="SQLDATE"] <- "date"
dat <- dat[c("date","percentProtest","percentMilitary")]
longdf <- reshape(dat
                  ,idvar="date"
                  ,direction="long"
                  ,varying=c("percentProtest","percentMilitary")
                  ,times = c("Reports on Political Dissent","Reports on Military Force")
                  ,v.names="value"
)
names(longdf)[which(names(longdf)=="time")]<-"type"
library(ggplot2)

ggplot(longdf,aes(x=date,y=value,group=type,fill=type)) + geom_bar(stat="identity")
