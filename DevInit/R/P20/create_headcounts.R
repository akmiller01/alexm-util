library(data.table)

wd <- "D:/Documents/Data/P20_2013/meta"
setwd(wd)

pop <- read.csv("undesa.pop.csv")
cc <- read.csv("country-codes.csv")
cc <- cc[c("ISO3166.1.Alpha.3","ISO3166.1.numeric")]
names(cc) <- c("iso3","LocID")
pop <- subset(pop,Variant=="Medium" & Time==2013)
pop <- merge(pop,cc,by="LocID")
pop <- subset(pop,iso3!="")
pop$pop <- pop$Value*1000
pop$sexage <- paste0(pop$Sex,pop$AgeGrp)
setnames(pop,"Time","year")
pop <- pop[order(pop$iso3,pop$year,pop$AgeGrpStart,pop$Sex),]
pop <- pop[c("iso3","year","sexage","pop")]
pop <- reshape(pop,idvar="iso3",timevar="sexage",v.names="pop",direction="wide")

dat <- read.csv("raw_headcounts.csv")

ksv.hc <- subset(dat,iso3=="KSV")$hc

dat <- merge(dat,pop,by=c("iso3"))

dat["pop total"] <- rowSums(dat[grepl("Both",colnames(dat))])
dat["pop male"] <- rowSums(dat[grepl("Male",colnames(dat))])
dat["pop female"] <- rowSums(dat[grepl("Female",colnames(dat))])

dat["female under5"] <- dat["pop.Female0-4"]
dat["male under5"] <- dat["pop.Male0-4"]

dat["female 5-14"] <- rowSums(dat[c("pop.Female5-9","pop.Female10-14")])
dat["male 5-14"] <- rowSums(dat[c("pop.Male5-9","pop.Male10-14")])

dat["female 15-49"] <- rowSums(dat[c("pop.Female15-19","pop.Female20-24"
                                     ,"pop.Female25-29","pop.Female30-34"
                                     ,"pop.Female35-39","pop.Female40-44"
                                     ,"pop.Female45-49")])
dat["male 15-49"] <- rowSums(dat[c("pop.Male15-19","pop.Male20-24"
                                     ,"pop.Male25-29","pop.Male30-34"
                                     ,"pop.Male35-39","pop.Male40-44"
                                     ,"pop.Male45-49")])

dat["female 15-64"] <- rowSums(dat[c("female 15-49","pop.Female50-54"
                                     ,"pop.Female55-59","pop.Female60-64")])
dat["male 15-64"] <- rowSums(dat[c("male 15-49","pop.Male50-54"
                                     ,"pop.Male55-59","pop.Male60-64")])

dat["female 49 plus"] <- dat["pop female"] - rowSums(dat[c("female under5","female 5-14","female 15-49")])
dat["male 49 plus"] <- dat["pop male"] - rowSums(dat[c("male under5","male 5-14","male 15-49")])

dat["female 65 plus"] <- dat["pop female"] - rowSums(dat[c("female under5","female 5-14","female 15-64")])
dat["male 65 plus"] <- dat["pop male"] - rowSums(dat[c("male under5","male 5-14","male 15-64")])

dat["female 25 plus"] <- dat["pop female"] - rowSums(dat[c("female under5","female 5-14","pop.Female15-19","pop.Female20-24")])
dat["male 25 plus"] <- dat["pop male"] - rowSums(dat[c("male under5","male 5-14","pop.Male15-19","pop.Male20-24")])

kosovo <- subset(read.csv("old_headcounts.csv",check.names=FALSE),iso3=="KSV")
kosovo$year <- 2013
kosovo$hc <- ksv.hc
common.names <- intersect(names(kosovo),names(dat))
kosovo <- kosovo[common.names]
dat <- dat[common.names]

dat <- rbind(dat,kosovo)



filenames <- read.csv("old_headcounts.csv",as.is=TRUE)[c("iso3","filename")]
filenames$iso3[which(filenames$iso3=="TMP")] <- "TLS"
dat <- merge(dat,filenames,by="iso3")
dat <- dat[c(1,length(dat),2:(length(dat)-1))]
write.csv(dat,"headcounts.csv",na="",row.names=FALSE)
