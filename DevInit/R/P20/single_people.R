library(Hmisc)
library(data.table)
library(descr)

setwd("C:/Users/Alex/Desktop/data/P20_2013/meta")
load("total_triple.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

hh.tab <- data.table(data.total)[,.(members=length(p20),has.elderly=max(age)>49,adults=sum(age>=18)),by=.(filename,cluster,household)]
hh.tab <- hh.tab[complete.cases(hh.tab),]
data.total <- merge(data.total,hh.tab)
single.hh <- subset(data.total,members==1)

crosstab(single.hh$ageCategory,single.hh$p20,prop.r=TRUE)
crosstab(single.hh$sex,single.hh$p20,prop.r=TRUE)

has.elderly = subset(data.total,has.elderly)
has.elderly$elderly.head <- has.elderly$head.age>49
crosstab(has.elderly$elderly.head,has.elderly$p20,prop.r=TRUE)

single.adult <- subset(data.total,adults==1)
crosstab(single.adult$head.sex,single.adult$p20,prop.r=TRUE)

crosstab(single.adult$head.ageCategory,single.adult$p20,prop.r=TRUE)

wd <- "C:/Users/Alex/Documents/Data/P20/Meta"
setwd(wd)

load("total_tab_data.RData")

data.total$whipple <- substr(as.character(data.total$age),nchar(as.character(data.total$age)),nchar(as.character(data.total$age)))
data.total$whipple05 <- data.total$whipple %in% c("0","5")
mean(data.total$whipple05,na.rm=TRUE)*500

p20 <- subset(data.total,p20==TRUE & age>=15 & age<=49)
mean(p20$whipple05,na.rm=TRUE)*500
nonp20 <- subset(data.total,p20==FALSE  & age>=15 & age<=49)
mean(nonp20$whipple05,na.rm=TRUE)*500

p20.elderly <- subset(data.total,p20==TRUE & age>49)
mean(p20.elderly$whipple05,na.rm=TRUE)*500
nonp20.elderly <- subset(data.total,p20==FALSE  & age>49)
mean(nonp20.elderly$whipple05,na.rm=TRUE)*500

children <- subset(data.total,!is.na(age.months))
children$whipple <- substr(as.character(children$age.months),nchar(as.character(children$age.months)),nchar(as.character(children$age.months)))
children$whipple06 <- children$whipple %in% c("0","6")
mean(children$whipple06,na.rm=TRUE)*500
p20.kids <- subset(children,p20==TRUE)
mean(p20.kids$whipple06,na.rm=TRUE)*500
nonp20.kids <- subset(children,p20==FALSE)
mean(nonp20.kids$whipple06,na.rm=TRUE)*500

water.dat <- subset(data.total,!is.na(water.time))
water.dat$water.time[which(water.dat$water.time>600)] <- NA
water.dat$elderly <- water.dat$age > 49
water.table <- data.table(water.dat)[,.(avg.time=mean(water.time,na.rm=TRUE)),by=.(ageCategory,p20)]
water.table$ageCategory <- factor(water.table$ageCategory,
                         levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                    ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                    ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                    ,"95+","missing"))
water.table <- subset(water.table,!is.na(p20))
water.table$P20 <- "Not in the P20"
water.table$P20[which(water.table$p20==TRUE)] <- "In the P20"
ggplot(water.table,aes(x=ageCategory,y=avg.time,group=P20,colour=P20)) + geom_point() + theme_bw() + xlab("Five-year age group") + ylab("Average time to fetch water")+ggtitle("Average time to fetch water by age group (DHS, various)")

stunt.dat <- subset(data.total,!is.na(stunting) & (age<5))
stunt.hh <- data.table(stunt.dat)[,.(stunted=sum(stunting!="Not stunted"),under5=sum(age<5)),by=.(filename,cluster,household)]
stunt.hh <- subset(stunt.hh,under5>0)
stunt.hh$perc <- stunt.hh$stunted/stunt.hh$under5
perc.tab <- data.frame(table(stunt.hh$perc))
perc.tab$perc <- perc.tab$Freq / sum(stunt.hh$under5)

dat <- data.table(data.total)[,.(p20=mean(p20,na.rm=TRUE)),by=.(ageCategory)]
dat$ageCategory <- factor(dat$ageCategory,
                            levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                               ,"95+","missing"))
library(ggplot2)
ggplot(dat,aes(x=ageCategory,y=p20)) + geom_point() + theme_bw() + xlab("Five-year age group") + ylab("Average P20 headcount")+ggtitle("Average P20 headcount by age group (DHS, MICS, PNAD, CFPS)")

totalWeights <- data.table(data.total)[,.(total.weights=sum(weights,na.rm=TRUE)),by=.(filename,p20)]
popWeights <- data.table(data.total)[,.(pop.weights=sum(weights,na.rm=TRUE)),by=.(filename)]
ageTable <- data.table(data.total)[,.(age.weights=sum(weights,na.rm=TRUE)),by=.(filename,p20,ageCategory)]
ageTable <- merge(ageTable,totalWeights,by=c("filename","p20"))
ageTable <- merge(ageTable,popWeights,by=c("filename"))
ageTable$weighted.percent.relative = ageTable$age.weights/ageTable$total.weights
ageTable$weighted.percent.absolute = ageTable$age.weights/ageTable$pop.weights
ageTable$ageCategory <- factor(ageTable$ageCategory,
                                  levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                             ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                             ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                             ,"95+","missing"))
ageTable <- ageTable[order(ageTable$filename,ageTable$p20,ageTable$ageCategory),]
write.csv(ageTable,"ageTable.csv",row.names=FALSE,na="")


p20.tab <- data.table(subset(data.total,!is.na(weights)))[,.(weighted.mean.p20=weighted.mean(p20,weights,na.rm=TRUE)),by=c("filename","ageCategory")]
p20.tab$ageCategory <- factor(p20.tab$ageCategory,
                               levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                          ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                          ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                          ,"95+","missing"))
p20.tab <- p20.tab[order(p20.tab$filename,p20.tab$ageCategory),]
write.csv(p20.tab,"p20.tab.csv",row.names=FALSE,na="")

educ.tab <- data.table(subset(data.total,!is.na(p20)))[,.(
  no.educ=mean(educ=="No education, preschool",na.rm=TRUE),
  primary=mean(educ=="Primary",na.rm=TRUE),
  secondary=mean(educ=="Secondary",na.rm=TRUE),
  higher=mean(educ=="Higher",na.rm=TRUE)
  ),by=c("p20","ageCategory")]

educ.tab$ageCategory <- factor(educ.tab$ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing"))
educ.tab <- educ.tab[order(educ.tab$p20,educ.tab$ageCategory),]
write.csv(educ.tab,"educ.tab.csv",row.names=FALSE,na="")
