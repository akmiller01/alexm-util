library(Hmisc)
library(data.table)
library(descr)

setwd("C:/Users/Alex/Desktop/data/P20_2013/meta")
load("total_triple.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))

hh.tab <- data.table(data.total)[,.(members=length(p20),has.elderly=max(age)>49),by=.(filename,cluster,household)]
hh.tab <- hh.tab[complete.cases(hh.tab),]
data.total <- merge(data.total,hh.tab)
single.hh <- subset(data.total,members==1)

crosstab(single.hh$ageCategory,single.hh$p20,prop.r=TRUE)

has.elderly = subset(data.total,has.elderly)
has.elderly$elderly.head <- has.elderly$head.age>49
crosstab(has.elderly$elderly.head,has.elderly$p20,prop.r=TRUE)
