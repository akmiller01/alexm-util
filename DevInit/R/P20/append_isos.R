library(data.table)
library(plyr)
library(varhandle)

setwd("D:/Documents/P20 Visualizations")

all.dist <- read.csv("all.dist.csv")

all.isos <- read.csv("D:/Documents/Data/DHSmeta/all.isos.csv",na.strings="")

all.dist.joined <- join(all.dist,all.isos,by="filename")

write.csv(all.dist.joined,"all.dist.joined.csv",row.names=FALSE,na="")
