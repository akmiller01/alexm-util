library(data.table)
library(plyr)

setwd("D:/Documents/P20 Visualizations")

all.dist <- read.csv("all.dist.csv")
all.venn <- read.csv("all.venn.csv")

all.isos <- read.csv("D:/Documents/Data/DHSmeta/all.isos.csv",na.strings="")

all.dist.joined <- join(all.dist,all.isos,by="filename")
all.venn.joined <- join(all.venn,all.isos,by="filename")
write.csv(all.dist.joined,"all.dist.joined.csv",row.names=FALSE,na="")
write.csv(all.venn.joined,"all.venn.joined.csv",row.names=FALSE,na="")
