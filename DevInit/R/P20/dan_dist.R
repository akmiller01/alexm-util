setwd("C:/Users/Alex/Documents/Data/P20/Meta")
load("total_tab_data.RData")

isos <- read.csv("all.isos.csv",as.is=TRUE)
isos <- isos[c("filename","iso3","year")]

keep.filenames <- c("afhr70dt","khhr72dt","Somalia MICS 2006 SPSS Datasets")

ss <- subset(data.total,filename %in% keep.filenames)

ss <- merge(ss,isos,by="filename")

# keep.isos <- c("KHM","HKG","PRK","NZL","SGP"
#           ,"BHR","KWT","LBY","OMN","QAT"
#           ,"SAU","ARE","AFG","ARG","CUB"
#           ,"PRI","GNQ","ERI","SOM")


keep.names <- c("iso3","year","wealth","urban","weights")

ss <- ss[keep.names]

write.csv(ss,"wealth_obs_dan.csv",row.names=FALSE,na="")
