library(readr)

url <- "https://www.imf.org/external/pubs/ft/weo/2017/02/weodata/WEOOct2017all.xls"

temp <- tempfile(fileext=".csv")
download.file(url,temp)
dat = read_delim(temp,delim="\t",na=c("n/a","--"),trim_ws=TRUE)
names(dat) <- make.names(names(dat))
ngd <- subset(dat,WEO.Subject.Code=="NGDPRPPPPC")



###Make long


keep <- c(2,10:52)
ngd <- ngd[keep]
names(ngd) <- c("iso3c",paste0("ngd.",1980:2022))

library(reshape)

ngd.long <- reshape(ngd,varying=c(2:ncol(ngd)),direction="long",timevar="year")
