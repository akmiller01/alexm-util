# install.packages("WDI")
# install.packages("reshape2")

library(WDI)
library(reshape2)

gg <- WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG",start=2000,end=2015,extra=TRUE)
keep <- c(3:5)
gg <- gg[keep]
ggm <- melt(gg,id.vars=c("iso3c","year"))
ggw <- dcast(ggm,iso3c+variable~year,fun.aggregate=mean)

ggw$average <- rowMeans(ggw[,3:18],na.rm=TRUE)
