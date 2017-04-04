library(foreign)
library(Hmisc)

wd <- "/Users/Alex/Documents/Data/DHS/UGHR52DT"
setwd(wd)

dat <- read.dta("ughr52fl.dta")
var.labs <- data.frame(names(dat),attributes(dat)[7])

refugees <- subset(dat,hv023=="refugee camps")
dat$refugee <- dat$hv023=="refugee camps"

library(ggplot2)
ggplot(dat[order(dat$hv271),],aes(x=hv271,colour=refugee)) + geom_point(aes(y=c(1:length(dat$hv271))),alpha=0.5)

library(rgeos)
library(rgdal)
library(maptools)
ug <- readOGR("/Users/Alex/Documents/Data/DHS/sdr_subnational_boundaries_2017-04-03/shps/sdr_subnational_boundaries.shp")
points <- readOGR("/Users/Alex/Documents/Data/DHS/UGGE53FL/UGGE53FL.shp")
clusters <- unique(refugees$hv001)
ref.clusters <- subset(points,DHSCLUST %in% clusters)

plot(ug)
plot(ref.clusters,add=TRUE)
