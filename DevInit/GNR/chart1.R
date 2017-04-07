setwd("~/git/alexm-util/DevInit/GNR")
source("bar.R")
dat <- read.csv("1.csv",check.names=FALSE)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(prop, col=heat.colors(length(rownames(prop))), width=2, beside=TRUE)
legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(prop))), legend=rownames(data))