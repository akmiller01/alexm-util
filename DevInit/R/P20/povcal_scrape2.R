library(Hmisc)
library(data.table)
library(plotly)
library(ggplot2)

wd <- "D:/Documents/Data/PovCal_Increment"
setwd(wd)

# header <- c(
#   "region"
#   ,"pl"
#   ,"hc"
#   ,"pg"
#   ,"pg2"
#   ,"poor"
#   ,"pop"
#   ,"svyCoverage"
#   ,"year"
# )
# 
# regions <- fread(
#   "regional.csv"
#   ,colClasses = c(
#     "character"
#     ,rep("numeric",8)
#     )
#   ,header=FALSE
#   ,col.names=header
#   ,na.strings=c("")
#   )
# 
# save(regions,file="regions.RData")

load("regions.RData")

total <- subset(regions,region=="Total of 6 regions")

#Remove implausible values
total <- total[!c(13637,22839),]
years <- c(2013, 2012, 2011, 2010, 2008, 2005, 2002, 1999, 1996, 1993, 1990, 1987, 1984, 1981)
for(i in years){
  filename <- paste("years_all/pcn",i,"csv",sep=".")
  write.csv(subset(total,year==i),filename,row.names=FALSE,na="")
}
total$log.pl <- log(total$pl)
plot_ly(total,x= ~year,y= ~log.pl,z= ~poor) %>% add_markers()
# plotly_POST(filename = "poverty-over-time")
plot_ly(subset(total,pl<20),x= ~year,y= ~pl,z= ~poor) %>% add_markers()
plotly_POST(filename = "poverty-over-time-trunc")
plot_ly(total,x= ~year,y= ~pl,z= ~poor) %>% add_markers()
plotly_POST(filename = "poverty-over-time-scale")

# library(raster)
# total_df <- data.frame(total)[c("year","log.pl","poor")]
# s100 <- as.matrix(total_df)
# # set up an 'empty' raster, here via an extent object derived from your data
# e <- extent(s100[,1:2])
# 
# r <- raster(e, ncol=length(unique(total$year)), nrow=length(unique(total$log.pl)))
# # or r <- raster(xmn=, xmx=,  ...
# 
# # you need to provide a function 'fun' for when there are multiple points per cell
# x <- rasterize(s100[, 1:2], r, s100[,3], fun=first)
# plot(x)
# total_mat <- as.matrix(x)
# year <- unique(total$year)[order(unique(total$year))]
# log.pl <- unique(total$log.pl)[order(unique(total$log.pl))]
# plot_ly(x = year, z= ~total_mat) %>% add_surface()
