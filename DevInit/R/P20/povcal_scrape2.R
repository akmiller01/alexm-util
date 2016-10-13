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
total$log.pl <- log(total$pl)
plot_ly(total,x= ~year,y= ~log.pl,z= ~poor) %>% add_markers()
plotly_POST(filename = "poverty-over-time")
