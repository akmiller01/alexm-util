list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2","scales","rgeos","maptools","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/iati-geo/output/"
setwd(wd)

agg <- fread("iati_unfiltered_agg.csv")
v1 = subset(agg,location_coordinates_lat!="")
v2 = subset(agg,location_point_pos!="")
rm(agg)

coordinates(v1)=~location_coordinates_long+location_coordinates_lat
plot(v1)

v2