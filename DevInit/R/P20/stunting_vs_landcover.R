# install.packages("data.table", type = "source",
#                  repos = "http://Rdatatable.github.io/data.table")

library(data.table)

wd <- "D:/Documents/Data/WorldPop/UGA-POP"
setwd(wd)

libs <- c("rgdal", "maptools", "gridExtra","rgeos","raster")
lapply(libs, require, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

p20.count <- raster("../pov-tifs/p20.tif")

r <- raster("D:/Documents/Data/DHS map/total_native.tif")
r <- crop(r,extent(p20.count))
data <- data.table(coordinates(r))
data$z <- getValues(r)
dat <- data
coordinates(data) <- ~x+y
ug <- readOGR(dsn = "../voronoi/voronoi.shp", layer = "voronoi")
proj4string(data) <- proj4string(ug)

ug.data <- over(data,ug[,c("DHSCLUST","p20")])
data <- dat
ug.data <- cbind(data,ug.data)
ug.data <- data.table(ug.data)

save(ug.data,file="ug.data.RData")
ug.data$lc <- factor(ug.data$z)
fit <- lm(p20~lc,data=ug.data)
summary(fit)

# r <- raster("UGA_pph_v2b_2015_UNadj.tif")
# 
# data <- data.table(coordinates(r))
# data$z <- getValues(r)
# dat <- data
# coordinates(data) <- ~x+y
# ug <- readOGR(dsn = "../voronoi/voronoi.shp", layer = "voronoi")
# proj4string(data) <- proj4string(ug)
# 
# ug.data <- over(data,ug[,c("DHSCLUST","p20","p50","gp50")])
# data <- dat
# ug.data <- cbind(data,ug.data)
# ug.data <- data.table(ug.data)
# ug.data.tab <- ug.data[,c("pp20","pp50","pgp50") := list(round(z*p20),round(z*p50),round(z*gp50))]
# 
# p20 <- rasterFromXYZ(ug.data.tab[,c(1,2,8),with=FALSE])
# p50 <- rasterFromXYZ(ug.data.tab[,c(1,2,9),with=FALSE])
# gp50 <- rasterFromXYZ(ug.data.tab[,c(1,2,10),with=FALSE])
# writeRaster(p20,"../pov-tifs/p20.tif")
# writeRaster(p50,"../pov-tifs/p50.tif")
# writeRaster(gp50,"../pov-tifs/gp50.tif")

p20.count <- raster("../pov-tifs/p20.tif")

land_cover <- raster("D:/Documents/Data/DHS map/total_native.tif")
land_cover <- crop(land_cover,extent(p20.count))
lcXYZ <- data.table(coordinates(land_cover))
lcXYZ$lc <- getValues(land_cover)

p20.count <- resample(p20.count,land_cover)
dat <- data.table(coordinates(p20.count))
dat$p20 <- getValues(p20.count)

dat$lc <- factor(lcXYZ$lc)

fit <- lm(p20~lc,data=dat)
summary(fit)
