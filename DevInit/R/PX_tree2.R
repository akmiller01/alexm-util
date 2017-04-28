library(data.table)
library(Hmisc)
library(plyr)

wd <- "D:/Documents/Data/EX/join"
setwd(wd)

files <- list.files(wd,pattern="*.csv",full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(file in files){
  dataList[[dataIndex]] <- read.csv(file,na.strings="",as.is=TRUE)
  message(basename(file))
  dataIndex <- dataIndex + 1
}

dat <- join_all(dataList,by=c("Country"),type="full")
write.csv(dat,"../pov2.csv",na="",row.names=FALSE)

diRamp <- function(colorText1,colorText2=NA,colorText3=NA){
  colorRef <- list("red"="#BA0C2F")
  colorRef <- c(colorRef,"white"="#FFFFFF")
  colorRef <- c(colorRef,"black"="#000000")
  colorRef <- c(colorRef,"orange"="#EA7600")
  colorRef <- c(colorRef,"purple"="#93328E")
  colorRef <- c(colorRef,"blue"="#1B365D")
  colorRef <- c(colorRef,"lightblue"="#0095CB")
  colorRef <- c(colorRef,"yellow"="#B7BF10")
  colorRef <- c(colorRef,"darkred"=rgb(96, 6, 24,1,maxColorValue=255))
  colorRef <- c(colorRef,"pink"=rgb(251, 197, 208,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue4"=rgb(27, 54, 93,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue3"=rgb(73, 94, 125,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue2"=rgb(118, 134, 158,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue1"=rgb(164, 175, 190,1,maxColorValue=255))
  colorRef <- c(colorRef,"blue0"=rgb(209, 215, 223,1,maxColorValue=255))
  
  if(!is.na(colorText2)){
    if(!is.na(colorText3)){
      color1 <- colorRef[[colorText1]]
      if(is.null(color1)){color1 <- colorText1}
      color2 <- colorRef[[colorText2]]
      if(is.null(color2)){color2 <- colorText2}
      color3 <- colorRef[[colorText3]]
      if(is.null(color3)){color3 <- colorText3}
      colorRamp(c(color1,color2,color3), interpolate="linear")
    }else{
      color1 <- colorRef[[colorText1]]
      if(is.null(color1)){color1 <- colorText1}
      color2 <- colorRef[[colorText2]]
      if(is.null(color2)){color2 <- colorText2}
      colorRamp(c(color1,color2), interpolate="linear")
    }
  }else{
    color1 <- colorRef[["white"]]
    color2 <- colorRef[[colorText1]]
    if(is.null(color2)){color2 <- colorText2}
    colorRamp(c(color1,color2), interpolate="linear")
  }
}

treeMapRamp <- function(vector){
  vectorMax = max(vector)
  message(vectorMax)
  colors <- c()
  for(i in 1:length(vector)){
    newVal <- vector[i]/vectorMax
    if(vector[i]>0){
      rgb <- diRamp("red")(newVal)
      color <- substr(rgb(rgb[1,1],rgb[1,2],rgb[1,3],1,maxColorValue=255),1,7)
      colors <- c(colors,color)
    }else{
      color <- "#cccccc"
      colors <- c(colors,color)
    }
  }
  return(colors)
}

data <- read.csv("national_pov.csv",na.strings="",as.is=TRUE)

depth <- read.csv("depth.csv",na.strings="",as.is=TRUE)
names(depth) = c("id","year","depth")
depth <- subset(depth,year==2013)
depth$year <- NULL
data <- join(data,depth,by="id")

entity <- read.csv("D:/git/digital-platform/reference/entity.csv",na.strings="")
entity <- entity[c("id","region")]
data <- join(data,entity,by="id")

data <- data[complete.cases(data[c("name","UK.Bilateral.ODA","P20.percent","Extreme.poverty.percent","P20.population","Extreme.poverty.population","depth","region")]),]

data <- transform(data,p20.pop.color=treeMapRamp(P20.population))
data <- transform(data,sqrt.p20.pop.color=treeMapRamp(sqrt(P20.population)))
data <- transform(data,ext.pop.color=treeMapRamp(Extreme.poverty.population))
data <- transform(data,sqrt.ext.pop.color=treeMapRamp(sqrt(Extreme.poverty.population)))
data <- transform(data,p20.color=treeMapRamp(P20.percent))
data <- transform(data,ext.color=treeMapRamp(Extreme.poverty.percent))
data <- transform(data,depth.color=treeMapRamp(depth))
data <- transform(data,UK.Bilateral.ODA=as.numeric(UK.Bilateral.ODA))


treemap(data
        ,index="name"
        ,vSize="UK.Bilateral.ODA"
        ,vColor="p20.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)
treemap(data
        ,index="name"
        ,vSize="UK.Bilateral.ODA"
        ,vColor="sqrt.p20.pop.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)
treemap(data
        ,index="name"
        ,vSize="UK.Bilateral.ODA"
        ,vColor="ext.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)
treemap(data
        ,index="name"
        ,vSize="UK.Bilateral.ODA"
        ,vColor="sqrt.ext.pop.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)
treemap(data
        ,index="name"
        ,vSize="UK.Bilateral.ODA"
        ,vColor="depth.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)