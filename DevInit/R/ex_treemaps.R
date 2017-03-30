#install.packages("treemap")
library(treemap)
library(plyr)
library(data.table)
wd <- "/Users/Alex/Downloads/home_work/bubble/join"
setwd(wd)

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

dataList <- list()
dataIndex <- 1

data.files <- list.files(pattern="*.csv")

for(file in data.files){
  temp <- read.csv(file,na.strings="")
  dataList[[dataIndex]] <- temp
  dataIndex <- dataIndex + 1
}

data <- join_all(dataList,by="Country")

# View(data[which(!complete.cases(data)),])
# 
# data <- transform(data,p20.pop.color=treeMapRamp(p20.pop))
# data <- transform(data,sqrt.p20.pop.color=treeMapRamp(sqrt(p20.pop)))
# data <- transform(data,ext.pop.color=treeMapRamp(ext.pop))
# data <- transform(data,sqrt.ext.pop.color=treeMapRamp(sqrt(ext.pop)))
# data <- transform(data,p20.color=treeMapRamp(p20))
# data <- transform(data,ext.color=treeMapRamp(ext))
# data <- transform(data,depth.color=treeMapRamp(ext.depth))
# data <- transform(data,UK.Bilateral.ODA=as.numeric(oda))
# data$name <- data$Country
# 
# treemap(data
#         ,index="name"
#         ,vSize="UK.Bilateral.ODA"
#         ,vColor="p20.color"
#         ,type="color"
#         ,title=""
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# treemap(data
#         ,index="name"
#         ,vSize="UK.Bilateral.ODA"
#         ,vColor="sqrt.p20.pop.color"
#         ,type="color"
#         ,title=""
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# treemap(data
#         ,index="name"
#         ,vSize="UK.Bilateral.ODA"
#         ,vColor="ext.color"
#         ,type="color"
#         ,title=""
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# treemap(data
#         ,index="name"
#         ,vSize="UK.Bilateral.ODA"
#         ,vColor="sqrt.ext.pop.color"
#         ,type="color"
#         ,title=""
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )
# treemap(data
#         ,index="name"
#         ,vSize="UK.Bilateral.ODA"
#         ,vColor="depth.color"
#         ,type="color"
#         ,title=""
#         # ,lowerbound.cex.labels=1
#         ,fontsize.labels=12
#         ,inflate.labels=TRUE
# )

pov.names <- read.csv("/Users/Alex/Downloads/home_work/bubble/pov_names.csv",na.strings="")
data <- join(data,pov.names,by="Country")

regions <- read.csv("~/git/digital-platform/reference/entity.csv",na.strings="",as.is=TRUE)
regions <- regions[c("id","region")]
data <- join(data,regions,by="id")

regionDict <- list(
  "europe"="Europe"               
  ,"middle-east"="Middle East"          
  ,"south-central-asia"="South central Asia"   
  ,"north-central-america"="North Central America"
  ,"south-of-sahara"="Sub-Saharan Africa"      
  ,"south-america"="South America"        
  ,"oceania"="Oceania"              
  ,"far-east-asia"="Far East Asia"        
  ,"north-of-sahara"="North Africa"
)
data$region <- sapply(regionDict[data$region],`[[`,index=1)

library(ggplot2)
ggplot(data,aes(x=ext*100,y=ext.depth,size=oda,colour=region,label=Country)) + geom_point(alpha=0.5) +
  labs(x="Percent of population in extreme poverty"
       ,y="Depth of extreme poverty"
       ,size="UK bilateral ODA (millions)"
       ,colour="Region") +
  theme_bw() + scale_size(range=c(1,10))
# +
  # geom_text(aes(label=ifelse(UK.Bilateral.ODA>334745686,as.character(name),'')),hjust=0,vjust=1)
require(scales)
ggplot(data,aes(y=ext.pop,x=ext.depth,size=oda,colour=region)) + geom_point(alpha=0.5) +
  labs(y="Population in extreme poverty"
       ,x="Depth of extreme poverty"
       ,size="UK bilateral ODA (millions)"
       ,colour="Region"
       ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma)  +
 geom_text(aes(label=ifelse(oda>300,as.character(Country),'')),hjust=0,vjust=1)
