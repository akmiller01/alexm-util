library(data.table)
library(plyr)

wd <- "~/git/digital-platform/country-year"
setwd(wd)

oda.files <- c(
  "oda.csv"
  ,"oda.2000.csv"
  ,"oda.2001.csv"
  ,"oda.2002.csv"
  ,"oda.2003.csv"
  ,"oda.2004.csv"
  ,"oda.2005.csv"
  ,"oda.2006.csv"
  ,"oda.2007.csv"
  ,"oda.2008.csv"
  ,"oda.2009.csv"
  ,"oda.2010.csv"
  ,"oda.2011.csv"
  ,"oda.2012.csv"
  ,"oda.2013.csv"
  ,"oda.2014.csv"
  ,"oda.2015.csv"
)

odaList <- list()
odaIndex <- 1

for(file in oda.files){
  message(file)
  temp <- read.csv(file,na.strings="")
  odaList[[odaIndex]] <- temp
  odaIndex <- odaIndex + 1
}

oda <- rbindlist(odaList)

health.oda <- subset(oda,sector=="health")
entity <- read.csv("../reference/entity.csv",na.strings="")
entity <- entity[c("id","name")]
names(entity) <- c("id.from","name.from")
health.oda <- join(health.oda,entity,by="id.from")

health.oda.recip.tab <- data.table(health.oda)[,.(health.oda=sum(value,na.rm=TRUE)),by=.(name.from,id.to,year)]

dalys <- read.csv("/Users/Alex/Downloads/home_work/bubble/ihme.csv",na.strings="")
daly.names <- read.csv("/Users/Alex/Downloads/home_work/bubble/daly_names.csv",na.strings="")
dalys <- join(dalys,daly.names,by="location_name")
dalys <- dalys[c("id.to","year","val")]
dalys <- subset(dalys,!is.na(id.to))
names(dalys) <- c("id.to","year","daly.rate")

health.oda.recip.tab <- join(health.oda.recip.tab,dalys,by=c("id.to","year"))
# health.oda.recip.tab <- subset(health.oda.recip.tab,!is.na(daly.rate))
data <- health.oda.recip.tab
data <- join(data,daly.names,by="id.to")
setnames(data,"location_name","name.to")
write.csv(health.oda.recip.tab,"/Users/Alex/Downloads/home_work/bubble/all_health_oda_joined.csv",row.names=FALSE,na="")


write.csv(data,"/Users/Alex/Downloads/home_work/bubble/oda_dalys_joined.csv",na="",row.names=FALSE)

library(treemap)
wd <- "/Users/Alex/Downloads/home_work/bubble/"
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
  vectorMin = min(vector)
  message(vectorMax)
  colors <- c()
  for(i in 1:length(vector)){
    newVal <- (vector[i]-vectorMin)/(vectorMax-vectorMin)
    rgb <- diRamp("red")(newVal)
    color <- substr(rgb(rgb[1,1],rgb[1,2],rgb[1,3],1,maxColorValue=255),1,7)
    colors <- c(colors,color)
  }
  return(colors)
}

uk.data <- subset(data,name.from=="UK" & year==2015)
uk.data <- transform(uk.data,daly.color=treeMapRamp(daly.rate))


treemap(uk.data
        ,index="name.to"
        ,vSize="health.oda"
        ,vColor="daly.color"
        ,type="color"
        ,title=""
        # ,lowerbound.cex.labels=1
        ,fontsize.labels=12
        ,inflate.labels=TRUE
)