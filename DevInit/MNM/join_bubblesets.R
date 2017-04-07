library(data.table)
library(Hmisc)
library(plyr)

wd <- "D:/Documents/Data/MNM/formatted"
setwd(wd)

files <- list.files(wd,pattern="*.csv",full.names=TRUE)

dataList <- list()
dataIndex <- 1

for(file in files){
  dataList[[dataIndex]] <- read.csv(file,na.strings="",as.is=TRUE)
  message(basename(file))
  dataIndex <- dataIndex + 1
}

dat <- join_all(dataList,by=c("id","year"))
dat$mc.oda.pc <- dat$mc.oda/(dat$pop/100000)
# write.csv(dat,"../bubble_data.csv",row.names=FALSE,na="")
dat <- read.csv("/Users/Alex/Downloads/data.csv",na.strings=FALSE,as.is=TRUE)
library(ggplot2)

# ggplot(dat,aes(x=mc.oda.pc,y=deaths.per.100k,size=log(health.exp),colour=region)) + geom_point() +
#   xlim(-0.1,.75) +
#   labs(x="Malaria control ODA per 100k population"
#       ,y="Deaths due to malaria per 100k population"
#       ,title="Malaria ODA spending vs. Health outcomes"
#       ,size="Log of health spending per capita"
#       ,colour="Region"
#       )

# ggplot(dat,aes(x=health.exp,y=deaths.per.100k,size=mc.oda.pc,colour=region)) + geom_point() +
#   xlim(0,1000) +
#   labs(x="Health spending per capitaHealth spending per capita"
#        ,y="Deaths due to malaria per 100k population"
#        ,title="Malaria ODA spending vs. Health outcomes"
#        ,size="Log Malaria control ODA per 100k population"
#        ,colour="Region"
#   )
# dat$health.exp[which(dat$health.exp>=400)] <- 400
ggplot(dat,aes(x=log(mc.oda),y=log(cases),size=health.exp,colour=region)) + geom_point() +
  labs(x="Malaria control ODA"
       ,y="Reported malaria cases"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Health expenditure"
       ,colour="Region"
  )

plot.dat <- subset(dat,mc.oda>0 & cases>0 & !is.na(health.exp))
plot.dat$mc.oda <- plot.dat$mc.oda*1000000
keep <- c("id","year","mc.oda","cases","health.exp","region")
plot.dat <- plot.dat[keep]
entities <- read.csv("D:/git/digital-platform/reference/entity.csv",na.strings="")
entities <- entities[c("id","name")]
plot.dat <- join(plot.dat,entities,by="id")
diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
              ,"steelblue"
)
library(rCharts)
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
plot.dat$Region <- sapply(regionDict[plot.dat$region],`[[`,index=1)
library(scales)
ggplot(dat,aes(x=mc.oda,y=cases,size=health.exp,colour=Region)) + geom_point(alpha=0.75) +
  labs(x="Malaria control ODA"
       ,y="Reported malaria cases"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Health expenditure per capita"
       ,colour="Region"
  ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma)
library(scales)
ggplot(subset(dat,year==2014),aes(x=mc.oda,y=health.exp,size=cases,colour=Region)) + geom_point(alpha=0.75) +
  labs(x="Malaria control ODA"
       ,y="Health expenditure per capita"
       ,title="Malaria ODA spending vs. Health outcomes"
       ,size="Reported malaria cases"
       ,colour="Region"
  ) +
  theme_bw() + scale_size(range=c(1,15)) + scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma)
write.csv(plot.dat,"D:/Documents/Data/MNM/bubble2/data.csv",row.names=FALSE,na="")
setnames(
  plot.dat
  ,"name"
  ,"Country"
)
setnames(
  plot.dat
  ,"year"
  ,"Year"
)
setnames(
  plot.dat
  ,"mc.oda"
  ,"Malaria control ODA"
)
setnames(
  plot.dat
  ,"cases"
  ,"Reported malaria cases"
)
setnames(
  plot.dat
  ,"health.exp"
  ,"Health expenditure per capita"
)

d <- dPlot(
  y = "Reported malaria cases",
  x = "Malaria control ODA",
  z = "Health expenditure per capita",
  groups = c("Country","Year","Region"),
  data = plot.dat,
  type = "bubble",
  height = 750,
  width = 800
)
d$defaultColors(diColors)
d$xAxis( type = "addLogAxis")
d$yAxis( type = "addLogAxis")
d$zAxis( type = "addMeasureAxis",overrideMax=2000,overrideMin=-50,outputFormat = ',.1f')
d$set(legend = list(x=600,y=550,height=100,width=100))
d$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('Malaria control ODA')
              myChart.axes[1].titleShape.text('Reported malaria cases')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('Malaria ODA spending vs. Health outcomes')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d$save('D:/Documents/Data/MNM/bubble1.html', cdn = TRUE)

dat$year <- factor(dat$year)
dat$id <- factor(dat$id)
dat$deaths.pc <- dat$deaths/dat$pop
fit <- lm(deaths.pc~mc.oda.pc+pop+health.exp+id+year,data=dat)
summary(fit)

cor(dat[c(3:7,9:13)],use="pairwise.complete.obs")

# complete <- dat[which(complete.cases(dat)),]
# cor(complete[c(names(complete)[3:12])])