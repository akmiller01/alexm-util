library(Hmisc)
library(WDI)
library(data.table)
library(ggplot2)
library(rCharts)

wd <- "D:/Documents/Data/P20 baseline"
setwd(wd)

p20.income <- read.csv("p20.income.csv",na.strings=c("#N/A","#DIV/0!"))
p20.income <- p20.income[complete.cases(p20.income$p20.income),]
p20.pop <- read.csv("p20.pop.csv")
p20.income <- merge(
  p20.income
  ,p20.pop
  ,by="iso3"
  )

p20.income$type <- tolower(p20.income$type)

indicator <- "NY.GNP.PCAP.PP.KD"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 2011, 
           end = 2012,
           extra = TRUE
)
dat <- dat[complete.cases(dat$NY.GNP.PCAP.PP.KD),]
dat <- dat[order(dat$iso3c,dat$year),]
dat <- data.table(dat)
dat$latest <- rev(!duplicated(dat[,list(rev(dat$iso3c))]))
dat <- subset(dat,latest)
dat <- data.frame(dat)

setnames(dat,"iso3c","iso3")
setnames(dat,"NY.GNP.PCAP.PP.KD","gdp.pc")
keep <- c("country","iso3","gdp.pc","region")
dat <- dat[keep]
dat <- dat[complete.cases(dat),]
p20.income <- merge(
  p20.income
  ,dat
  ,by="iso3"
  )

diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)

consumption <- subset(p20.income,type=="c")
income <- subset(p20.income,type=="i")

# p <- ggplot(consumption,aes(x=p20.income,y=gdp.pc,colour=region)) +
#   geom_point(size=3) +
#   scale_colour_manual(values=diColors)
# p
# p2 <- ggplot(income,aes(x=p20.income,y=gdp.pc,colour=region)) +
#   geom_point(size=3) +
#   scale_colour_manual(values=diColors)
# p2

setnames(
  consumption
  ,"gdp.pc"
  ,"GDP per capita (2011 PPP)"
  )
setnames(
  consumption
  ,"p20.income"
  ,"Average consumption of P20 (2011 PPP)"
)
setnames(
  consumption
  ,"country"
  ,"Country"
)
setnames(
  consumption
  ,"region"
  ,"Region"
)
setnames(
  consumption
  ,"p20.pop"
  ,"Population in the P20"
)

d <- dPlot(
  y = "GDP per capita (2011 PPP)",
  x = "Average consumption of P20 (2011 PPP)",
  z = "Population in the P20",
  groups = c("Country","Region"),
  data = consumption,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d$defaultColors(diColors)
d$xAxis( type = "addMeasureAxis")
d$yAxis( type = "addMeasureAxis")
d$zAxis( type = "addMeasureAxis")
d$set(legend = list(x=60,y=40,height=100,width=100))
d$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('Average consumption of P20 (Constant 2011 PPP)')
              myChart.axes[1].titleShape.text('GNI per capita (Constant 2011 PPP)')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('P20 consumption vs. GNI per capita (PovcalNet 2012)')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d

d2 <- dPlot(
  y = "GDP per capita (2011 PPP)",
  x = "Average consumption of P20 (2011 PPP)",
  groups = c("Population in the P20","Country","Region"),
  data = consumption,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d2$defaultColors(diColors)
d2$xAxis( type = "addMeasureAxis")
d2$yAxis( type = "addMeasureAxis")
d2$set(legend = list(x=60,y=40,height=100,width=100))
d2$setTemplate(afterScript = "
              <script>
              myChart.draw()
              myChart.axes[0].titleShape.text('Average consumption of P20 (Constant 2011 PPP)')
              myChart.axes[1].titleShape.text('GNI per capita (Constant 2011 PPP)')
              myChart.svg.append('text')
              .attr('x', 250)
              .attr('y', 20)
              .text('P20 consumption vs. GNI per capita (PovcalNet 2012)')
              .style('text-anchor','beginning')
              .style('font-size', '100%')
              .style('font-family','sans-serif')
              </script>               
              ")
d2

d$save('scaled_bubble.html', cdn = TRUE)
d2$save('unscaled_bubble.html', cdn = TRUE)
write.csv(p20.income,"bubble-dat.csv",na="",row.names=FALSE)
