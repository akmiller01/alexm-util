library(rCharts)
library(eulerr)
library(ggplot2)
library(scales)
library(Cairo)
library(extrafont)
font_import()
loadfonts(device="win") 
windowsFonts(Arial = windowsFont("arial"))

simple_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line.x = element_line(color="#443e42", size = 0.5)
    ,axis.line.y = element_line(color="#443e42", size = 0.5)
    ,legend.position="top"
    ,legend.text = element_text(size=12,color="#443e42",family="Arial")
    ,legend.title = element_blank()
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_text(size=12,color="#443e42",family="Arial")
    ,axis.title.y=element_text(size=12,color="#443e42",family="Arial")
    ,axis.ticks=element_blank()
    ,axis.text.y = element_text(size=12,color="#443e42",family="Arial")
    ,axis.text.x = element_text(size=12,color="#443e42",family="Arial")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_rect(fill = "transparent", colour = "transparent")
    )

reds <- c(
  "#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#6b120a", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25"
)
reds_and_greys <- c(
  "#d9d4da", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#302b2e", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25"
)

redColour <- scale_colour_manual(values=reds)
redgreyColour <- scale_colour_manual(values=reds_and_greys)
redGrad <- scale_fill_gradientn(colours=c(reds[7],reds[1],reds[6]),name="Frequency",na.value=NA)

setwd("C:/Users/Alex/Documents/Data/coding_p20")

fig1.dat <- read.csv("cwi_vs_gni.csv",na.strings="")
fig1.dat$region <- gsub(" (all income levels)","",fig1.dat$region,fixed=TRUE)

diColors <- c("#ba0c2f" #Red
              ,"#1b365d" #blue
              ,"#ea7600" #Orange
              ,"#93328e" #purple
              ,"#0095c8" #lightblue
              ,"#b7bf10" #Yellow
)

d <- dPlot(
  y = "NY.GNP.PCAP.PP.KD",
  x = "mean.cwi",
  groups = c("country","iso2c","year","region"),
  data = fig1.dat,
  type = "bubble",
  bounds = list(x = 50, y = 50, height = 600, width = 700),
  height = 750,
  width = 800
)
d$defaultColors(diColors)
d$xAxis( type = "addMeasureAxis")
d$yAxis( type = "addMeasureAxis")
d$set(legend = list(x=510,y=40,height=100,width=100))
d$setTemplate(afterScript = "
               <script>
               myChart.draw()
               myChart.axes[0].titleShape.text('Mean CWI')
               myChart.axes[1].titleShape.text('GNI per capita (Constant 2011 PPP)')
               myChart.svg.append('text')
               .attr('x', 250)
               .attr('y', 20)
               .text('CWI vs. GNI per capita')
               .style('text-anchor','beginning')
               .style('font-size', '100%')
               .style('font-family','sans-serif')
               </script>               
               ")
# d$save("fig1_interactive.html", cdn = TRUE)

fig1 <- ggplot(fig1.dat,aes(x=mean.cwi,y=NY.GNP.PCAP.PP.KD,colour=region)) +
  geom_point(size=2) +
  redgreyColour +
  simple_style +
  theme(legend.text = element_text(size=12,color="#443e42",family="Arial")
        ,legend.key.size = unit(0.8,"lines")
        ,axis.line.x = element_line(color="#443e42", size = 0.5)
        ,axis.line.y = element_line(color="#443e42", size = 0.5)
        ) +
  labs(y="GNI per capita (PPP)",x="Mean Comparable Wealth Index") +
  scale_y_continuous(label=comma)
ggsave("figure1.png",fig1,family="Arial",height=5.31,width=8.36,units="in")

fig2.dat <- unlist(c(read.csv("russia_venn.csv")[1,]))
names(fig2.dat) <- c(
  "Expenditure"
  ,"Income"
  ,"Wealth"
  ,"Expenditure&Income"
  ,"Income&Wealth"
  ,"Wealth&Expenditure"
  ,"Expenditure&Income&Wealth"
)
fit2 <- euler(fig2.dat)
png("figure2.png",bg="transparent",family="Arial",height=5.31,width=8.36,units="in",res=300,type="cairo")
plot(fit2
     ,fill = reds
     ,edges = "transparent"
     ,quantities = TRUE)
dev.off()

fig3.dat <- read.csv("eth_wealth.csv",na.strings="")
p <- ggplot(fig3.dat,aes(x=wealth,y=pcexpppp)) + stat_binhex() +
  redGrad +
  labs(
    x="Wealth index"
    ,y="Expenditure per capita (Constant 2011 $ PPP)"
  ) + theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,axis.line.x = element_line(color="#443e42", size = 0.5)
    ,axis.line.y = element_line(color="#443e42", size = 0.5)
    ,legend.position="top"
    ,legend.text = element_text(size=12,color="#443e42",family="Arial")
    ,legend.title = element_text(size=12,color="#443e42",family="Arial")
    ,legend.justification=c(0,0)
    ,legend.direction="horizontal"
    ,axis.title.x=element_text(size=12,color="#443e42",family="Arial")
    ,axis.title.y=element_text(size=12,color="#443e42",family="Arial")
    ,axis.ticks=element_blank()
    ,axis.text.y = element_text(size=12,color="#443e42",family="Arial")
    ,axis.text.x = element_text(size=12,color="#443e42",family="Arial")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) + scale_y_log10(label=comma)
ggsave("figure3.png",p,family="Arial",height=5.31,width=8.36,units="in")
