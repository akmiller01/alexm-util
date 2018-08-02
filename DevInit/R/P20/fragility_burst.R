list.of.packages <- c("data.table","ggplot2","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

set.seed(12345)

dat = WDI("NY.GDP.PCAP.KD",country="all",start=2016,end=2016,extra=T)
pc = WDI("NY.GDP.PCAP.CD",country="all",start=2016,end=2016)
dat = merge(dat,pc)
ppp =  WDI("NY.GDP.PCAP.PP.CD",country="all",start=2016,end=2016)
dat = merge(dat,ppp)
dat = subset(dat,complete.cases(dat) & capital!="")
dat = dat[sample(0:nrow(dat),50,replace=T),]
dat = dat[order(-dat$NY.GDP.PCAP.KD),]
dat$value = 1
dat$ymax = cumsum(dat$value)
dat$ymin = c(0,head(dat$ymax,n=-1))
y.maximum = max(dat$ymax)
gap = 1.33
angle.step = 360/(y.maximum*gap)
angle.start = 90
angle.end = (angle.step*nrow(dat)*(gap-1))-(360-angle.start)
angles = seq(angle.start,angle.end,(angle.end-angle.start)/nrow(dat))
angles = angles[2:length(angles)]
country = dat$country
angle.dat = dat
angle.dat$angles = angles
angle.dat$hjusts = 0
angle.dat$vjusts = 0
angle.dat$hjusts[which(angle.dat$angles < -90)] = 1
angle.dat$vjusts[which(angle.dat$angles < -90)] = 1
angle.dat$angles[which(angle.dat$angles < -90)] = angle.dat$angles[which(angle.dat$angles < -90)] + 180
labels = c(
  "Constant 2010 US$",
  "Current US$",
  "Current international $"
  )
positions = c(1.5,0.5,-0.5)
label.dat = data.frame(labels,positions,y.maximum)
ggplot(dat,aes(ymin=ymin,ymax=ymax)) +
  geom_rect(aes(xmax=2,xmin=1,fill=NY.GDP.PCAP.KD),color="white") +
  geom_rect(aes(xmax=1,xmin=0,fill=NY.GDP.PCAP.CD),color="white") +
  geom_rect(aes(xmax=0,xmin=-1,fill=NY.GDP.PCAP.PP.CD),color="white") +
  geom_text(data=angle.dat,aes(x=2.1,y=ymax,label=country,hjust=hjusts,vjust=vjusts,angle=angles)) +
  geom_text(data=label.dat,aes(x=positions,y=0,label=labels,ymin=1,ymax=1),hjust=1) + 
  coord_polar(theta="y") +
  xlim(c(-4,4)) +
  ylim(c(0,y.maximum*gap)) +
  theme(
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.line = element_blank(),
    panel.grid=element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.title=element_blank()
  )
