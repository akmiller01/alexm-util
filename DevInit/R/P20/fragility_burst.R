list.of.packages <- c("data.table","ggplot2","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

dat = WDI("NY.GDP.MKTP.KD",country="all",start=2016,end=2016)
dat = dat[order(dat$NY.GDP.MKTP.KD),]
dat = dat[1:50,]
dat$value = 1
dat$ymax = cumsum(dat$value)
dat$ymin = c(0,head(dat$ymax,n=-1))
y.maximum = max(dat$ymax)
gap = 1.3
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
ggplot(dat,aes(xmax=2,xmin=1,ymin=ymin,ymax=ymax,fill=NY.GDP.MKTP.KD)) +
  geom_rect(color="white") +
  geom_text(data=angle.dat,aes(x=2.1,y=ymax,label=country,hjust=hjusts,vjust=vjusts,angle=angles)) +
  coord_polar(theta="y") +
  xlim(c(-3,4)) +
  ylim(c(0,y.maximum*gap)) +
  theme(
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.line = element_blank(),
    panel.grid=element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  )
