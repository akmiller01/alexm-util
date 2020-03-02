list.of.packages <- c("ggplot2","data.table","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

usd_format = function(x){
  return(paste0("US$",prettyNum(round(x/1000000),big.mark=","),"m"))
}

text_angle = function(x){
  if(x<0.5){
    return(-1*x*360+90) 
  }
  return(-1*x*360-90)
}
text_angle = Vectorize(text_angle)

hjust_angle = function(x){
  if(x<0.5){
    return(1) 
  }
  return(0)
}
hjust_angle = Vectorize(hjust_angle)

dat = data.table(
  donor=c(
    "Turkey**",
    "US",
    "Germany",
    "EU*",
    "UK",
    "UAE",
    "Saudi Arabia",
    "Sweden",
    "Canada",
    "Japan",
    "Norway",
    "France",
    "Netherlands",
    "Denmark",
    "Italy",
    "Switzerland",
    "Belgium",
    "Kuwait",
    "Spain",
    "Australia",
    "Ireland",
    "Finland"
  ),
  value=c(
    8399000000,
    6646000000,
    2962000000,
    2240000000,
    2394000000,
    1968000000,
    1272000000,
    952000000,
    870000000,
    648000000,
    628000000,
    558000000,
    563000000,
    495000000,
    459000000,
    404000000,
    323000000,
    309000000,
    240000000,
    224000000,
    212000000,
    123000000
  )
)

dat$year = 1999
original_dat = copy(dat)
for(i in 2000:2020){
  dat_copy = copy(original_dat)
  dat_copy$year = i
  dat_copy$value = dat_copy$value * runif(nrow(dat_copy))
  dat = rbind(dat,dat_copy)
}

dat = dat[order(dat$year, -dat$value)]

dat[,angle:=c(1:nrow(.SD))/nrow(.SD),by=.(year)]
dat$label = NA
dat$label[which(dat$angle < 0.5)] = paste0("paste(bold('",dat$donor[which(dat$angle < 0.5)],"'),' ','",usd_format(dat$value[which(dat$angle < 0.5)]),"')")
dat$label[which(dat$angle >= 0.5)] = paste0("paste('",usd_format(dat$value[which(dat$angle >= 0.5)]),"',' ',bold('",dat$donor[which(dat$angle >= 0.5)],"'))")
dat[,rank:=c(1:nrow(.SD)),by=.(year)]

line_dat = data.frame(
  ymin=0.3,
  ymax=2,
  angle=dat$angle,
  value=0
)

p = ggplot(dat,aes(x=angle,y=2,color=value,size=value)) +
  geom_hline(yintercept=2,size=0.3) +
  geom_hline(yintercept=0.3,size=0.3) +
  geom_rect(data=line_dat,aes(xmin=angle,xmax=angle,ymin=ymin,ymax=ymax),show.legend=F,size=0.3) +
  geom_text(
    parse=T,
    data=dat,
    aes(
      label=label,
      angle=text_angle(angle),
      hjust=hjust_angle(angle),
      y=1.7
    ),
    show.legend=F,
    color="black",
    size=2.5,
    vjust=0
  ) +
  geom_point(
    show.legend=F,
    aes(
      tooltip=paste0(
        "<p><b>Donor: </b>",donor,"</p>",
        "<p><b>Contributions: </b>",usd_format(value),"</p>",
        "<p><b>Rank: </b>",rank,"</p>"
      ),
      frame=year
    )
  ) +
  ylim(min=0,max=2) +
  xlim(min=0,max=1) +
  scale_size_continuous(range=(c(1,15))) +
  coord_polar(theta="x") +
  theme(
    panel.border = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line = element_blank()
    ,axis.ticks = element_blank()
    ,text = element_blank()
  )

p = ggplotly(p)
