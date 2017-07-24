library(ggplot2)
library(reshape2)
library(data.table)
library(scales)

wd <- "C:/git/alexm-util/DevInit/GNR/draft"
setwd(wd)

dat <- read.csv("2015data.csv",na.strings=c("","."),as.is=TRUE)

countries <- unique(dat$country)

wd <- "C:/Users/Alex/Documents/Data/GNR/Country profiles"
setwd(wd)

unlink(
  dir(wd, full.names = TRUE)
  , recursive = TRUE
)

simple_style = theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

c1values <- list(
  "US$1.25/day (%)"=list(
  "poverty125_1"="yearpov125_1"
  ,"poverty125_2"="yearpov125_2"
  ,"poverty125_3"="yearpov125_3"
  ,"poverty125_4"="yearpov125_4"
  ),
  "US$2/day (%)"=list("poverty2_1"="yearpov2_1"
  ,"poverty2_2"="yearpov2_2"
  ,"poverty2_3"="yearpov2_3"
  ,"poverty2_4"="yearpov2_4"
  ),
  "GDP per capita PPP ($)"=list("GDP_1"="yearGDP_1"
  ,"GDP_2"="yearGDP_2"
  ,"GDP_3"="yearGDP_3"
  ,"GDP_4"="yearGDP_4"
  ,"GDP_5"="yearGDP_5"
  )
)


for(this.country in countries){
  message(this.country)
  dir.create(this.country)
  setwd(paste(wd,this.country,sep="/"))
  countrydat <- subset(dat,country==this.country)
  c1list <- list()
  c1index <- 1
  for(indicator in names(c1values)){
    message(indicator)
    value.names <- names(c1values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(countrydat[1,sapply(c1values[[indicator]],'[[',index=1)],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c1list[[c1index]] <- indicator.df
    c1index=c1index+1
  }
  c1data <- rbindlist(c1list)
  c1data <- subset(c1data,!is.na(year))
  c1data <- c1data[order(c1data$year),]
  c1wide <- reshape(c1data,v.names="value",timevar="indicator",idvar="year",direction="wide")
  names(c1wide)[2:length(c1wide)] <- substr(names(c1wide)[2:length(c1wide)],7,nchar(names(c1wide)[2:length(c1wide)]))
  c1.melt <- melt(c1wide,id.vars="year")
  c1.melt$year <- factor(c1.melt$year)
  c1a.melt <- subset(c1.melt,variable %in% c("US$1.25/day (%)","US$2/day (%)"))
  c1a.max <- max(c1a.melt$value,na.rm=TRUE)
  c1b.melt <- subset(c1.melt,variable == "GDP per capita PPP ($)")
  c1b.max <- max(c1b.melt$value,na.rm=TRUE)
  ggplot(c1a.melt,aes(year,value,fill=variable)) +
    geom_bar(position="dodge",stat="identity") +
    guides(fill=guide_legend(title="")) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c1a.max*1.1) +
    theme(
      legend.position="top"
      ,legend.justification=c(0,1)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.text.y = element_blank()
    ) + geom_text(aes(label=value),position=position_dodge(1),vjust=-0.2)
  ggplot(c1b.melt,aes(year,value)) +
    geom_line(aes(group=variable,colour=variable),size=1) +
    expand_limits(y=0) +
    expand_limits(y=c1b.max*1.1) +
    guides(colour=guide_legend(title="")) +
    simple_style +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.position="top"
      ,legend.justification=c(1,1)
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.text.y = element_blank()
    ) + geom_text(aes(label=value),position=position_dodge(1),vjust=-1)
}
