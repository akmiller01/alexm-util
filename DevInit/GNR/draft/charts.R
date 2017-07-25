####Setup#####
library(ggplot2)
library(reshape2)
library(data.table)
library(scales)
library(varhandle)

#Needed for UTF-8
# Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

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
blank <- data.frame(x=0,y=0,text="No data")
no.data <- ggplot(blank,aes(x,y,label=text)) +
  geom_text(size=20) +
  theme(
    axis.line = element_blank()
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )
blank2 <- data.frame(x=0,y=0,text=" ")
cblank <- ggplot(blank2,aes(x,y,label=text)) +
  geom_text(size=20) +
  theme(
    axis.line = element_blank()
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
  )

simple_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line = element_line(colour = "black"))

yellow <- "#cdd500"
orange <- "#de5d09"
purple <- "#7b1059"
blue <- "#9ed5d7"
grey <- "#879099"

yellowOrangeFill <- scale_fill_manual(values=c(yellow,orange))
purpleFill <- scale_fill_manual(values=c(purple))
orangeFill <- scale_fill_manual(values=c(orange))
blueFill <- scale_fill_manual(values=c(blue))
quintileFill <-  scale_fill_manual(values=c(grey,yellow,purple,blue,orange))
purpleOrangeBlueFill <-  scale_fill_manual(values=c(purple,orange,blue))

yellowOrangeColor <- scale_color_manual(values=c(yellow,orange))
purpleColor <- scale_color_manual(values=c(purple))
orangeColor <- scale_color_manual(values=c(orange))
blueColor <- scale_color_manual(values=c(blue))
quintileColor <-  scale_color_manual(values=c(grey,yellow,purple,blue,orange))
purpleOrangeBlueColor <-  scale_color_manual(values=c(purple,orange,blue))

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
  "PPP($) GDP per capita"=list("GDP_1"="yearGDP_1"
  ,"GDP_2"="yearGDP_2"
  ,"GDP_3"="yearGDP_3"
  ,"GDP_4"="yearGDP_4"
  ,"GDP_5"="yearGDP_5"
  )
)

c2values <- c(
  "u5mr2009"
  ,"u5mr2010"
  ,"u5mr2011"
  ,"u5mr2012"
  ,"u5mr2013"
)

c3values = list("rate_stuntingtrend1"="year_stuntingtrend1"
    ,"rate_stuntingtrend2"="year_stuntingtrend2"
    ,"rate_stuntingtrend3"="year_stuntingtrend3"
    ,"rate_stuntingtrend4"="year_stuntingtrend4"
    ,"rate_stuntingtrend5"="year_stuntingtrend5"
)

c4values <- NA

c5values <- list(
  "Raised blood pressure"=list(
    "BPboth" = "Both sexes"
    ,"BPmale" = "Male"
    ,"BPfemale" = "Female"
  ),
  "Raised blood glucose"=list(
    "BGboth" = "Both sexes"
    ,"BGmale" = "Male"
    ,"BGfemale" = "Female"
  ),
  "Raised blood cholesterol"=list(
    "cholesterol_BOTH" = "Both sexes"
    ,"cholesterol_MALE" = "Male"
    ,"cholesterol_FEMALE" = "Female"
  )
)

c6values <- list(
  "Overweight"=list(
    "ow_bothsexes" = "Both sexes"
    ,"ow_male" = "Male"
    ,"ow_female" = "Female"
  ),
  "Obesity"=list(
    "ob_bothsexes" = "Both sexes"
    ,"ob_male" = "Male"
    ,"ob_female" = "Female"
  )
)

####End setup####
####Loop####
for(this.country in countries){
  message(this.country)
  dir.create(paste(wd,this.country,sep="/"))
  setwd(paste(wd,this.country,sep="/"))
  countrydat <- subset(dat,country==this.country)
  #Chart 1 part a and b
  c1list <- list()
  c1index <- 1
  for(indicator in names(c1values)){
    # message(indicator)
    value.names <- names(c1values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(countrydat[1,sapply(c1values[[indicator]],'[[',index=1)],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c1list[[c1index]] <- indicator.df
    c1index=c1index+1
  }
  c1data <- rbindlist(c1list)
  c1data <- subset(c1data,!is.na(year))
  if(nrow(c1data)!=0){
    c1data <- c1data[order(c1data$year),]
    c1wide <- reshape(c1data,v.names="value",timevar="indicator",idvar="year",direction="wide")
    names(c1wide)[2:length(c1wide)] <- substr(names(c1wide)[2:length(c1wide)],7,nchar(names(c1wide)[2:length(c1wide)]))
    c1.melt <- melt(c1wide,id.vars="year")
    c1.melt$year <- factor(c1.melt$year)
    c1a.melt <- subset(c1.melt,variable %in% c("US$1.25/day (%)","US$2/day (%)"))
    c1a.max <- max(c1a.melt$value,na.rm=TRUE)
    c1b.melt <- subset(c1.melt,variable == "PPP($) GDP per capita")
    c1b.max <- max(c1b.melt$value,na.rm=TRUE)
    c1a <- ggplot(c1a.melt,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity") +
      yellowOrangeFill +
      guides(fill=guide_legend(title=element_blank())) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=15)
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=15,color="black")
      ) + geom_text(aes(label=sprintf("%0.1f", round(value, digits = 1))),position=position_dodge(1),vjust=-0.2)
    c1b <- ggplot(c1b.melt,aes(year,value)) +
      geom_line(aes(group=variable,colour=variable),size=1.2) +
      purpleColor +
      expand_limits(y=0) +
      expand_limits(y=c1b.max*1.5) +
      guides(colour=guide_legend(title=element_blank())) +
      simple_style +
      scale_y_continuous(expand = c(0,0)) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=15)
        ,legend.justification=c(1,1)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=15,color="black")
      ) + geom_text(size=6,aes(label=format(round(value, digits = 0),big.mark=",")),position=position_dodge(1),vjust=-1)
  }else{
    c1a <- cblank
    c1b <- no.data
  }
  
  #Chart 2
  c2data <- countrydat[c2values]
  c2.melt <- data.frame(melt(c2data))
  if(is.factor(c2.melt$variable)){
    c2.melt$variable <- unfactor(c2.melt$variable) 
  }
  c2.melt$year <- as.numeric(substr(c2.melt$variable,nchar(c2.melt$variable)-3,nchar(c2.melt$variable)))
  c2.max <- max(c2.melt$value,na.rm=TRUE)
  c2 <- ggplot(c2.melt,aes(year,value,fill="Deaths per 1,000 live births")) +
    geom_bar(stat="identity",width=0.6) +
    orangeFill +
    guides(fill=guide_legend(title=element_blank())) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(2008.7,2013.3)) +
    expand_limits(y=c2.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=15)
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=15,color="black")
    ) + geom_text(size=6,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(1),vjust=-0.2)
  #Chart 3
  indicator <- "Under-5 stunting"
  value.names <- names(c3values)
  value <- sapply(countrydat[value.names],'[[',index=1)
  year <- sapply(countrydat[1,sapply(c3values,'[[',index=1)],'[[',index=1)
  c3data <- data.frame(indicator,year,value)
  c3data <- subset(c3data,!is.na(year))
  c3data <- c3data[order(c3data$year),]
  c3data$year <- factor(c3data$year)
  c3.max <- max(c3data$value,na.rm=TRUE)
  c3 <- ggplot(c3data,aes(year,value,fill="Blue")) +
    geom_bar(stat="identity",width=0.6) +
    blueFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c3.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=15)
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=15,color="black")
    ) + geom_text(size=6,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(1),vjust=-0.2)
  #Chart 4
  #Data missing?
  c4 <- no.data
  #Chart 5
  c5list <- list()
  c5index <- 1
  for(indicator in names(c5values)){
    # message(indicator)
    value.names <- names(c5values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    sex <- sapply(c5values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,sex,value)
    c5list[[c5index]] <- indicator.df
    c5index=c5index+1
  }
  c5data <- rbindlist(c5list)
  c5.max <- max(c5data$value,na.rm=TRUE)
  c5data$sex <- factor(c5data$sex,levels=c("Both sexes","Male","Female"))
  c5 <- ggplot(c5data,aes(sex,value,fill=indicator)) +
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.7) +
    purpleOrangeBlueFill +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c5.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=15)
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=15,color="black")
    ) + geom_text(aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(0.8),vjust=-0.2)
  #Chart 6
  c6list <- list()
  c6index <- 1
  for(indicator in names(c6values)){
    # message(indicator)
    value.names <- names(c6values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    sex <- sapply(c6values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,sex,value)
    c6list[[c6index]] <- indicator.df
    c6index=c6index+1
  }
  c6data <- rbindlist(c6list)
  c6.max <- max(c6data$value,na.rm=TRUE)
  c6data$sex <- factor(c6data$sex,levels=c("Both sexes","Male","Female"))
  c6 <- ggplot(c6data,aes(sex,value,fill=indicator)) +
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.8) +
    scale_fill_manual(
      labels=c(expression("Obesity (BMI ">=" 30)"),expression("Overweight (BMI ">=" 25)"))
      ,breaks=c("Obesity","Overweight")
      ,values=c(yellow,orange)
      ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c6.max*1.1) +
    theme(
      legend.position="right"
      ,legend.text = element_text(size=15)
      ,legend.text.align=0
      ,legend.justification=c(1,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.x = element_blank()
      ,axis.text.x = element_blank()
      ,axis.text.y = element_text(size=15,color="black")
    ) + geom_text(aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(0.8),hjust=-0.2) + coord_flip()
  png("c1a.png",width=400,height=300,units="px",bg="transparent")
  tryCatch({print(c1a)},error=function(e){message(e);print(cblank)})
  dev.off()
  png("c1b.png",width=400,height=300,units="px",bg="transparent")
  print(c1b)
  dev.off()
  png("c2.png",width=400,height=350,units="px",bg="transparent")
  tryCatch({print(c2)},error=function(e){message(e);print(no.data)})
  dev.off()
  png("c3.png",width=300,height=250,units="px",bg="transparent")
  tryCatch({print(c3)},error=function(e){message(e);print(no.data)})
  dev.off()
  png("c4.png",width=400,height=200,units="px",bg="transparent")
  tryCatch({print(c4)},error=function(e){message(e);print(no.data)})
  dev.off()
  png("c5.png",width=500,height=200,units="px",bg="transparent")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  png("c6.png",width=600,height=140,units="px",bg="transparent")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
}
####End loop####
