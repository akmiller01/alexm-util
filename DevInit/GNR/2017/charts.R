####Setup#####
library(ggplot2)
library(reshape2)
library(data.table)
library(scales)
library(varhandle)
library(Cairo)

#Needed for UTF-8
# Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

wd <- "C:/git/alexm-util/DevInit/GNR/2017"
setwd(wd)

dat <- read.csv("data.csv",na.strings=c("","."),as.is=TRUE)
dist_data <- read.csv("dist_data.csv",na.strings=c("","."),as.is=TRUE)
setnames(dist_data,"quin1","Poorest   ")
setnames(dist_data,"quin2","\nSecond   \npoorest")
setnames(dist_data,"quin3","Middle   ")
setnames(dist_data,"quin4","\nSecond   \nwealthiest")
setnames(dist_data,"quin5","Wealthiest   ")
dist_data <- melt(dist_data,measure.vars=c("Poorest   ","\nSecond   \npoorest","Middle   ","\nSecond   \nwealthiest","Wealthiest   "))
dist_data$value <- dist_data$value*100

countries <- unique(dat$country)

wd <- "C:/Users/Alex/Documents/Data/GNR/Country profiles"
setwd(wd)

unlink(
  dir(wd, full.names = TRUE)
  , recursive = TRUE
)
blank <- data.frame(x=0,y=0,text="No data")
no.data <- ggplot(blank,aes(x,y,label=text)) +
  geom_text(size=20,color="grey") +
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
quintileColor <-  scale_color_manual(values=c(orange,blue,purple,yellow,grey))
purpleOrangeBlueColor <-  scale_color_manual(values=c(purple,orange,blue))

c1values <- list(
  "US$1.90/day (%)"=list(
  "poverty_190_1"="yearpov190_1"
  ,"poverty190_2"="yearpov190_2"
  ),
  "US$3.10/day (%)"=list("poverty310_1"="yearpov310_1"
  ,"poverty310_2"="yearpov310_2"
  ),
  "PPP($) GDP per capita"=list("gdp_1990"="gdp_1990_year"
  ,"gdp_2000"="gdp_2000_year"
  ,"gdp_2010"="gdp_2010_year"
  ,"gdp_2016"="gdp_2016_year"
  )
)
dat$gdp_1990_year <- 1990
dat$gdp_2000_year <- 2000
dat$gdp_2010_year <- 2010
dat$gdp_2016_year <- 2016

c2values <- c(
  "u5mr2011"
  ,"u5mr2012"
  ,"u5mr2013"
  ,"u5mr2014"
  ,"u5mr2015"
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
    "BPmale" = "Male"
    ,"BPfemale" = "Female"
    # ,"BPboth" = "Both sexes"
  ),
  "Raised blood glucose"=list(
    "BGmale" = "Male"
    ,"BGfemale" = "Female"
    # ,"BGboth" = "Both sexes"
  ),
  "Raised blood cholesterol"=list(
    "cholesterol_BOTH" = "Both sexes"
    ,"cholesterol_MALE" = "Male"
    ,"cholesterol_FEMALE" = "Female"
  )
)

c6values <- list(
  "Overweight"=list(
    "ow_male" = "Male"
    ,"ow_female" = "Female"
    # ,"ow_bothsexes" = "Both sexes"
  ),
  "Obesity"=list(
    "ob_male" = "Male"
    ,"ob_female" = "Female"
    # ,"ob_bothsexes" = "Both sexes"
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
    c1a.melt <- subset(c1.melt,variable %in% c("US$1.90/day (%)","US$3.10/day (%)"))
    c1a.max <- max(c1a.melt$value,na.rm=TRUE)
    c1b.melt <- subset(c1.melt,variable == "PPP($) GDP per capita")
    c1b.max <- max(c1b.melt$value,na.rm=TRUE)
    c1a <- ggplot(c1a.melt,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color="transparent") +
      yellowOrangeFill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="transparent", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=35,color="transparent",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(size=9,aes(label=sprintf("%0.1f", round(value, digits = 1))),position=position_dodge(1),vjust=-0.3)
    c1b <- ggplot(c1b.melt,aes(year,value)) +
      geom_line(data=c1b.melt[which(!is.na(value)),],aes(group=variable,colour=variable),size=2.4,lineend="round") +
      purpleColor +
      expand_limits(y=0) +
      expand_limits(y=c1b.max*1.5) +
      guides(colour=guide_legend(title=element_blank())) +
      simple_style +
      scale_y_continuous(expand = c(0,0)) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.justification=c(1,1)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) + geom_text(size=12,aes(label=format(round(value, digits = 0),big.mark=",")),position=position_dodge(1),vjust=-1,color="#443e42")
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
    geom_bar(stat="identity",width=0.6,color="transparent") +
    orangeFill +
    guides(fill=guide_legend(title=element_blank())) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(2010.7,2015.3)) +
    expand_limits(y=c2.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(1),vjust=-0.3,color="#443e42")
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
    geom_bar(stat="identity",width=0.6,color="transparent") +
    blueFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c3.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(1),vjust=-0.3,color="#443e42")
  #Chart 4
  country_dist <- subset(dist_data,country==this.country)
  if(nrow(country_dist)==0){
    c4 <- no.data
  }else{
    country_dist$year <- factor(country_dist$year)
    c4 <- ggplot(country_dist,aes(x=value,y=year)) +
      geom_line(aes(group=year),size=1,color="#adb6c0") +
      geom_point(size=7,aes(group=variable,colour=variable)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      quintileColor +
      guides(colour=guide_legend(title=element_blank())) +
      simple_style  +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color="#443e42")
        ,legend.justification=c(0,0)
        ,legend.direction="horizontal"
        ,axis.title.y=element_blank()
        ,axis.title.x=element_text(color="#443e42",size=20)
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1)
        ,axis.text.y = element_text(size=21,color="#443e42")
        ,axis.text.x = element_text(size=25,color="#443e42")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) +
      xlab("Mean prevalence of stunting (%)")
  }
  
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
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.7,color="transparent") +
    purpleOrangeBlueFill +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c5.max*1.2) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=10,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(0.8),vjust=-0.3,color="#443e42")
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
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.8,color="transparent") +
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
      ,legend.text = element_text(size=35,color="#443e42")
      ,legend.text.align=0
      ,legend.justification=c(1,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.x = element_blank()
      ,axis.line.y = element_line(color="#443e42", size = 1.1)
      ,axis.text.x = element_blank()
      ,axis.text.y = element_text(size=35,color="#443e42",margin=margin(t=0,r=20,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=10,aes(label=sprintf("%0.0f", round(value, digits = 0))),position=position_dodge(0.8),hjust=-0.2,color="#443e42") + coord_flip()
  #Chart 7
  c7datalist <- list()
  c7index <- 1
  yr_anc <- countrydat$yr_anc[1]
  yr_sab <- countrydat$year_sab[1]
  yr_earlybf <- countrydat$yr_earlybf[1]
  yr_contbf <- countrydat$yr_contbf[1]
  yr_unmet_need <- countrydat$yr_unmet_need[1]
  if(!is.null(yr_anc) & !is.na(yr_anc)){
    anctext <- paste0("Antenatal care (4+ visits), ",yr_anc)
    ancdat <- data.frame(ypos=c7index,value=countrydat$anc4[1],label=anctext,color=purple)
    c7datalist[[c7index]] <- ancdat
    c7index <- c7index + 1
  }
  if(!is.null(yr_sab) & !is.na(yr_sab)){
    sabtext <- paste0("Skilled attendant at birth, ",yr_sab)
    sabdat <- data.frame(ypos=c7index,value=countrydat$sab[1],label=sabtext,color=purple)
    c7datalist[[c7index]] <- sabdat
    c7index <- c7index + 1
  }
  if(!is.null(yr_earlybf) & !is.na(yr_earlybf)){
    earlybftext <- paste0("Initiation of breastfeeding within 1 hour after birth, ",yr_earlybf)
    earlybfdat <- data.frame(ypos=c7index,value=countrydat$earlybf[1],label=earlybftext,color=purple)
    c7datalist[[c7index]] <- earlybfdat
    c7index <- c7index + 1
  }
  if(!is.null(yr_contbf) & !is.na(yr_contbf)){
    contbftext <- paste0("Continued breastfeeding at 1 year, ",yr_contbf)
    contbfdat <- data.frame(ypos=c7index,value=countrydat$contbf[1],label=contbftext,color=purple)
    c7datalist[[c7index]] <- contbfdat
    c7index <- c7index + 1
  }
  if(!is.null(yr_unmet_need) & !is.na(yr_unmet_need)){
    unmet_needtext <- paste0("Unmet need for family planning, ",yr_unmet_need)
    unmet_needdat <- data.frame(ypos=c7index,value=countrydat$unmetneed[1],label=unmet_needtext,color=orange)
    c7datalist[[c7index]] <- unmet_needdat
    c7index <- c7index + 1
  }
  c7data <- rbindlist(c7datalist)
  c7data$ypos <- (max(c7data$ypos)+1)-c7data$ypos
  # Cairo(file="c1a.png",width=800,height=600,units="px",bg="transparent")
  # tryCatch({print(c1a)},error=function(e){message(e);print(cblank)})
  # dev.off()
  # Cairo(file="c1b.png",width=800,height=600,units="px",bg="transparent")
  # print(c1b)
  # dev.off()
  Cairo(file="c1.png",width=800,height=600,units="px",bg="transparent")
  tryCatch({print(c1a)},error=function(e){message(e);print(cblank)})
  print(c1b)
  dev.off()
  Cairo(file="c2.png",width=800,height=700,units="px",bg="transparent")
  tryCatch({print(c2)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c3.png",width=800,height=700,units="px",bg="transparent")
  tryCatch({print(c3)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c4.png",width=800,height=400,units="px",bg="transparent")
  tryCatch({print(c4)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c5.png",width=1000,height=400,units="px",bg="transparent")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c6.png",width=1200,height=280,units="px",bg="transparent")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
}
####End loop####
