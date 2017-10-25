####Setup#####
library(ggplot2)
library(reshape2)
library(data.table)
library(scales)
library(varhandle)
library(Cairo)
library(plyr)

#Needed for UTF-8
# Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

wd <- "C:/git/alexm-util/DevInit/GNR/2017"
setwd(wd)

dat <- read.csv("agg_long.csv",na.strings=c(" "),as.is=TRUE)

regions <- unique(dat$region)

wd <- "C:/Users/Alex/Documents/Data/GNR/Aggregate profiles"
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

yellow <- "#bfc200"
orange <- "#de5d09"
purple <- "#71105f"
blue <- "#93cac9"
grey <- "#a0adbb"
white <- "#ffffff"
black <- "#443e42"

yellowOrangeFill <- scale_fill_manual(values=c(yellow,orange))
yellowOrangePurpleFill <- scale_fill_manual(values=c(yellow,orange,purple))
purpleFill <- scale_fill_manual(values=c(purple))
orangeFill <- scale_fill_manual(values=c(orange))
blueFill <- scale_fill_manual(values=c(blue))
quintileFill <-  scale_fill_manual(values=c(grey,yellow,purple,blue,orange))
purpleOrangeBlueFill <-  scale_fill_manual(values=c(purple,orange,blue))

quintileFillValues <- c(grey,yellow,purple,blue,orange)

yellowOrangeColor <- scale_color_manual(values=c(yellow,orange))
purpleColor <- scale_color_manual(values=c(purple))
orangeColor <- scale_color_manual(values=c(orange))
blueColor <- scale_color_manual(values=c(blue))
quintileColor <-  scale_color_manual(values=c(orange,blue,purple,yellow,grey))
purpleOrangeBlueColor <-  scale_color_manual(values=c(purple,orange,blue))

textQuintileOffset <- scale_color_manual(values=c(black,black,white,black,black))

safeFormat <- function(vec){
  results <- c()
  for(x in vec){
    #Missing
    if(is.na(x)){
      result <- ""
      #Large Negative
    }else if(x<= -1000){
      result <- format(round(x, digits = 0),format="d",big.mark=",")
      #Middle Negative
    }else if(x< -1){
      result <- round(x,digits=0)
      #Small negative
    }else if(x<0){
      result <- round(x,digits=1)
      #Zero
    }else if(x==0){
      result <- "0"
      #Small positive
    }else if(x<1){
      result <- round(x,digits=1)
      #Middle positive
    }else if(x<1000){
      result <- round(x,digits=0)
      #Large positive
    }else{
      result <- format(round(x, digits = 0),format="d",big.mark=",")
    }
    results <- c(results,result)
  }
  return(results)
}

###Indicator setup####
c5indicators <- c(
  "Raised blood pressure, Male (%)"      
  ,"Raised blood pressure, Female (%)"    
  ,"Raised blood glucose, Male (%)"       
  ,"Raised blood glucose, Female (%)"     
  ,"Raised blood cholesterol, Male (%)"   
  ,"Raised blood cholesterol, Female (%)"
)
c6indicators <- c(
  "Adult overweight and obesity, Male (%)"  
  ,"Adult overweight and obesity, Female (%)"
  ,"Adult obesity, Male (%)"                 
  ,"Adult obesity, Female (%)" 
)
c7indicators <- c(
  "Antenatal care (4+ visits)"                           
  ,"Skilled attendant at birth"                           
  ,"Initiation of breastfeeding within 1 hour after birth"
  ,"Continued breastfeeding at 1 year"                    
  ,"Unmet need for family planning"
)
c9indicators <- c(
  "Undernourishment (%)"
  ,"Fruit and veg (grams)"
  ,"Non-staples (%)" 
)

c11indicators <- c(
  "Drinking.Basic"
  ,"Drinking.Limited"
  ,"Drinking.Surface water"
  ,"Drinking.Unimproved"
  ,"Drinking.Safely managed"
)

c12indicators <- c(
  "Sanitation.Basic"
  ,"Sanitation.Limited"
  ,"Sanitation.Safely managed"
  ,"Sanitation.Unimproved"
  ,"Sanitation.Open defecation"
)

c13indicators <- c(
  "Agriculture"
  ,"Education"
  ,"Health"
  ,"Social protection"
)

c11values <- list(
  "Surface water "=list(
    "water_surface2000" = "2000"
    ,"water_surface2010" = "2010"
    ,"water_surface2015" = "2015"
  ),
  "Unimproved"=list(
    "water_unimproved2000" = "2000"
    ,"water_unimproved2010" = "2010"
    ,"water_unimproved2015" = "2015"
  ),
  "Limited"=list(
    "water_limited2000" = "2000"
    ,"water_limited2010" = "2010"
    ,"water_limited2015" = "2015"
  ),
  "Basic"=list(
    "water_basic2000" = "2000"
    ,"water_basic2010" = "2010"
    ,"water_basic2015" = "2015"
  ),
  "Safely managed"=list(
    "water_safelymanaged2000" = "2000"
    ,"water_safelymanaged2010" = "2010"
    ,"water_safelymanaged2015" = "2015"
  )
)

c12values <- list(
  "Open defecation"=list(
    "san_open2000" = "2000"
    ,"san_open2010" = "2010"
    ,"san_open2015" = "2015"
  ),
  "Unimproved"=list(
    "san_unimproved2000" = "2000"
    ,"san_unimproved2010" = "2010"
    ,"san_unimproved2015" = "2015"
  ),
  "Limited"=list(
    "san_limited2000" = "2000"
    ,"san_limited2010" = "2010"
    ,"san_limited2015" = "2015"
  ),
  "Basic"=list(
    "san_basic2000" = "2000"
    ,"san_basic2010" = "2010"
    ,"san_basic2015" = "2015"
  ),
  "Safely managed "=list(
    "san_safelymanaged2000" = "2000"
    ,"san_safelymanaged2010" = "2010"
    ,"san_safelymanaged2015" = "2015"
  )
)

c13values <- list(
  "Agriculture "=list(
    "totag_ppp1990" = "1990"
    ,"totag_ppp2000" = "2000"
    ,"totag_ppp2010" = "2010"
    ,"totag_ppp2012" = "2012"
  ),
  "Education"=list(
    "toteducation_ppp1990" = "1990"
    ,"toteducation_ppp2000" = "2000"
    ,"toteducation_ppp2010" = "2010"
    ,"toteducation_ppp2012" = "2012"
  ),
  "Health"=list(
    "tothealth_ppp1990" = "1990"
    ,"tothealth_ppp2000" = "2000"
    ,"tothealth_ppp2010" = "2010"
    ,"tothealth_ppp2012" = "2012"
  ),
  "Social protection"=list(
    "totsp_ppp1990" = "1990"
    ,"totsp_ppp2000" = "2000"
    ,"totsp_ppp2010" = "2010"
    ,"totsp_ppp2012" = "2012"
  )
)

c14indicators <- c(
  "Bringing people into a shared space for action (%)"
  ,"Ensuring a coherent policy and legal framework (%)"    
  ,"Aligning actions around a common results framework (%)"
  ,"Financial tracking and resource mobilization (%)"      
  ,"Total weighed score (%)" 
)

####End setup####
####Loop####
#Don't forget to set year, maybe scale exemptions for Global
regions <- c("Eastern Africa")

for(this.region in regions){
  message(this.region)
  dir.create(paste(wd,this.region,sep="/"))
  setwd(paste(wd,this.region,sep="/"))
  regiondat <- subset(dat,region==this.region)
  
  #Chart 5
  c5data <- subset(regiondat,indicator %in% c5indicators)
  c5data$value <- as.numeric(c5data$value)
  c5data$sex <- "Men"
  c5data$sex[which(grepl("Female",c5data$indicator))] <- "Women"
  c5data$indicator <- paste(sapply(strsplit(c5data$indicator,split=","),`[[`,index=1),"(%),",c5data$year)
  c5.max <- max(c5data$value,na.rm=TRUE)
  c5data$sex <- factor(c5data$sex,levels=c("Both sexes","Men","Women"))
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
    ) + geom_text(size=10,aes(label=safeFormat(value)),position=position_dodge(0.8),vjust=-0.3,color="#443e42")
  #Chart 6
  c6data <- subset(regiondat,indicator %in% c6indicators)
  c6data$value <- as.numeric(c6data$value)
  c6data$sex <- "Men"
  c6data$sex[which(grepl("Female",c6data$indicator))] <- "Women"
  c6data$indicator <- sapply(strsplit(c6data$indicator,split=","),`[[`,index=1)
  c6.max <- max(c6data$value,na.rm=TRUE)
  c6data$sex <- factor(c6data$sex,levels=c("Women","Men","Both sexes"))
  c6 <- ggplot(c6data,aes(sex,value,fill=indicator)) +
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.8,color="transparent") +
    scale_fill_manual(
      labels=c(expression("Obesity (BMI ">="30)"),expression("Overweight (BMI ">="25)"))
      ,breaks=c("Adult obesity","Adult overweight and obesity")
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
    ) + geom_text(size=10,aes(label=safeFormat(value)),position=position_dodge(0.8),hjust=-0.2,color="#443e42") + coord_flip()
  #Chart 7
  c7data <- subset(regiondat,indicator %in% c7indicators)
  c7data$value <- as.numeric(c7data$value)
  c7data$valpos <- c7data$value
  unmetNeed <- subset(c7data,indicator==c7indicators[5])$value
  metNeed <- 100-unmetNeed
  metNeedFrame <- data.frame(value=unmetNeed,ypos=5,color=orange,outline=orange,vallab=unmetNeed,valpos=95-unmetNeed,superscript="",sspos=0,label="")
  c7data$ypos <- c(1:5)
  c7data$color <- c(purple,purple,purple,purple,"transparent")
  c7data$outline <- c(purple,purple,purple,purple,purple)
  c7data$vallab <- c7data$value
  c7data$vallab[5] <- ""
  c7data$valpos[5] <- NA
  c7data$value[5] <- metNeed
  c7data$superscript <- c(1,1,1,1,2)
  c7data$sspos <- c(0.45,0.44,0.68,0.53,0.51)
  c7data$label <- paste0(c7data$indicator," ",c7data$year," (n = ",c7data$n,")")
  c7data <- data.frame(rbindlist(list(c7data,metNeedFrame),fill=TRUE))

  c7missing <- sum(is.na(c7data$value))
  if(nrow(c7data)==0 | c7missing==5){
    c7 = no.data
  }else{
    c7data$ypos <- (max(c7data$ypos)+1)-c7data$ypos
    c7max <- max(c7data$value,na.rm=TRUE)
    ss.adj = max(c7max*1.1,100)
    c7data$sspos = c7data$sspos * ss.adj
    c7yposmax <- max(c7data$ypos)
    ss.y.adj = 0.1/(6-c7yposmax)
    c7 <- ggplot(c7data,aes(y=value,x=ypos,fill=color
                            # ,colour=outline
    )) +
      geom_bar(stat="identity",width=0.4,position=position_stack(reverse=TRUE)) +
      scale_fill_identity() +
      # scale_color_identity() +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c7max*1.1) +
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
        ,axis.text.y = element_blank()
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) + geom_text(size=12,aes(label=vallab,y=valpos),hjust=-0.2,color="#443e42") +
      geom_text(size=12,aes(label=label,y=1,x=ypos+0.25),hjust=0,vjust=0,color="#443e42") +
      geom_text(size=9,aes(label=superscript,y=sspos,x=ypos+0.25+ss.y.adj),hjust=0,vjust=0,color="#443e42") +
      coord_flip()
  }
  
  #Chart 9 part a and b
  c9data <- subset(regiondat,indicator %in% c9indicators)
  setnames(c9data,"indicator","variable")
  c9data$value <- as.numeric(c9data$value)
  c9data <- subset(c9data,!is.na(value))
  if(nrow(c9data)!=0){
    c9a.data <- subset(c9data,variable==c9indicators[1])
    c9a.data <- subset(c9a.data,!is.na(value))
    c9b.data <- subset(c9data,variable==c9indicators[2])
    c9b.data <- subset(c9b.data,!is.na(value))
    c9c.data <- subset(c9data,variable==c9indicators[3])
    c9c.data <- subset(c9c.data,!is.na(value))
    c9a.max <- max(c9a.data$value,na.rm=TRUE)
    c9b.max <- max(c9b.data$value,na.rm=TRUE)
    c9c.max <- max(c9c.data$value,na.rm=TRUE)
    if(nrow(c9a.data)!=0){
      c9a <- ggplot(c9a.data,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color="transparent",show.legend=FALSE) +
        geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
        scale_fill_manual(
          labels=c(bquote(atop('Undernourishment' ^ 1,'(% population)')))
          ,breaks=c(c9indicators[1])
          ,values=c(yellow)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c9a.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color="#443e42")
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color="#443e42", size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9a.missing <- FALSE
    }else{
      c9a <- no.data
      c9a.missing <- TRUE
    }
    if(nrow(c9b.data)!=0){
      c9b <- ggplot(c9b.data,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color="transparent",show.legend=FALSE) +
        geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
        scale_fill_manual(
          labels=c(bquote(atop('Availability of fruit and' ^ 1,'vegetables (grams)')))
          ,breaks=c(c9indicators[2])
          ,values=c(orange)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c9b.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color="#443e42")
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color="#443e42", size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9b.missing <- FALSE
    }else{
      c9b <- no.data
      c9b.missing <- TRUE
    }
    if(nrow(c9c.data)!=0){
      c9c <- ggplot(c9c.data,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color="transparent",show.legend=FALSE) +
        geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
        scale_fill_manual(
          labels=c(bquote(atop('% of total calories' ^ 2,'from non-staples')))
          ,breaks=c(c9indicators[3])
          ,values=c(purple)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c9c.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color="#443e42")
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color="#443e42", size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9c.missing <- FALSE
    }else{
      c9c <- no.data
      c9c.missing <- TRUE
    }
  }else{
    c9a <- no.data
    c9b <- no.data
    c9c <- no.data
    c9a.missing <- TRUE
    c9b.missing <- TRUE
    c9c.missing <- TRUE
  }
 
  #Chart 11
  c11data <- subset(regiondat,indicator %in% c11indicators)
  c11data$value <- as.numeric(c11data$value)
  c11data$indicator <- sapply(strsplit(c11data$indicator,split=".",fixed=TRUE),`[[`,index=2)
  c11.max <- max(c11data$value,na.rm=TRUE)
  c11data$year <- factor(c11data$year)
  c11data$indicator <- factor(c11data$indicator)
  c11data <- c11data[complete.cases(c11data$year),]
  c11data <- c11data[order(is.na(c11data$value),c11data$year,desc(c11data$indicator)),]
  c11data <- ddply(c11data, .(year),
                   transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c11data <- subset(c11data,valid>=1)
  c11 <- ggplot(c11data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=0.7,color="transparent") +
    scale_fill_manual(
      labels=names(c11values)
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
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
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c11data,value>1),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=names(c11values),values=c(black,black,white,black,black),drop=FALSE)
  #Chart 12
  c12data <- subset(regiondat,indicator %in% c12indicators)
  c12data$value <- as.numeric(c12data$value)
  c12data$indicator <- sapply(strsplit(c12data$indicator,split=".",fixed=TRUE),`[[`,index=2)
  c12.max <- max(c12data$value,na.rm=TRUE)
  c12data$year <- factor(c12data$year)
  c12data$indicator <- factor(c12data$indicator)
  c12data <- c12data[complete.cases(c12data$year),]
  c12data <- c12data[order(is.na(c12data$value),c12data$year,desc(c12data$indicator)),]
  c12data <- ddply(c12data, .(year),
                   transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c12data <- subset(c12data,valid>=1)
  c12 <- ggplot(c12data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=0.7,color="transparent") +
    scale_fill_manual(
      labels=names(c12values)
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
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
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c12data,value>1),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=names(c12values),values=c(black,black,white,black,black),drop=FALSE)
  #Chart 13
  c13data <- subset(regiondat,indicator %in% c13indicators)
  c13data$value <- as.numeric(c13data$value)
  c13.max <- max(c13data$value,na.rm=TRUE)
  c13data$year <- factor(c13data$year)
  c13data$indicator <- factor(c13data$indicator)
  c13data <- c13data[complete.cases(c13data$year),]
  c13data <- c13data[order(is.na(c13data$value),c13data$year,desc(c13data$indicator)),]
  c13data <- ddply(c13data, .(year),
                   transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c13data <- subset(c13data,valid>=1)
  if(nrow(c13data)>0){
    
    
    c13 <- ggplot(c13data,aes(year,value,fill=indicator)) +
      geom_bar(stat="identity",width=0.7,color="transparent") +
      scale_fill_manual(
        labels=names(c13values)
        ,values=quintileFillValues
        ,drop = FALSE
      ) +
      guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
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
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(data=subset(c13data,value>0),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
      scale_color_manual(breaks=names(c13values),values=c(black,black,white,black,black),drop=FALSE)
  }else{
    c13 <- no.data
  }
  #Chart 14
  c14data <- subset(regiondat,indicator %in% c14indicators)
  c14data$value <- as.numeric(c14data$value)
  c14data$label = c(
      "Bringing people into a shared space for action"
      ,"Ensuring a coherent policy and legal framework"
      ,"Aligning actions around a common results framework"
      ,"Financial tracking and resource mobilisation"
      ,"Total weighted"
    )
  c14data$ypos= c(5:1)
  c14data$color=c(yellow,orange,purple,blue,grey)
  if(nrow(c14data)==0){
    c14 <- no.data
  }else{
    c14max = 100
    c14 <- ggplot(c14data,aes(y=value,x=ypos)) +
      geom_bar(aes(y=100),stat="identity",width=0.03) +
      geom_bar(aes(fill=color),stat="identity",width=0.1) +
      scale_fill_identity() +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c14max*1.1) +
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
        ,axis.line.y = element_blank()
        ,axis.text.x = element_blank()
        ,axis.text.y = element_blank()
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) + geom_text(size=10,aes(y=100,x=ypos+0.25,label=safeFormat(value)),hjust=1,vjust=0,color="#443e42") +
      geom_text(size=9,aes(label=label,y=1,x=ypos+0.25),hjust=0,vjust=0,color="#443e42") +
      coord_flip()
  }

  Cairo(file="c5.png",width=1000,height=400,units="px",bg="white")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c6.png",width=1200,height=280,units="px",bg="white")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c7.png",width=1600,height=700,units="px",bg="white")
  tryCatch({print(c7)},error=function(e){message(e);print(no.data)})
  dev.off()
  
  #Have c9a, c9b, and c9c
  if(!c9a.missing && !c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9a and c9b only
  if(!c9a.missing && !c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9a and c9c only
  if(!c9a.missing && c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9b and c9c only
  if(c9a.missing && !c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #have c9a only
  if(!c9a.missing && c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have c9b only
  if(c9a.missing && !c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #have c9c only
  if(c9a.missing && c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have none of c9s
  if(c9a.missing && c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  
  Cairo(file="c11.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c11)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c12.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c12)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c13.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c13)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c14.png",width=800,height=500,units="px",bg="white")
  tryCatch({print(c14)},error=function(e){message(e);print(no.data)})
  dev.off()
}
####End loop####
