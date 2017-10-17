library(Hmisc)
library(data.table)
library(readr)
library(WDI)
library(varhandle)

wd <- "/Users/Alex/Documents/Data/PovCal_Increment"
setwd(wd)

# agg.header <- c(
#   "country"
#   ,"year"
#   ,"type"
#   ,"pl"
#   ,"mean"
#   ,"hc"
#   ,"pg"
#   ,"pg2"
#   ,"watts"
#   ,"gini"
#   ,"median"
#   ,"mld"
#   ,"pop"
#   ,"svyYear"
#   ,"detail"
# )
# 
# agg <- read_csv("regional.csv",col_names=agg.header)
# 
# ind.header <- c(
#   "country"
#   ,"year"
#   ,"type"
#   ,"mean"
#   ,"pl"
#   ,"hc"
#   ,"pg"
#   ,"pg2"
#   ,"watts"
#   ,"gini"
#   ,"median"
#   ,"mld"
#   ,"pop"
#   ,"detail"
# )
# 
# ind <- read_csv("individual.csv",col_names=ind.header)
# 
# ind <- subset(ind,!grepl("--",country,fixed=TRUE))
# ind <- data.table(ind)
# 
# agg <- subset(agg,!grepl("--",country,fixed=TRUE))
# agg <- data.table(agg)
# agg.dup <- agg[,.(count=sum(!is.na(type))),by=.(country,year,pl)]
# agg.dup <- subset(agg.dup,count>1)
# agg.dup$id <- paste(agg.dup$country,agg.dup$year,agg.dup$pl)
# agg$id <- paste(agg$country,agg$year,agg$pl)
# all.dups <- agg[which(agg$id %in% agg.dup$id),]
# good.dups <- subset(all.dups,svyYear=="Interpolated")
# agg <- agg[!(agg$id %in% agg.dup$id),]
# agg <- rbind(agg,good.dups)
# agg[,("id"):=NULL]
# 
# 
# agg <- agg[order(-agg$year,agg$pl,agg$country),]

# save(agg,ind,file="agg.ind.RData")
load("agg.ind.RData")

# iso3s <- WDI(country="all",indicator="SP.POP.TOTL",start=1990,end=2015,extra=TRUE)
# iso3s <- unique(iso3s[c("country","iso3c")])
# iso3s <- iso3s[complete.cases(iso3s),]
# countries <- unique(c(agg$country,ind$country))
# countries <- data.frame(country=countries)
# countries <- merge(countries,iso3s,by="country",all.x=TRUE)
# write.csv(countries,"iso_key.csv",na="",row.names=FALSE)

iso.key <- read.csv("iso_key.csv",na.strings="")

ind <- merge(ind,iso.key,by="country",all.x=TRUE)
agg <- merge(agg,iso.key,by="country",all.x=TRUE)

growth <- read.csv("GDPgrowthRates.csv",na.strings="")
lkg10 <- growth[c("iso3c","Laknergrowth10")]
lkg20 <- growth[c("iso3c","Laknergrowth20")]

countries <- unique(agg$country)
agg2013 <- subset(agg,year==2013 & svyYear %in% c("2013","Weighted sum"))
countries2013 <- unique(agg2013$country)
not2013 <- setdiff(countries,countries2013)
not2013 <- c(not2013,"India*")

agg.test <- subset(agg,country %in% not2013)
agg.test2012 <- subset(agg.test,year==2012)
agg.test2013 <- subset(agg,year==2013 & country %in% not2013)

agg.test2012 <- merge(agg.test2012,lkg20,by="iso3c")
agg.test2012$pl2013 <- agg.test2012$pl*(1+agg.test2012$Laknergrowth20)
agg.test2012 <- agg.test2012[,c("country","pl2013","hc")]

agg.mean2012 <- unique(subset(agg,year==2012)[,c("country","iso3c","mean")])
agg.mean2013 <- unique(subset(agg,year==2013)[,c("country","iso3c","mean")])
setnames(agg.mean2012,"mean","mean2012")
setnames(agg.mean2013,"mean","mean2013")
agg.mean <- merge(agg.mean2012,agg.mean2013,by=c("country","iso3c"))
agg.mean <- transform(agg.mean,mean.growth = (mean2013/mean2012) - 1)
agg.mean <- subset(agg.mean,country %in% not2013)

hhexp.ind <- "NE.CON.PRVT.PP.KD"
pop.ind <- "SP.POP.TOTL"
library(WDI)
hhexp <- WDI(indicator=hhexp.ind,country='all',start=2012,end=2013,extra=TRUE)
hhexp$iso3c <- unfactor(hhexp$iso3c)
hhexp$iso3c[which(hhexp$country=="Kosovo")] <- "XKX"
hhexp$iso3c[which(hhexp$country=="Cabo Verde")] <- "CPV"
hhexp <- hhexp[,c("iso3c","year","NE.CON.PRVT.PP.KD")]
pop <- WDI(indicator=pop.ind,country='all',start=2012,end=2013,extra=TRUE)
pop$iso3c <- unfactor(pop$iso3c)
pop$iso3c[which(pop$country=="Kosovo")] <- "XKX"
pop$iso3c[which(pop$country=="Cabo Verde")] <- "CPV"
pop <- pop[,c("iso3c","year","SP.POP.TOTL")]
hhexppc <- merge(hhexp,pop)
hhexppc <- transform(hhexppc,hhexppc=NE.CON.PRVT.PP.KD/SP.POP.TOTL)
hhexppc <- hhexppc[,c("iso3c","year","hhexppc")]
library(reshape)
hhexppc.wide <- reshape(hhexppc,idvar="iso3c",direction="wide",timevar="year")
hhexppc.wide <- hhexppc.wide[complete.cases(hhexppc.wide),]
hhexppc.wide$hh.growth <- (hhexppc.wide$hhexppc.2013/hhexppc.wide$hhexppc.2012)-1

agg.mean <- merge(agg.mean,hhexppc.wide,by="iso3c")
regions <- unique(WDI(indicator=pop.ind,country='all',start=2013,end=2013,extra=TRUE)[,c("iso3c","region")])
regions <- regions[complete.cases(regions),]
options(digits=22)
agg.mean <- merge(agg.mean,regions,by="iso3c")
gdp.growth <- WDI(indicator="NY.GDP.PCAP.KD.ZG",country="all",start=2013,end=2013,extra=TRUE)[,c("iso3c","NY.GDP.PCAP.KD.ZG")]
gdp.growth$NY.GDP.PCAP.KD.ZG <- (gdp.growth$NY.GDP.PCAP.KD.ZG/100)
agg.mean <- merge(agg.mean,gdp.growth,by="iso3c")

agg.mean$final.growth <- agg.mean$hh.growth
afr <- c("Sub-Saharan Africa (all income levels)")
agg.mean$final.growth[which(agg.mean$region %in% afr)] <- agg.mean$NY.GDP.PCAP.KD.ZG[which(agg.mean$region %in% afr)]
agg.mean$diff <- agg.mean$mean.growth - agg.mean$final.growth
agg.mean$percent.diff <- (agg.mean$mean.growth - agg.mean$final.growth)/agg.mean$final.growth
agg.mean$essentially.zero <- abs(agg.mean$diff)<0.01

new.surveys <- unique(ind$country)
new <- data.frame(new=TRUE,country=new.surveys)
agg.mean <- merge(agg.mean,new,by="country",all.x=TRUE)
agg.mean$new[which(is.na(agg.mean$new))] <- FALSE
# agg.mean <- merge(agg.mean,lkg20,by="iso3c")
# agg.mean$diff <- agg.mean$mean.growth - agg.mean$Laknergrowth20
# agg.mean$essentially.zero <- abs(agg.mean$diff)<0.01

agg.tab <- data.table(agg.mean)[,.(
  mean.diff=mean(diff)
  ,mean.zero=mean(essentially.zero)
  ),by=.(region,new)]

# ago12 <- subset(agg,country=="Angola" & year==2012)
# ago13 <- subset(agg,country=="Angola" & year==2013)
# setnames(ago12,"hc","hc2012")
# setnames(ago13,"hc","hc2013")
# ago12 <- ago12[,c("pl","hc2012")]
# ago13 <- ago13[,c("pl","hc2013")]
# ago <- merge(ago12,ago13)
# agom <- melt(ago,id.var="pl")
# library(ggplot2)
# ggplot(subset(agom,pl<15 & pl>0),aes(x=pl,y=value,group=variable,color=variable)) + geom_line()
# years <- unique(pcn$year)
# for(this.year in years){
#   filename <- paste0("years/pcn",this.year,".csv")
#   message(filename)
#   write.csv(subset(pcn,year==this.year),filename,na="",row.names=FALSE)
# }