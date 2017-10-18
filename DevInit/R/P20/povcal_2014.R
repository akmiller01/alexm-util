library(Hmisc)
library(data.table)
library(readr)
library(WDI)
library(varhandle)

wd <- "/Users/Alex/Documents/Data/PovCal_Increment"
setwd(wd)

load("agg.ind.RData")

iso.key <- read.csv("iso_key.csv",na.strings="")

ind <- merge(ind,iso.key,by="country",all.x=TRUE)
agg <- merge(agg,iso.key,by="country",all.x=TRUE)

hhexp.ind <- "NE.CON.PRVT.PP.KD"
pop.ind <- "SP.POP.TOTL"
library(WDI)
hhexp <- WDI(indicator=hhexp.ind,country='all',start=2013,end=2014,extra=TRUE)
hhexp$iso3c <- unfactor(hhexp$iso3c)
hhexp$iso3c[which(hhexp$country=="Kosovo")] <- "XKX"
hhexp$iso3c[which(hhexp$country=="Cabo Verde")] <- "CPV"
hhexp <- hhexp[,c("iso3c","year","NE.CON.PRVT.PP.KD")]
pop <- WDI(indicator=pop.ind,country='all',start=2013,end=2014,extra=TRUE)
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
hhexppc.wide$hh.growth <- (hhexppc.wide$hhexppc.2014/hhexppc.wide$hhexppc.2013)-1

regions <- unique(WDI(indicator=pop.ind,country='all',start=2013,end=2013,extra=TRUE)[,c("iso3c","region")])
regions <- regions[complete.cases(regions),]
add.regions <- data.frame(iso3c=c("CPV","XKX"),region=c("Sub-Saharan Africa (all income levels)","Europe & Central Asia (all income levels)"))
regions <- rbind(regions,add.regions)

gdp.growth <- WDI(indicator="NY.GDP.PCAP.KD.ZG",country="all",start=2014,end=2014,extra=TRUE)
gdp.growth$iso3c <- unfactor(gdp.growth$iso3c)
gdp.growth$iso3c[which(gdp.growth$country=="Kosovo")] <- "XKX"
gdp.growth$iso3c[which(gdp.growth$country=="Cabo Verde")] <- "CPV"
gdp.growth <- gdp.growth[,c("iso3c","NY.GDP.PCAP.KD.ZG")]
gdp.growth$NY.GDP.PCAP.KD.ZG <- (gdp.growth$NY.GDP.PCAP.KD.ZG/100)

ind$year <- round(ind$year)
surveys2014 <- as.character(unique(subset(ind,year==2014)$country)) # No need to project, can use in 2014, length = 68
surveys2014plus <- as.character(unique(ind$country))
surveys2015plus <- as.character(unique(subset(ind,year>=2015)$country)) # Need to interpolate
surveys2015plus <- surveys2015plus[which(!(surveys2015plus %in% surveys2014))] # Length = 8, including Myanmar (backwards interp)
countries <- unique(agg$country)
no2014plussurvey <- setdiff(countries,surveys2014plus) # Can project forward without interpolation, length=86

### Already in 2014
pcn2014 <- subset(ind, year==2014)
pcn2014 <- pcn2014[,c("country","iso3c","year","pl","hc","type")]

### Project forward 1 year without future survey
pcn2013project <- subset(agg,year==2013 & country %in% no2014plussurvey)

pcn2013growth <- data.frame(iso3c=unique(pcn2013project$iso3c))
pcn2013growth <- merge(pcn2013growth,regions,by="iso3c")
pcn2013growth <- merge(pcn2013growth,hhexppc.wide,by="iso3c",all.x=TRUE)
pcn2013growth <- merge(pcn2013growth,gdp.growth,by="iso3c",all.x=TRUE)
pcn2013growth$final.growth <- pcn2013growth$hh.growth
pcn2013growth$final.growth[which(is.na(pcn2013growth$final.growth))] <- pcn2013growth$NY.GDP.PCAP.KD.ZG[which(is.na(pcn2013growth$final.growth))]
afr <- c("Sub-Saharan Africa (all income levels)")
pcn2013growth$final.growth[which(pcn2013growth$region %in% afr)] <- pcn2013growth$NY.GDP.PCAP.KD.ZG[which(pcn2013growth$region %in% afr)]
#Special case of no national accounts
pcn2013growth$final.growth[which(pcn2013growth$iso3c=="SYR")] <- 0.01907235
pcn2013growth <- pcn2013growth[,c("iso3c","final.growth")]

pcn2013project <- merge(pcn2013project,pcn2013growth,by="iso3c")
pcn2013project$pl2014 <- pcn2013project$pl*(1+pcn2013project$final.growth)
pcn2013project$year <- 2014
pcn2013project <- pcn2013project[,c("country","iso3c","year","pl2014","hc","type")]
setnames(pcn2013project,"pl2014","pl")

pcn2014 <- rbind(pcn2014,pcn2013project)

### Interpolate between surveys
means2015 <- unique(subset(ind,country %in% surveys2015plus)[,c("iso3c","mean")])
means2015 <- subset(means2015,iso3c!="MMR" & mean!=223.020000000000010)
setnames(means2015,"mean","mean.2015")
means2013 <- unique(subset(agg,iso3c %in% means2015$iso3c & year==2013)[,c("iso3c","mean")])
setnames(means2013,"mean","mean.2013")
mean.growth <- merge(means2015,means2013,by="iso3c")
mean.growth <- transform(mean.growth, final.growth=((mean.2015-mean.2013)/mean.2013)/2 )

pcn2013interp <- subset(agg,year==2013 & iso3c %in% mean.growth$iso3c)
pcn2013interp <- merge(pcn2013interp,mean.growth,by="iso3c")
pcn2013interp$pl2014 <- pcn2013interp$pl*(1+pcn2013interp$final.growth)
pcn2013interp$year <- 2014
pcn2013interp <- pcn2013interp[,c("country","iso3c","year","pl2014","hc","type")]
setnames(pcn2013interp,"pl2014","pl")

pcn2014 <- rbind(pcn2014,pcn2013interp)

### Myanmar, project backwards
gdp.growth.mmr <- WDI(indicator="NY.GDP.PCAP.KD.ZG",country="all",start=2015,end=2015,extra=TRUE)
gdp.growth.mmr <- subset(gdp.growth.mmr,iso3c=="MMR")$NY.GDP.PCAP.KD.ZG/100
mmr <- subset(ind,iso3c=="MMR")
mmr$pl2014 <- mmr$pl / (1+gdp.growth.mmr)
mmr$year <- 2014
mmr <- mmr[,c("country","iso3c","year","pl2014","hc","type")]
setnames(mmr,"pl2014","pl")

pcn2014 <- rbind(pcn2014,mmr)

pov2014 <- subset(pcn2014,abs(pl-1.90)<0.006)
pov2014 <- subset(pov2014, ( !(iso3c %in% c("MEX","POL")) | type=="c" ) )
pov2014 <- data.table(pov2014)[,.(pl=mean(pl),hc=mean(hc)),by=.(country,iso3c,year,type)]
pov2014$hc <- pov2014$hc/100
pop2014 <- subset(pop,year==2014)
pop2014 <- pop2014[complete.cases(pop2014),]
pov2014 <- merge(pov2014,pop2014,by=c("iso3c","year"))
pov2014$poor.pop <- pov2014$hc * pov2014$SP.POP.TOTL
sum(pov2014$poor.pop)
sum(subset(pop,year==2014 & iso3c %in% pov2014$iso3c)$SP.POP.TOTL)
global.pop <- 7268986175.7391596
#Coverage
sum(subset(pop,year==2014 & iso3c %in% pov2014$iso3c)$SP.POP.TOTL)/global.pop
#Global poverty hc
sum(pov2014$poor.pop)/global.pop

save(pcn2014,pov2014,global.pop,file="pcn2014.RData")

agg.means <- unique(subset(agg,year %in% c(2012,2013) & svyYear %in% c(2012,2013) & year==svyYear)[,c("country","year","type","iso3c","mean","svyYear")])
agg.means$id <- paste0(agg.means$iso3c,agg.means$type) 
agg.means <- agg.means[,c("id","year","mean")]
library(reshape)
agw <- reshape(agg.means,direction="wide",idvar="id",timevar="year")
agw <- agw[complete.cases(agw),]
agw$delta.percent <- 100*((agw$mean.2013-agw$mean.2012)/agw$mean.2012)
