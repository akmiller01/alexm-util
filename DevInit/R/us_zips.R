library(readr)
library(data.table)
library(Hmisc)

# tAHS2011 <- read_csv("~/Data/US data/AHS 2011 National and Metropolitan PUF v1.4 Flat CSV/tAHS2011.csv",quote="'")
# zcta_county_rel_10 <- read_csv("~/Data/US data/zcta_county_rel_10.txt")
# 
# save(tAHS2011,zcta_county_rel_10,zips,file="~/Data/US data/AHS 2011 National and Metropolitan PUF v1.4 Flat CSV/tAHS2011.RData")
load("~/Data/US data/AHS 2011 National and Metropolitan PUF v1.4 Flat CSV/tAHS2011.RData")


tAHS2011$DOWNPERCENT <- NA
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==0)] <- 0
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==1)] <- 2
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==2)] <- 5
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==3)] <- 10
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==4)] <- 15
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==5)] <- 20
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==6)] <- 40
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==7)] <- 99
tAHS2011$DOWNPERCENT[which(tAHS2011$DOWNPCT==8)] <- 100

tAHS2011$FIRSTHOME <- NA
tAHS2011$FIRSTHOME[which(tAHS2011$FRSTHO==1)] <- 0
tAHS2011$FIRSTHOME[which(tAHS2011$FRSTHO==2)] <- 1

tAHS2011$BATHS[which(tAHS2011$BATHS<0)] <- NA
tAHS2011$BEDRMS[which(tAHS2011$BATHS<0)] <- NA
tAHS2011$UNITSF[which(tAHS2011$UNITSF<0)] <- NA

tAHS2011$valpersf <- tAHS2011$VALUE/tAHS2011$UNITSF

fit <- lm(valpersf~BATHS+BEDRMS,data=tAHS2011)
summary(fit)

keep <- c("valpersf","VALUE",'BEDRMS',"BATHS","UNITSF","STATE","COUNTY")
hsSlim <- tAHS2011[keep]
hsSlim <- hsSlim[complete.cases(hsSlim),]
hsSlim$STATE <- as.integer(hsSlim$STATE)

coStats <- zcta_county_rel_10[
  c("STATE","COUNTY","COAREALAND","COHU","COPOP")
  ]
coStats <- unique(coStats)

hsSlim <- merge(hsSlim,coStats,by=c("STATE","COUNTY"),all.x=TRUE)

fit <- lm(log(VALUE)~log(UNITSF)+BEDRMS+BATHS+log(COAREALAND)+log(COHU)+log(COPOP),data=hsSlim)
summary(fit)

commuters <- read.csv("~/Data/US data/commuters_from.csv"
                      ,na.strings=""
                      ,colClasses=c("numeric","character","numeric","numeric"))
commuters <- data.table(commuters)[,.(COMMUTERS=sum(COMMUTERS,na.rm=TRUE)),by=.(STATE,COUNTY)]

# commutersIn <- read.csv("~/Data/US data/commuters_to.csv"
#                       ,na.strings=""
#                       ,colClasses=c("numeric","character","numeric"))
# commutersIn <- data.table(commutersIn)[,.(COMMUTERS_IN=sum(COMMUTERS_IN,na.rm=TRUE)),by=.(STATE,COUNTY)]

hsSlim <- merge(hsSlim,commuters,by=c("STATE","COUNTY"),all.x=TRUE)
# hsSlim <- merge(hsSlim,commutersIn,by=c("STATE","COUNTY"),all.x=TRUE)

fit <- lm(log(VALUE)~log(UNITSF)+BEDRMS+BATHS+log(COAREALAND)+log(COHU)+log(COPOP)+log(COMMUTERS),data=hsSlim)
summary(fit)
### Variation in home price is dependent on home-specific factors plus
### a 1% increase in county landarea results in a 0.13% decrease in price
### a 1% increase in housing units results in a 2.4% decrease in price
### a 1% increase in population results in a 0.44% increase in price
### and a 1% increase in commuting population results in a 2.14% increase in price

###So we want to look for a county that, all else equal has:
### A large land area
### A high number of housing units
### A small population
### and a small commuting population

coStats <- merge(coStats,commuters,by=c("STATE","COUNTY"),all=TRUE)

coStats <- transform(coStats,
    priceVariance = -0.131326*log(COAREALAND) + -2.404489*log(COHU) + 0.444121*log(COPOP) + 2.135840*log(COMMUTERS)
                       )


dmv <- subset(coStats,STATE %in% c(11,24,51))
zips <- zcta_county_rel_10[c("STATE","COUNTY","ZCTA5")]
zips$STATE <- as.integer(zips$STATE)
dmv <- merge(dmv,zips,all.x=TRUE)
dmv <- dmv[order(dmv$priceVariance),]


# distList <- list()
# distIndex <- 1
# fromZip = "20001"
# library(ggmap)
# for(zip in dmv$ZCTA5){
#   distDat <- mapdist(fromZip,zip,mode="driving")
#   distList[[distIndex]] <- distDat
#   distIndex <- distIndex + 1
# }
# distData <- rbindlist(distList,fill=TRUE)
# save(distData,file="~/Data/US data/distData.RData")
load("~/Data/US data/distData.RData")

names(distData)[2] = "ZCTA5"

dmv <- merge(dmv,distData,all.x=TRUE)
close <- subset(dmv,hours<=.75)
write.csv(dmv,"~/Data/US data/dmv.csv",na="",row.names=FALSE)

zStats <- zcta_county_rel_10[c("STATE","COUNTY","ZCTA5","ZHU","ZPOP","ZAREALAND")]
zStats <- transform(zStats,
                    priceVariance = -0.131326*log(ZAREALAND) + -2.404489*log(ZHU) + 0.444121*log(ZPOP)
)

zStats <- merge(zStats,distData,by="ZCTA5")
zStats <- subset(zStats,is.finite(priceVariance))
close_zips <- subset(zStats,minutes<=60)
