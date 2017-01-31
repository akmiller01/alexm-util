path<- "D:/git/digital-platform/country-year/"
setwd(path)

df <- read.csv("./domestic.csv",colClasses=c("character","numeric","character","character","character","character","character","character","character","numeric","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)

totalRevGrants <- subset(df,l1=="total-revenue-and-grants" & is.na(l2))
grantPhrases <- c("grants","official-grants","total-grants")
totalGrants <- subset(df,l1=="total-revenue-and-grants" & (l2 %in% grantPhrases) & is.na(l3))
# setdiff(unique(totalRevGrants$id),unique(totalGrants$id))
# jmGrants <- subset(df,id=="JM" & l3=="grants" & is.na(l4))
# totalGrants <- rbind(totalGrants,jmGrants)
keep <- c("id","year","value.ppp","budget.type")
totalRevGrants <- totalRevGrants[keep]
totalGrants <- totalGrants[keep]
dat <- merge(
  totalRevGrants
  ,totalGrants
  ,by=c("id","year","budget.type")
  ,suffix=c(".total",".grant")
  ,all.x=TRUE
  )
dat <- transform(dat,value.ng=(value.ppp.total-value.ppp.grant),value.total=value.ppp.total)
keep <- c("id","year","value.ng","value.total","budget.type")
dat <- dat[keep]

pop <- read.csv("./weo-population-total.csv",colClasses=c("character","numeric","numeric"), header = TRUE,sep=",",na.strings="",stringsAsFactors=FALSE)
names(pop)[3] <- "pop"
dat <- merge(
  dat,
  pop,
  by=c("id","year"),
  all.x=TRUE
  )

dat <- transform(dat,value.ng.pc=value.ng/pop,value.total.pc=value.total/pop)

dat.ng <- dat[c("id","year","value.ng.pc","budget.type")]
names(dat.ng) <- c("id","year","value","budget-type")
dat.total <- dat[c("id","year","value.total.pc","budget.type")]
names(dat.total) <- c("id","year","value","budget-type")

write.csv(dat.ng,"./non-grant-revenue-PPP-capita.csv",row.names=FALSE,na="")
write.csv(dat.total,"./total-revenue-PPP-capita.csv",row.names=FALSE,na="")

