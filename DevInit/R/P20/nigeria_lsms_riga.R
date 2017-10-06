library(foreign)
library(ggplot2)
library(reshape2)
library(data.table)
library(Hmisc)
library(networkD3)
library(reshape)

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

wd <- "C:/Users/Alex/Documents/Data/Nigeria13/"
setwd(wd)

riga.income <- read.dta("Nigeria13_HH_INCOME_V12.dta")
riga.income.labs <- data.frame(names(riga.income),attributes(riga.income)[7])

riga.admin <- read.dta("Nigeria13_HH_ADMIN_V12.dta")
riga.admin.labs <- data.frame(names(riga.admin),attributes(riga.admin)[7])
weights <- riga.admin[c("hh","weight","pcexp")]

riga.income <- merge(riga.income,weights,by="hh")

riga.char <- read.dta("Nigeria13_HHCHAR_V12.dta")
riga.char.labs <- data.frame(names(riga.char),attributes(riga.char)[7])
wealth <- riga.char[c("hh","agwealth","wealth")]

riga.income <- merge(riga.income,wealth,by="hh")

riga.income <- riga.income[order(riga.income$totincome1),]
riga.income$rank <- c(1:nrow(riga.income))

ggplot(riga.income,aes(x=rank)) +
  geom_linerange(aes(ymin=totincome1,ymax=totincome2))

riga.sankey <- riga.income[c("totincome1","totincome2","weight")]
riga.sankey$t1.quint <- NA
riga.sankey$t2.quint <- NA

t1.quints <- weighted.percentile(riga.sankey$totincome1,riga.sankey$weight,seq(0,1,0.2))
t2.quints <- weighted.percentile(riga.sankey$totincome2,riga.sankey$weight,seq(0,1,0.2))
quint.names.t1 <- c("Poorest period 1","Second poorest period 1","Middle period 1","Second richest period 1","Richest period 1")
quint.names.t2 <- c("Poorest period 2","Second poorest period 2","Middle period 2","Second richest period 2","Richest period 2")
quint.names <- c(quint.names.t1,quint.names.t2)

for(i in 1:5){
  riga.sankey$t1.quint[which(riga.sankey$totincome1>=t1.quints[i])] <- quint.names.t1[i]
  riga.sankey$t2.quint[which(riga.sankey$totincome2>=t2.quints[i])] <- quint.names.t2[i]
  # riga.sankey$t2.quint[which(riga.sankey$totincome2>=t1.quints[i])] <- quint.names.t2[i]
  
}

income.changes <- expand.grid(t1.quint=quint.names.t1,t2.quint=quint.names.t2,stringsAsFactors=FALSE)
income.changes$percent <- NA
income.changes$t1.quint.index <- NA
income.changes$t2.quint.index <- NA

for(i in 1:nrow(income.changes)){
  numerator <- nrow(subset(riga.sankey,t1.quint==income.changes$t1.quint[i] & t2.quint==income.changes$t2.quint[i]))
  denominator <- nrow(subset(riga.sankey,t1.quint==income.changes$t1.quint[i]))
  income.changes$percent[i] <- (numerator/denominator)*100
  
  income.changes$t1.quint.index[i] <- which(quint.names == income.changes$t1.quint[i]) - 1
  income.changes$t2.quint.index[i] <- which(quint.names == income.changes$t2.quint[i]) - 1
}

nodes <- data.frame(name=quint.names)
links <- data.frame(income.changes)[c("t1.quint.index","t2.quint.index","percent")]
names(links) <- c("source","target","value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, units="%")

riga.sankey2 <- riga.income[c("wealth","agwealth","pcexp","totincome1","totincome2","weight")]
riga.sankey2 <- riga.sankey2[complete.cases(riga.sankey2),]
riga.sankey2$w.quint <- NA
riga.sankey2$agw.quint <- NA
riga.sankey2$exp.quint <- NA
riga.sankey2$t1.quint <- NA
riga.sankey2$t2.quint <- NA

w.quints <- weighted.percentile(riga.sankey2$wealth,riga.sankey2$weight,seq(0,1,0.2))
agw.quints <- weighted.percentile(riga.sankey2$agwealth,riga.sankey2$weight,seq(0,1,0.2))
exp.quints <- weighted.percentile(riga.sankey2$pcexp,riga.sankey2$weight,seq(0,1,0.2))
t1.quints <- weighted.percentile(riga.sankey2$totincome1,riga.sankey2$weight,seq(0,1,0.2))
t2.quints <- weighted.percentile(riga.sankey2$totincome2,riga.sankey2$weight,seq(0,1,0.2))
quint.names.w <- c("Poorest wealth","Second poorest wealth","Middle wealth","Second richest wealth","Richest wealth")
quint.names.agw <- c("Poorest agri wealth","Second poorest agri wealth","Middle agri wealth","Second richest agri wealth","Richest agri wealth")
quint.names.exp <- c("Poorest expenditure","Second poorest expenditure","Middle expenditure","Second richest expenditure","Richest expenditure")
quint.names.t1 <- c("Poorest income 1","Second poorest income 1","Middle income 1","Second richest income 1","Richest income 1")
quint.names.t2 <- c("Poorest income 2","Second poorest income 2","Middle income 2","Second richest income 2","Richest income 2")

for(i in 1:5){
  riga.sankey2$w.quint[which(riga.sankey2$wealth>=w.quints[i])] <- quint.names.w[i]
  riga.sankey2$agw.quint[which(riga.sankey2$agwealth>=agw.quints[i])] <- quint.names.agw[i]
  riga.sankey2$exp.quint[which(riga.sankey2$pcexp>=exp.quints[i])] <- quint.names.exp[i]
  riga.sankey2$t1.quint[which(riga.sankey2$totincome1>=t1.quints[i])] <- quint.names.t1[i]
  riga.sankey2$t2.quint[which(riga.sankey2$totincome2>=t2.quints[i])] <- quint.names.t2[i]
}

# steps <- c("w.quint","agw.quint","exp.quint","t1.quint","t2.quint")
# stepNames <- list(quint.names.w,quint.names.agw,quint.names.exp,quint.names.t1,quint.names.t2)
# quint.names <- c(quint.names.w,quint.names.agw,quint.names.exp,quint.names.t1,quint.names.t2)

# steps <- c("t1.quint","t2.quint")
# stepNames <- list(quint.names.t1,quint.names.t2)
# quint.names <- c(quint.names.t1,quint.names.t2)
steps <- c("w.quint","exp.quint","t1.quint","t2.quint")
stepNames <- list(quint.names.w,quint.names.exp,quint.names.t1,quint.names.t2)
quint.names <- c(quint.names.w,quint.names.exp,quint.names.t1,quint.names.t2)

linkList <- list()
linkIndex <- 1

for(j in 2:length(steps)){
  sourceStep = steps[j-1]
  targetStep = steps[j]
  income.changes = expand.grid(source=stepNames[[j-1]],target=stepNames[[j]],stringsAsFactors=FALSE)
  income.changes$source.index <- NA
  income.changes$target.index <- NA
  for(i in 1:nrow(income.changes)){
    numerator <- nrow(subset(riga.sankey2,
                             get(sourceStep)==income.changes$source[i] &
                               get(targetStep)==income.changes$target[i]
                             )
    )
    # denominator <- nrow(subset(riga.sankey2,
    #                          get(sourceStep)==income.changes$source[i]
    #                           )
    # )
    denominator <- nrow(riga.sankey2)
    if(denominator==0){denominator=1}
    income.changes$value[i] <- (numerator/denominator)*100
    
    income.changes$source.index[i] <- which(quint.names == income.changes$source[i]) - 1
    income.changes$target.index[i] <- which(quint.names == income.changes$target[i]) - 1
  }
  income.changes <- income.changes[c("source.index","target.index","value")]
  names(income.changes) <- c("source","target","value")
  linkList[[linkIndex]] <- income.changes
  linkIndex <- linkIndex + 1
}

nodes2 <- data.frame(name=quint.names)
links2 <- rbindlist(linkList)
# links <- links[which(links$value>0),]

sankeyNetwork(Links = links2, Nodes = nodes2,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30, units="%")

prefixes = c("poor","second.poorest","middle","second.richest","rich")
for(i in 1:5){
  varname = paste0("consistent.",prefixes[i])
  riga.sankey2[,varname] <- riga.sankey2$w.quint==quint.names.w[i] &
    riga.sankey2$exp.quint==quint.names.exp[i] &
    riga.sankey2$t1.quint==quint.names.t1[i] &
    riga.sankey2$t2.quint==quint.names.t2[i]
  message(prefixes[i]," ",mean(riga.sankey2[,varname],na.rm=TRUE))
}

connect <- riga.income[c("totincome1","totincome2")]
names(connect) <- c("income.1","income.2")
connect <- connect[order(connect$income.1),]
connect$rank.1 <- nrow(connect):1
connect <- connect[order(connect$income.2),]
connect$rank.2 <- nrow(connect):1
cut <- 1000
connect <- subset(connect,rank.1 > nrow(connect)-cut)
lconnect <- reshape(connect,direction="long",varying=c("income.1","income.2","rank.1","rank.2"))

ggplot(lconnect,aes(x=time,y=rank,group=id,colour=id)) + geom_line()
ggplot(lconnect,aes(x=time,y=income,group=id,colour=id)) + geom_line()

connect <- riga.income[c("totincome1","totincome2")]
names(connect) <- c("income.1","income.2")
connect <- connect[order(connect$income.1),]
connect$rank.1 <- nrow(connect):1
connect <- connect[order(connect$income.2),]
connect$rank.2 <- nrow(connect):1
cut <- 1000
connect <- subset(connect, rank.1 <= cut)
lconnect <- reshape(connect,direction="long",varying=c("income.1","income.2","rank.1","rank.2"))

ggplot(lconnect,aes(x=time,y=rank,group=id,colour=id)) + geom_line()
ggplot(lconnect,aes(x=time,y=income,group=id,colour=id)) + geom_line()
