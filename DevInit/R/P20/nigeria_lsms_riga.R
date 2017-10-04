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
weights <- riga.admin[c("hh","weight")]

riga.income <- merge(riga.income,weights,by="hh")

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
