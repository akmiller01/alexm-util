library(Hmisc)
library(data.table)
library(survey)

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
person.weight <- function(ages){
  ages <- ages[complete.cases(ages)]
  if(length(ages)==0){
    return(0)
  }else if(length(ages)==1){
    return(1)
  }else if(length(ages)>1){
    weight <- 1
    ages <- ages[order(ages)]
    oldest <- ages[length(ages)]
    ages.without.oldest <- ages[1:(length(ages)-1)]
    for(age in ages.without.oldest){
      if(age>=14){
        weight <- weight + 0.5
      }else{
        weight <- weight + 0.3
      }
    }
    return(weight)
  }
}


hm <- read.LIS("ch13p")
nrow(hm)

pov.weight.tab <- data.table(hm)[,.(pov.weight=person.weight(age)),by=.(hid)]
describe(pov.weight.tab$pov.weight)

hh <- read.LIS("ch13h") 
nrow(hh)

describe(hh$hi)

hh <- merge(hh,pov.weight.tab,by=c("hid"),all.x=TRUE)

hh$pchi <- (hh$hi/365)/hh$pov.weight

median.income <- weighted.percentile(hh$pchi,hh$hwgt,0.5)
median.income
hh$nat.pov <- hh$pchi < (median.income*0.6)
income.quintiles <- weighted.percentile(hh$pchi,hh$hwgt,c(0.2,0.4,0.6,0.8,1.0))
income.quintiles
hh$poorest <- (hh$pchi < income.quintiles[1])
hh$second.poorest <- (hh$pchi >= income.quintiles[1]) & (hh$pchi < income.quintiles[2])
hh$middle <- (hh$pchi >= income.quintiles[2]) & (hh$pchi < income.quintiles[3])
hh$second.richest <- (hh$pchi >= income.quintiles[3]) & (hh$pchi < income.quintiles[4])
hh$richest <- (hh$pchi >= income.quintiles[4])

keep <- c("hid","hi","pchi","pov.weight","nat.pov","poorest","second.poorest","middle","second.richest","richest","region_c","rural")
hh.pov <- hh[keep]
hm <- merge(hm,hh.pov,by="hid",all.x=TRUE)


hm$age.cat <- NA
for(i in 0:16){
  hm$age.cat[which(hm$age>(i*5))] <- i
}
describe(hm$age.cat)

dsn <- svydesign(id=~1,weights=~pwgt,data=hm)
summary(dsn)

ruraltab <- svytable(~rural+nat.pov,dsn)
summary(ruraltab)
prop.table(ruraltab,1)
ruralp20 <- svytable(~rural+poorest,dsn)
summary(ruralp20)
prop.table(ruralp20,1)

regtab <- svytable(~region_c+nat.pov,dsn)
summary(regtab)
prop.table(regtab,1)
regp20 <- svytable(~region_c+poorest,dsn)
summary(regp20)
prop.table(regp20,1)

eductab <- svytable(~educ_c+nat.pov,dsn)
summary(eductab)
prop.table(eductab,1)
educp20 <- svytable(~educ_c+poorest,dsn)
summary(educp20)
prop.table(educp20,1)

sextab <- svytable(~sex+nat.pov,dsn)
summary(sextab)
prop.table(sextab,1)
sexp20 <- svytable(~sex+poorest,dsn)
summary(sexp20)
prop.table(sexp20,1)

immtab <- svytable(~immigr+nat.pov,dsn)
summary(immtab)
prop.table(immtab,1)
immp20 <- svytable(~immigr+poorest,dsn)
summary(immp20)
prop.table(immp20,1)

birthtab <- svytable(~ctrybrth+nat.pov,dsn)
summary(birthtab)
prop.table(birthtab,1)
birthp20 <- svytable(~ctrybrth+poorest,dsn)
summary(birthp20)
prop.table(birthp20,1)

disabtab <- svytable(~disabl_c+nat.pov,dsn)
summary(disabtab)
prop.table(disabtab,1)
disabp20 <- svytable(~disabl_c+poorest,dsn)
summary(disabp20)
prop.table(disabp20,1)

unemtab <- svytable(~unemp+nat.pov,dsn)
summary(unemtab)
prop.table(unemtab,1)
unemp20 <- svytable(~unemp+poorest,dsn)
summary(unemp20)
prop.table(unemp20,1)

clfstab <- svytable(~clfs+nat.pov,dsn)
summary(clfstab)
prop.table(clfstab,1)
clfsp20 <- svytable(~clfs+poorest,dsn)
summary(clfsp20)
prop.table(clfsp20,1)

childtab <- svytable(~nchildren+nat.pov,dsn)
summary(childtab)
prop.table(childtab,1)
childp20 <- svytable(~nchildren+poorest,dsn)
summary(childp20)
prop.table(childp20,1)

age.cattab <- svytable(~age.cat+nat.pov,dsn)
summary(age.cattab)
prop.table(age.cattab,1)
age.catp20 <- svytable(~age.cat+poorest,dsn)
summary(age.catp20)
prop.table(age.catp20,1)