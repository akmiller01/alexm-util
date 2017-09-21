
pcniso <- interp
pcniso$iso3c <- pcniso$country
pcniso$income2014 <- pcniso$x2013_pl
pcniso$hc2013 <- pcniso$hc

varname1 <- "income2013"
varname2 <- "income2014"
hc1 <- "hc2013"
hc2 <- "hc2014"

order_and_assign = function (x){
  step = 100/length(x)
  newSeq = seq(step,100,step)
  newSeq = newSeq[order(order(x))]
  return(newSeq)
}

newHC <- data.table(pcniso)[,.(
  oldHC = get(hc1),
  newHC = order_and_assign(get(varname2))
),by=.(iso3c)]

setnames(newHC,"newHC",hc2)
setnames(newHC,"oldHC",hc1)
pcniso <- merge(pcniso,newHC,by=c("iso3c",hc1))

for(i in 1:1000000){
  if(i %% 100000 == 0){
    message(i)
  }
}
