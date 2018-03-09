library(WDI)

pop = WDI("SP.POP.TOTL",country="all",extra=TRUE,start=1996,end=2016)

incomes <- unique(pop[,c("iso3c","income")])
incomes <- incomes[complete.cases(incomes),]
names(incomes) <- c("RecipientISO3","WBincome")
setwd("C:/Users/Alex/Documents/Data/ITEP2018")
write.csv(incomes,"wb_incomes.csv",na="",row.names=FALSE)
