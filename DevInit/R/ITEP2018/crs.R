library(data.table)
library(readr)
library(ggplot2)

wd <- "C:/Users/Alex/Documents/Data/CRS"
setwd(wd)

# data.list <- list()
# txts <- list.files(wd,"*.txt")
# 
# for(txt in txts){
#   message(txt)
#   tmp.dat <- read_delim(txt,delim="|")
#   data.list[[txt]] <- tmp.dat
# }
# 
# crs <- rbindlist(data.list,fill=TRUE)
# save(crs,file="CRS_combined.RData")
load("CRS_combined.RData")

crs20 <- subset(crs,Year %in% c(1996:2016))
rm(crs)
gc()

#0A - Standard grants
standard_grants <- subset(crs20,Finance_t==110)
standard_grants$Finance_type_verbose <- "Standard grants"
#0B - Standard loans
standard_loans <- subset(crs20,Finance_t==421)
standard_loans$Finance_type_verbose <- "Standard loans"
#0C - Common equity
common_equity <- subset(crs20,Finance_t==510)
common_equity$Finance_type_verbose <- "Common equity"
#0D - Other equity
other_equity <- subset(crs20,Finance_t %in% c(432,520,530))
other_equity$Finance_type_verbose <- "Other equity"
#0E - Guarantees
guarantees <- subset(crs20,Finance_t==1100)
guarantees$Finance_type_verbose <- "Guarantees"
#0F - subordinated loans
subordinated_loans <- subset(crs20,Finance_t==431)
subordinated_loans$Finance_type_verbose <- "Subordinated loans"
dat0 <- rbindlist(
  list(
    standard_grants
    ,standard_loans
    ,common_equity
    ,other_equity
    ,guarantees
    ,subordinated_loans
    )
  )

#Exclude private
dat0 <- subset(dat0,Category %in% c(10,21,22))

dat0$Category_verbose <- NA
dat0$Category_verbose[which(dat0$Category==10)] <- "ODA"
dat0$Category_verbose[which(dat0$Category %in% c(21,22))] <- "OOF"

recipient_codes <- read_csv("recipient_codes.csv")
recipient_codes$RecipientName <- NULL
donor_codes <- read_csv("donor_codes.csv")
donor_codes$DonorName <- NULL

dat0 <- merge(dat0,recipient_codes,by="RecipientCode",all.x=TRUE)
dat0 <- merge(dat0,donor_codes,by="DonorCode",all.x=TRUE)

dat0_tab <- data.table(dat0)[,.(
  usd_disbursement_defl_sum=sum(usd_disbursement_defl,na.rm=TRUE)
  ),by=.(
    Finance_type_verbose
    ,Category_verbose
    ,Year
  )]

# base <- ggplot(dat0_tab_sub,aes(x=Year,y=usd_disbursement_defl_sum,group=interaction(Finance_type_verbose,Category_verbose),colour=interaction(Finance_type_verbose,Category_verbose),fill=interaction(Finance_type_verbose,Category_verbose)))
# log.base <- ggplot(dat0_tab,aes(x=Year,y=log(usd_disbursement_defl_sum),group=interaction(Finance_type_verbose,Category_verbose),colour=interaction(Finance_type_verbose,Category_verbose),fill=interaction(Finance_type_verbose,Category_verbose)))
# base + geom_line()
# log.base + geom_line()
# base + geom_area(position="fill")
