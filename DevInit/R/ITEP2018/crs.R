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

setwd("C:/Users/Alex/Documents/Data/ITEP2018")

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

wb_groups <- read_csv("wb_groups.csv")
wb_groups$country <- NULL
wb_miss <- setdiff(dat0$RecipientISO3,wb_groups$RecipientISO3)
message(paste("WB groups missing",paste(wb_miss,collapse=" ")))

un_groups <- read_csv("un_groups.csv")
un_groups$RecipientName <- NULL
un_miss <- setdiff(dat0$RecipientISO3,un_groups$RecipientISO3)
message(paste("UN groups missing",paste(un_miss,collapse=" ")))

povcal <- read_csv("ext.csv")
povcal$RecipientName <- NULL
povcal_miss <- setdiff(dat0$RecipientISO3,povcal$RecipientISO3)
message(paste("PovCal missing",paste(povcal_miss,collapse=" ")))

inform <- read_csv("inform.csv")
inform$RecipientName <- NULL
inform_miss <- setdiff(dat0$RecipientISO3,inform$RecipientISO3)
message(paste("INFORM missing",paste(inform_miss,collapse=" ")))

gain <- read_csv("gain.csv")
gain$RecipientName <- NULL
gain_miss <- setdiff(dat0$RecipientISO3,gain$RecipientISO3)
message(paste("ND Gain missing",paste(gain_miss,collapse=" ")))

sof <- read_csv("sof.csv")
sof$RecipientName <- NULL
sof_miss <- setdiff(dat0$RecipientISO3,sof$RecipientISO3)
message(paste("States of Fragility missing",paste(sof_miss,collapse=" ")))

meta.list <-list(
  wb_groups
  ,un_groups
  ,povcal
  ,inform
  ,gain
  ,sof
)

meta = Reduce(function(...) merge(..., all=T,by="RecipientISO3"), meta.list)

# write_csv(dat0,"CRS_1996-2016_specific_instruments.csv")
# write_csv(meta,"ITEP_CRS_meta")

#Remove short and long description
dat0$ShortDescription <- NULL
dat0$LongDescription <- NULL

save(meta,dat0,file="financing_instruments.RData")

# dat0_tab <- data.table(dat0)[,.(
#   usd_disbursement_defl_sum=sum(usd_disbursement_defl,na.rm=TRUE)
#   ),by=.(
#     Finance_type_verbose
#     ,Category_verbose
#     ,Year
#   )]

# base <- ggplot(dat0_tab_sub,aes(x=Year,y=usd_disbursement_defl_sum,group=interaction(Finance_type_verbose,Category_verbose),colour=interaction(Finance_type_verbose,Category_verbose),fill=interaction(Finance_type_verbose,Category_verbose)))
# log.base <- ggplot(dat0_tab,aes(x=Year,y=log(usd_disbursement_defl_sum),group=interaction(Finance_type_verbose,Category_verbose),colour=interaction(Finance_type_verbose,Category_verbose),fill=interaction(Finance_type_verbose,Category_verbose)))
# base + geom_line()
# log.base + geom_line()
# base + geom_area(position="fill")
