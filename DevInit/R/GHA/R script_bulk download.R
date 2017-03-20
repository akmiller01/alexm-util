# install.packages("plyr")
# install.packages("readxl")
library(readxl)

wd <- "D:/Documents/Data/GHA/FTS"
setwd(wd)

#Define renamed vars
csv_names <- c(
  "Flow ID"
  ,"Flow status"
  ,"Flow date"
  ,"Description"
  ,"Amount (USD)"
  ,"Original amount"
  ,"Original currency"
  ,"Exchange rate"
  ,"Flow type"
  ,"Contribution type"
  ,"Budget year"
  ,"Decision date"
  ,"Version ID"
  ,"Created"
  ,"Last updated"
  ,"Modality"
  ,"Donor project code"
  ,"Reporting organization"
  ,"Donor"
  ,"Source Organization type"
  ,"Source Emergency"
  ,"Source Location"
  ,"Source Project"
  ,"Source Usage year"
  ,"Source Plan"
  ,"Source Cluster"
  ,"Source Sector"
  ,"Recipient Organization"
  ,"Destination Organization type"
  ,"Destination Emergency"
  ,"Destination Country"
  ,"Destination Project"
  ,"Destination Usage year"
  ,"Destination Plan"
  ,"Destination Cluster"
  ,"Destination Sector"
)

#Format them as if R did it automatically
csv_names <- make.names(csv_names)

data <- read_excel("Somalia 2015_full download.xls",sheet="Results - Incoming",skip=3,col_names=csv_names)

#No longer a need to skip 5 rows, header is right at the top with this download
# data <- read.csv("2015_Somalia.csv",header=TRUE,na.strings="",as.is=TRUE)
library(plyr)

#Parse numbers (removing commas)
#Commas aren't in numbers anymore, so these aren't necessary
# data <- transform(data,USD.committed.contributed=as.numeric(gsub(",","", USD.committed.contributed)))
# data <- transform(data,Original.currency.amount=as.numeric(gsub(",","", Original.currency.amount)))
# data <- transform(data,USD.pledged=as.numeric(gsub(",","", USD.pledged)))
# data <- transform(data,Project.current.request=as.numeric(gsub(",","", Project.current.request)))
# data <- transform(data,Item.ID=as.numeric(gsub(",","", Item.ID)))

#Remove total row
#Note sure "Total: " still applies, I'll remove the comma and make it case insensitive to pick it up in case
data <- subset(data,!grepl("total",Donor,ignore.case=TRUE))

#Remove government
data$Donor <- gsub(", Government of","",data$Donor)

#Merge to create new column "Code name" based on donor type
codenames <- read.csv("codename.csv",na.strings="",as.is=TRUE)
codenames <- codenames[!duplicated(codenames$Donor),]
codenames$lower.Donor <- tolower(codenames$Donor)
codenames$Donor <- NULL
data$lower.Donor <- tolower(data$Donor)
data <- join(data, codenames, by='lower.Donor', type='left', match='all')


withoutCodename <- subset(data,is.na(codename))
unique(withoutCodename$Donor)
#Essentially just the gov'ts without codenames. Is that okay? Do we want to keep them in?


#Merge to create new column "Private money" based on donor type
#I don't have these csvs, but I'll double check the new var names match up
privatemoney <- read.csv("privatemoney.csv",na.strings="",as.is=TRUE)
privatemoney <- privatemoney[!duplicated(privatemoney$Donor),]
privatemoney$lower.Donor <- tolower(privatemoney$Donor)
privatemoney$Donor <- NULL
data <- join(data, privatemoney, by='lower.Donor', type='left', match='all')

withoutPrivate <- subset(data,is.na(privatemoney))
unique(withoutPrivate$Donor)


#Merge to create new column "Donor DAC region" based on donor type
donordacregion <- read.csv("dacregions.csv",na.strings="",as.is=TRUE)
donordacregion <- donordacregion[!duplicated(donordacregion$Donor),]
data <- join(data, donordacregion, by='Donor', type='left', match='all')

withoutDACRegion <- subset(data,is.na(donordacregion))
unique(withoutDACRegion$Donor)


#Merge to create new column "Appealing agency code name" based on recipient type
recipientcodename <- read.csv("recipientcodename.csv",na.strings="",as.is=TRUE)
recipientcodename <- recipientcodename[!duplicated(recipientcodename$Recipient.Organization),]
data <- join(data, recipientcodename, by='Recipient.Organization', type='left', match='all')

withoutRecipientcode <- subset(data,is.na(recipientcodename))
unique(withoutRecipientcode$Recipient.Organization)


#Merge to create new column "Recip Org NGO type" based on recipient type
ngotype <- read.csv("ngotype.csv",na.strings="",as.is=TRUE)
ngotype <- ngotype[!duplicated(ngotype$Recipient.Organization),]
data <- join(data, ngotype, by='Recipient.Organization', type='left', match='all')

withoutngos <- subset(data,is.na(ngotype))
unique(withoutngos$Recipient.Organization)

#Merge to create new column "Channels of delivery" based on recipient type
deliverychannels <- read.csv("deliverychannels.csv",na.strings="",as.is=TRUE)
deliverychannels <- deliverychannels[!duplicated(deliverychannels$Recipient.Organization),]
data <- join(data, deliverychannels, by='Recipient.Organization', type='left', match='all')

withoutchannels <- subset(data,is.na(deliverychannels))
unique(withoutchannels$Recipient.Organization)

#These are now in the same column. And it's just "Amount..USD." so we'll need to adjust
# data <- transform(data,millionsContributed=USD.committed.contributed/1000000)
# data <- transform(data,millionsPledged=USD.pledged/1000000)

#Create new column "Domestic" 
data <- transform(data,domesticresponse=Donor==Destination.Country)


deflator <- read.csv("deflatorstrial2015.csv",na.strings="",as.is=TRUE)
deflator <- deflator[!duplicated(deflator$Donor),]

data <- join(data,deflator,by="Donor",type='left', match='all')
data <- transform(data,amountDeflated=Amount..USD./Deflatorvalue)
data <- transform(data,amountDeflatedMillions=amountDeflated/1000000)

withoutdeflators <- subset(data,is.na(Deflatorvalue))
unique(withoutdeflators$Donor)

#Merge to create new column "Income group" based on destination country
incomegroups <- read.csv("incomegroups.csv",na.strings="",as.is=TRUE)
incomegroups <- incomegroups[!duplicated(incomegroups$Destination.Country),]

data <- join(data, incomegroups, by='Destination.Country', type='left', match='all')

withoutincome <- subset(data,is.na(incomegroups))
unique(withoutincome$Destination.Country)

write.csv(data,"fts_transformed.csv",na="",row.names=FALSE)

library(data.table)
donor.tab <- data.table(data)[,.(amountDeflatedMillions=sum(amountDeflatedMillions,na.rm=TRUE)),by=.(Donor,Flow.status)]
write.csv(donor.tab,"donor_flow_sums.csv",na="",row.names=FALSE)
donor.country.tab <- data.table(data)[,.(amountDeflatedMillions=sum(amountDeflatedMillions,na.rm=TRUE)),by=.(Donor,Flow.status,Destination.Country)]

library(ggplot2)
plot.dat <- subset(donor.tab,Flow.status=="Paid Contribution")
plot.dat <- plot.dat[order(-plot.dat$amountDeflatedMillions),]
ggplot(plot.dat,aes(x=Donor,y=amountDeflatedMillions)) + geom_bar(stat = "identity")
