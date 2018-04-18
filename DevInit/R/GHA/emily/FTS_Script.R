# install.packages("plyr")
# install.packages("readxl")
# install.packages("data.table")
library(readxl)

lowerConv <- function(x){
  return(iconv(x,"WINDOWS-1252","UTF-8"))
}

wd <- "C:/git/alexm-util/DevInit/R/GHA/emily"
setwd(wd)

data <- read_excel("FTS 2016.xlsx",sheet="Results - Incoming")
names(data) <- make.names(names(data))

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

# Remove government of
data$Donor <- gsub(", Government of","",data$Donor)
unique(data$Donor)
data$Recipient.Organization <- gsub(", Government of","",data$Recipient.Organization)
unique(data$Recipient.Organization)

#Merge to create new column "Code name" based on donor type
codenames <- read.csv("codename.csv",na.strings="",as.is=TRUE)
codenames <- codenames[!duplicated(codenames$Donor),]
codenames$lower.Donor <- lowerConv(codenames$Donor)
codenames$Donor <- NULL
data$lower.Donor <- lowerConv(data$Donor)
data <- join(data, codenames, by='lower.Donor', type='left', match='all')

withoutCodename <- subset(data,is.na(codename))
unique(withoutCodename$Donor)
#Essentially just the gov'ts without codenames. Is that okay? Do we want to keep them in?


#Merge to create new column "Private money" based on donor type
#I don't have these csvs, but I'll double check the new var names match up
privatemoney <- read.csv("privatemoney.csv",na.strings="",as.is=TRUE)
privatemoney <- privatemoney[!duplicated(privatemoney$Donor),]
privatemoney$lower.Donor <- lowerConv(privatemoney$Donor)
privatemoney$Donor <- NULL
data <- join(data, privatemoney, by='lower.Donor', type='left', match='all')

withoutPrivate <- subset(data,is.na(privatemoney))
unique(withoutPrivate$Donor)


#Merge to create new column "Donor DAC region" based on donor type
donordacregion <- read.csv("dacregions.csv",na.strings="",as.is=TRUE)
donordacregion <- donordacregion[!duplicated(donordacregion$Donor),]
donordacregion$lower.Donor <- lowerConv(donordacregion$Donor)
donordacregion$Donor <- NULL
data <- join(data, donordacregion, by='lower.Donor', type='left', match='all')

withoutDACRegion <- subset(data,is.na(donordacregion))
unique(withoutDACRegion$Donor)


#Merge to create new column "Appealing agency code name" based on recipient type
recipientcodename <- read.csv("recipientcodename1.csv",na.strings="",as.is=TRUE)
recipientcodename <- recipientcodename[!duplicated(recipientcodename$Recipient.Organization),]
recipientcodename$lower.Recipient.Organization <- lowerConv(recipientcodename$Recipient.Organization)
recipientcodename$Recipient.Organization <- NULL 
data$lower.Recipient.Organization <- lowerConv(data$Recipient.Organization)
data <- join(data, recipientcodename, by='lower.Recipient.Organization', type='left', match='all')

withoutRecipientcode <- subset(data,is.na(recipientcodename))
unique(withoutRecipientcode$Recipient.Organization)


#Merge to create new column "Recip Org NGO type" based on recipient type
ngotype <- read.csv("ngotype.csv",na.strings="",as.is=TRUE)
ngotype <- ngotype[!duplicated(ngotype$Recipient.Organization),]
ngotype$lower.Recipient.Organization <- lowerConv(ngotype$Recipient.Organization)
ngotype$Recipient.Organization <- NULL
data <- join(data, ngotype, by='lower.Recipient.Organization', type='left', match='all')

withoutngos <- subset(data,is.na(ngotype))
unique(withoutngos$Recipient.Organization)

#Merge to create new column "Channels of delivery" based on recipient type
deliverychannels <- read.csv("deliverychannels.csv",na.strings="",as.is=TRUE)
deliverychannels <- deliverychannels[!duplicated(deliverychannels$Recipient.Organization),]
deliverychannels$lower.Recipient.Organization <- lowerConv(deliverychannels$Recipient.Organization)
deliverychannels$Recipient.Organization <- NULL
data <- join(data, deliverychannels, by='lower.Recipient.Organization', type='left', match='all')

withoutchannels <- subset(data,is.na(deliverychannels))
unique(withoutchannels$Recipient.Organization)

#These are now in the same column. And it's just "Amount..USD." so we'll need to adjust
# data <- transform(data,millionsContributed=USD.committed.contributed/1000000)
# data <- transform(data,millionsPledged=USD.pledged/1000000)

#Merge to create new column "Income group" based on destination country
incomegroups <- read.csv("incomegroups.csv",na.strings="",as.is=TRUE)
incomegroups <- incomegroups[!duplicated(incomegroups$Destination.Country),]
incomegroups$lower.Destination.Country <- lowerConv(incomegroups$Destination.Country)
incomegroups$Destination.Country <- NULL 
data$lower.Destination.Country <- lowerConv(data$Destination.Country)
data <- join(data, incomegroups, by='lower.Destination.Country', type='left', match='all')

withoutincome <- subset(data,is.na(incomegroups))
unique(withoutincome$Destination.Country)

#Create new column "Domestic" 
data <- transform(data,domesticresponse=Donor==Destination.Country)


deflator <- read.csv("2016def.csv",na.strings="",as.is=TRUE)
deflator <- deflator[!duplicated(deflator$Donor),]

data <- join(data,deflator,by="Donor",type='left', match='all')
data <- transform(data,amountDeflated=as.numeric(Amount..USD.)/Deflatorvalue)
data <- transform(data,amountDeflatedMillions=amountDeflated/1000000)

withoutdeflators <- subset(data,is.na(Deflatorvalue))
unique(withoutdeflators$Donor)

#Remove Deflatorvalue column
data$Deflatorvalue <- NULL

#No longer have an X
# data$X <- NULL

write.csv(data,"fts_transformed.csv",na="",row.names=FALSE)

# install.packages("data.table")
library(data.table)
donor.tab <- data.table(data)[,.(amountDeflatedMillions=sum(amountDeflatedMillions,na.rm=TRUE)),by=.(Donor,Flow.status)]
write.csv(donor.tab,"donor_flow_status.csv", na="",row.names=FALSE)

