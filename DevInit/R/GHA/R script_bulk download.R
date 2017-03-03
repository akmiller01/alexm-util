# install.packages("plyr")

wd <- "D:/Documents/Data/GHA/FTS"
setwd(wd)
#No longer a need to skip 5 rows, header is right at the top with this download
data <- read.csv("2015_Somalia.csv",header=TRUE,na.strings="",as.is=TRUE)
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

#Merge to create new column "Code name" based on donor type
codenames <- read.csv("codename.csv",na.strings="",as.is=TRUE)
codenames <- codenames[!duplicated(codenames$Donor),]
data <- join(data, codenames, by='Donor', type='left', match='all')


withoutCodename <- subset(data,is.na(codename))
unique(withoutCodename$Donor)
#Essentially just the gov'ts without codenames. Is that okay? Do we want to keep them in?


#Merge to create new column "Private money" based on donor type
#I don't have these csvs, but I'll double check the new var names match up
privatemoney <- read.csv("privatemoney.csv",na.strings="",as.is=TRUE)
privatemoney <- privatemoney[!duplicated(privatemoney$Donor),]
data <- join(data, privatemoney, by='Donor', type='left', match='all')

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

# Set up new blank vars
data$USD.contributed <- NA
data$USD.committed <- NA
data$USD.pledged <- NA

# Replace with data given conditionals
data$USD.contributed[which(data$Flow.status=="Paid Contribution")] <- data$Amount..USD.[which(data$Flow.status=="Paid Contribution")]
data$millionsContributed <- data$USD.contributed/1000000
data$USD.committed[which(data$Flow.status=="Commitment")] <- data$Amount..USD.[which(data$Flow.status=="Commitment")]
data$millionsCommitted <- data$USD.committed/1000000
data$USD.pledged[which(data$Flow.status=="Pledge")] <- data$Amount..USD.[which(data$Flow.status=="Pledge")]
data$millionsPledged <- data$USD.pledged/1000000

#Need a new function to do sums by row...
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 

data$USD.committed.contributed <- psum(data$USD.committed,data$USD.contributed,na.rm=TRUE)
data$millionsComCon <- data$USD.committed.contributed/1000000

#Make sure it worked okay
View(data[c("Flow.status","Amount..USD.","millionsContributed","millionsCommitted","millionsPledged","millionsComCon")])

#Create new column "Domestic" 
data <- transform(data,domesticresponse=Donor==Destination.Country)


deflator <- read.csv("deflatorstrial2000.csv",na.strings="",as.is=TRUE)
deflator <- deflator[!duplicated(deflator$Donor),]

data <- join(data,deflator,by="Donor",type='left', match='all')
data <- transform(data,contributedDeflated=USD.committed.contributed/Deflatorvalue)
data <- transform(data,pledgedDeflated=USD.pledged/Deflatorvalue)


withoutdeflators <- subset(data,is.na(Deflatorvalue))
unique(withoutdeflators$Donor)


#Remove Deflatorvalue column
data$Deflatorvalue <- NULL

#No longer have an X
# data$X <- NULL

data <- transform(data,millionsContributeddeflated=contributedDeflated/1000000)
data <- transform(data,millionsPledgeddeflated=pledgedDeflated/1000000)

#Merge to create new column "Income group" based on destination country
incomegroups <- read.csv("incomegroups.csv",na.strings="",as.is=TRUE)
incomegroups <- incomegroups[!duplicated(incomegroups$Destination.Country),]

data <- join(data, incomegroups, by='Destination.Country', type='left', match='all')

withoutincome <- subset(data,is.na(incomegroups))
unique(withoutincome$Destination.Country)

write.csv(data,"fts_transformed.csv",na="",row.names=FALSE)

