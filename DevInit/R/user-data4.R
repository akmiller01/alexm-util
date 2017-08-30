library(openxlsx)
library(reshape)
library(utils)
source("C:/git/alexm-util/DevInit/R/ddw/connect.R")

wd <- "C:/git/digital-platform/user-data/"
setwd(wd)
refPath <- "https://raw.githubusercontent.com/devinit/digital-platform/master/reference/"
refMap <- list("data_series.domestic"="di_budget_type,di_domestic_budget_level,di_currency")
refMap <- c(refMap,"data_series.domestic-sectors"="di_budget_type,di_domestic_budget_level,di_currency")
refMap <- c(refMap,"data_series.domestic-netlending"="di_budget_type,di_domestic_budget_level,di_currency")
refMap <- c(refMap,"data_series.intl-flows-donors"="di_flow_type,di_flow_name")
refMap <- c(refMap,"data_series.intl-flows-recipients"="di_flow_type,di_flow_name")
refMap <- c(refMap,"data_series.intl-flows-donors-wide"="di_flow_type,di_flow_name")
refMap <- c(refMap,"data_series.intl-flows-recipients-wide"="di_flow_type,di_flow_name")
refMap <- c(refMap,"data_series.largest-intl-flow"="di_largest_intl_flow")
refMap <- c(refMap,"data_series.fragile-states"="di_fragile_state")
refMap <- c(refMap,"data_series.long-term-debt"="di_destination_institution_type,di_financing_type")
refMap <- c(refMap,"data_series.oda"="di_sector,di_oof_bundle,di_channel")
refMap <- c(refMap,"data_series.oof"="di_sector,di_oof_bundle,di_channel")
refMap <- c(refMap,"data_series.fdi-out"="di_financing_type")
refMap <- c(refMap,"data_series.dfis-out-dev"="di_financing_type")
refMap <- c(refMap,"data_series.ssc-out"="di_financing_type")

#Delete everything in user-data
unlink(dir(wd, full.names = TRUE),recursive=TRUE)

all.entities <- ddw("reference.di_entity")

userDat <- function(data,basename){
  #Read Data
  names <- colnames(data)
  fwd = paste0(wd,basename)
  
  #Add country names
  entities <- all.entities[c("id","name")]
  names(entities) <- c("di_id","entity_name")
  if("di_id" %in% names){
    data <- merge(
      entities
      ,data
      ,by=c("di_id")
      ,all.y=TRUE
    ) 
  }else{
    if("to_di_id" %in% names){
      names(entities) <- c("to_di_id","entity_to_name")
      data <- merge(
        entities
        ,data
        ,by=c("to_di_id")
        ,all.y=TRUE
      ) 
    }
    if("from_di_id" %in% names){
      names(entities) <- c("from_di_id","entity_from_name")
      data <- merge(
        entities
        ,data
        ,by=c("from_di_id")
        ,all.y=TRUE
      ) 
    }
  }
  
  #Try and sort by entity name, failing that: id, failing that: year, failing that, the first column.
  names <- colnames(data)
  if("entity_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_name"],data$year),]
    }else{
      data <- data[order(data["entity_name"]),]
    }
  }else if("entity_to_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_to_name"],data$year),]
    }else{
      data <- data[order(data["entity_to_name"]),]
    }
  }else if("entity_from_name" %in% names){
    if("year" %in% names){
      data <- data[order(data["entity_from_name"],data$year),]
    }else{
      data <- data[order(data["entity_from_name"]),]
    }
  }else if("di_id" %in% names){
    if("year" %in% names){
      data <- data[order(data["di_id"],data$year),]
    }else{
      data <- data[order(data["di_id"]),]
    }
  }else{
    if("year" %in% names){
      data <- data[data$year,]
    }else{
      data <- data[order(data[,1]),]
    }
  }
  
  #Create a folder for each indicator with sub-csv dir
  dir.create(fwd)
  setwd(fwd)
  cwd = paste(fwd,"csv",sep="/")
  dir.create(cwd)
  
  #Create workbook
  wb <- createWorkbook(basename)
  
  #Start notes sheet/csv
  concept = concepts[which(concepts$id==basename),]
  notesList <- c(
    paste("Name:",basename)
    ,paste("Description:",concept$description)
    ,paste("Units of measure:",concept$uom)
    ,paste("Source:",concept[,"source"])
    ,if(!is.na(concept[,"source_link"])) c(paste("Source-link:",concept[,"source_link"]),"") else ""
    ,"Notes:"
  )
  if("estimate" %in% names){
    notesList<-c(
      notesList
      ,"This data contains information that may be a projection. Projected datapoints are indicated by a value of TRUE in the 'estimate' column. The year at which projections begin varies from country to country."
      ,""
    )
  }
  if("value_ncu" %in% names){
    notesList<-c(
      notesList
      ,"This data contains information that has been converted from current native currency units (NCU) to constant US Dollars. The NCU values are contained in the 'value-ncu' column, while the converted and deflated values are contained in the 'value' column."
      ,""
    )
  }
  addWorksheet(wb,"Notes")
  
  #Copy the data
  write.csv(data,paste0(cwd,"/",basename,".csv"),row.names=FALSE,na="")
  addWorksheet(wb,"Data")
  writeData(wb,sheet="Data",data,colNames=TRUE,rowNames=FALSE)    
  
  #If we have an ID, a year to widen it by and it's simple, provide wide
  if("di_id" %in% names & "year" %in% names & "value" %in% names)  {
    if("entity_name" %in% names){
      wdata <- reshape(data[c("di_id","entity_name","year","value")],idvar=c("di_id","entity_name"),timevar="year",direction="wide")
    }else{
      wdata <- reshape(data[c("di_id","year","value")],idvar=c("di_id"),timevar="year",direction="wide")
    }
    wnames <- names(wdata)
    for(j in 1:length(wnames)){
      wname = wnames[j]
      if(substr(wname,1,5)=="value"){
        names(wdata)[names(wdata) == wname] <- substr(wname,7,nchar(wname))
      }
    }
    notesList<-c(
      notesList
      ,"On the 'Data-wide-value' sheet, we have provided the indicator in a wide format. The values you see listed there are from the 'value' column."
      ,""
    )
    addWorksheet(wb,"Data-wide-value")
    writeData(wb,sheet="Data-wide-value",wdata,colNames=TRUE,rowNames=FALSE)  
    write.csv(wdata,paste(cwd,"/",basename,"-wide-value",".csv",sep=""),row.names=FALSE,na="")
  }
  #Wide for original-value
  if("di_id" %in% names & "year" %in% names & "original_value" %in% names)  {
    if("entity-name" %in% names){
      wdata <- reshape(data[c("di_id","entity_name","year","original_value")],idvar=c("di_id","entity_name"),timevar="year",direction="wide")
    }else{
      wdata <- reshape(data[c("di_id","year","original_value")],idvar=c("di_id"),timevar="year",direction="wide")
    }
    wnames <- names(wdata)
    for(j in 1:length(wnames)){
      wname = wnames[j]
      if(substr(wname,1,14)=="original_value"){
        names(wdata)[names(wdata) == wname] <- substr(wname,16,nchar(wname))
      }
    }
    notesList<-c(
      notesList
      ,"On the 'Data-wide-original-value' sheet, we have provided the indicator in a wide format. The values you see listed there are from the 'original-value' column."
      ,""
    )
    addWorksheet(wb,"Data-wide-original-value")
    writeData(wb,sheet="Data-wide-original-value",wdata,colNames=TRUE,rowNames=FALSE)  
    write.csv(wdata,paste(cwd,"/",basename,"-wide-original-value",".csv",sep=""),row.names=FALSE,na="")
  }
  
  #Reference
  #Copy entity.csv
  write.csv(all.entities,paste(cwd,"entity.csv",sep="/"),row.names=FALSE,na='')
  if(basename %in% names(refMap)){
    refNames = strsplit(refMap[[basename]],",")[[1]]
    notesList<-c(
      notesList
      ,"The following tabs have been included for reference purposes:"
      ,paste(refNames,collapse=", ")
      ,""
    )
    for(j in 1:length(refNames)){
      refBaseName = refNames[j]
      #Copy the reference files
      refData <- ddw(paste0("reference.",refBaseName))
      if(nrow(refData)>0){
        write.csv(refData,paste0(cwd,"/",refBaseName,".csv"),row.names=FALSE,na="")
        addWorksheet(wb,refBaseName)
        writeData(wb,sheet=refBaseName,refData,colNames=TRUE,rowNames=FALSE) 
      }
    }
  }
  
  #Cap off notes sheet
  notesList<-c(
    notesList
    ,""
    ,""
    ,"The following is data downloaded from Development Initiative's Datahub: http://devinit.org/data"
    ,"It is licensed under a Creative Commons Attribution 4.0 International license."
    ,"More information on licensing is available here: https://creativecommons.org/licenses/by/4.0/"
    ,"For concerns, questions, or corrections: please email info@devinit.org"
    ,"Copyright Development Initiatives Poverty Research Ltd. 2016"
  )
  notesDf <- data.frame(notesList)
  writeData(wb,sheet="Notes",notesDf,colNames=FALSE,rowNames=FALSE)  
  write.table(notesDf,paste0(cwd,"/",basename,"-notes",".csv"),col.names=FALSE,row.names=FALSE,na="",sep=",")
  saveWorkbook(wb, paste0(basename,".xlsx"), overwrite = TRUE)
  
  #Go back to user-data folder
  setwd(wd)
}

concepts <- read.csv("https://raw.githubusercontent.com/devinit/datahub-cms/master/country-profile/concept.csv")

for(id in unique(concepts$id)){
  message(id)
  dat <- ddw(id)
  if(nrow(dat)>0){
    userDat(dat,id)
  }
}

concepts <- read.csv("https://raw.githubusercontent.com/devinit/datahub-cms/master/global-picture/concept.csv")

for(id in unique(concepts$id)){
  message(id)
  dat <- ddw(id)
  if(nrow(dat)>0){
    userDat(dat,id)
  }
}
