list.of.packages <- c("RPostgreSQL","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("/home/alex/git/alexm-util/DevInit/R/ddw")

cred <- readLines("~/ddw.txt")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv
                 ,dbname=cred[5]
                 ,host = cred[2]
                 ,port = as.numeric(cred[3])
                 ,user = cred[1]
                 ,password = cred[4]
)

schemaListTables <- function(schema){
  query <- paste0("SELECT table_name FROM information_schema.tables
                  WHERE table_schema='",schema,"'")
  return(dbGetQuery(con,query)$table_name)
}

ddw <- function(schemaTable){
  split <- strsplit(schemaTable,".",fixed=TRUE)[[1]]
  if(length(split)==2){
    schema <- split[1]
    table <- split[2]
    schemaTables <- schemaListTables(schema)
    if(table %in% schemaTables){
      return(dbReadTable(con,table))
    }else{
      return(data.frame())
    }
  }else{
    allTables <- dbListTables(con)
    if(schemaTable %in% allTables){
      return(dbReadTable(con,schemaTable))
    }else{
      return(data.frame())
    }
  }
}

rm(cred,drv,list.of.packages,new.packages)

wants = c(
  "di_itep_channel"
  ,"oecd_crs_channel_code_5_digit"
  ,"oecd_crs_channel_code_5_digit_to_di_itep_channel_map"
  ,"oecd_crs_channel_code_5_digit_to_itep_channel_web_id_map"
  ,"di_itep_sector"
  ,"oecd_crs_purpose_code_5_digit"
  ,"oecd_crs_purpose_code_5_digit_to_di_itep_sector_map"
  ,"oecd_crs_purpose_code_5_digit_to_itep_sector_web_id_map"
  ,"oecd_crs_sector_code_3_digit"
  ,"oecd_crs_sector_code_3_digit_to_di_itep_sector_map"
  ,"oecd_crs_sector_code_3_digit_to_itep_sector_web_id_map"
)
short.names = substr(gsub("oecd_crs_|_code_|5_digit|3_digit","",wants),1,30)
wb = createWorkbook()
tables = list()
for(i in 1:length(wants)){
  want = wants[i]
  table.name = short.names[i]
  table = ddw(want)
  addWorksheet(wb,table.name)
  writeData(wb,table.name,table)
}
saveWorkbook(wb,"dims.xlsx",overwrite=T)

dbDisconnect(con)