#Help docs:
# http://datahelp.imf.org/knowledgebase/articles/788715-data-services-how-to-get-the-code-of-a-dataset-an
# http://datahelp.imf.org/knowledgebase/articles/667681-using-json-restful-web-service
# http://dataservices.imf.org/REST/SDMX_JSON.svc/help
# http://datahelp.imf.org/knowledgebase/articles/937155-how-to-query-large-datasets-with-data-services


library(RCurl)
library(rjson)

user_agent = "di-imf-rsdmx/0.0.1"

data_flow_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow"
ds_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/"
cl_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CodeList/"
cd_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/"

content <- getURL(data_flow_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
Sys.sleep(1)

rawJson <- fromJSON(content)
structure <- rawJson$Structure
dataflows <- structure$Dataflows$Dataflow

dataflowNames <- list()
dataflowStructures <- list()
for(dataflow in dataflows){
  dataflowNames[[dataflow$KeyFamilyRef$KeyFamilyID]] = dataflow$Name$`#text`
}


dataflow = dataflows[[20]]
ds_url <- paste0(ds_base_url,dataflow$KeyFamilyRef$KeyFamilyID)
dsContent <- getURL(ds_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
Sys.sleep(1)

#Clean unescaped quotes by removing HTML tags
dsContent = gsub("\\<[^>]*>", "", dsContent, perl=TRUE)
ds <- fromJSON(dsContent)$Structure
dataflowStructures[[dataflow$KeyFamilyRef$KeyFamilyID]] = ds
codelistIDs <- ds$CodeLists$CodeList
codelists <- list()
for(codelistID in codelistIDs){
  cl_url <- paste0(cl_base_url,codelistID$`@id`)
  clContent <- getURL(cl_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  Sys.sleep(1)
  cl <- fromJSON(clContent)$Structure$CodeLists$CodeList
  codelists[[codelistID$`@id`]] <- cl
}

cd_url <- paste0(cd_base_url,dataflow$KeyFamilyRef$KeyFamilyID,"/A..S1311B.XDC.W0_S1_G1?startPeriod=2016&endPeriod=2016")
# http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/{database ID}/{frequency}.{item1 from
#   dimension1}+{item2 from dimension1}+{item N from dimension1}.{item1 from
#     dimension2}+{item2 from dimension2}+{item M from dimension2}?startPeriod={start
#       date}&endPeriod={end date}
cdContent <- getURL(cd_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
Sys.sleep(1)
cd <- fromJSON(cdContent)

country.id <- c()
country.revenue <- c()
for(series in cd$CompactData$DataSet$Series){
  if("Obs" %in% names(series)){
    if("@OBS_VALUE" %in% names(series$Obs)){
      country.id <- c(country.id,series$`@REF_AREA`)
      country.revenue <- c(country.revenue,series$Obs$`@OBS_VALUE`)
    }
  }
}
df <- data.frame(country.id,country.revenue)
df$year <- 2016
write.csv(df,"sdmx_revenue_2016.csv",row.names=FALSE)
