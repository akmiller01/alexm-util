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

rawJson <- fromJSON(content)
structure <- rawJson$Structure
dataflows <- structure$Dataflows$Dataflow

dataflowNames <- list()
dataflowStructures <- list()
for(dataflow in dataflows){
  dataflowNames[[dataflow$KeyFamilyRef$KeyFamilyID]] = dataflow$Name$`#text`
}


dataflow = dataflows[[1]]
ds_url <- paste0(ds_base_url,dataflow$KeyFamilyRef$KeyFamilyID)
dsContent <- getURL(ds_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
#Clean unescaped quotes by removing HTML tags
dsContent = gsub("\\<[^>]*>", "", dsContent, perl=TRUE)
ds <- fromJSON(dsContent)$Structure
dataflowStructures[[dataflow$KeyFamilyRef$KeyFamilyID]] = ds
codelistIDs <- ds$CodeLists$CodeList
#All null?
# for(codelistID in codelistIDs){
#   cl_url <- paste0(cl_base_url,codelistID$`@id`,"_",dataflow$KeyFamilyRef$KeyFamilyID)
#   clContent <- getURL(cl_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
#   cl <- fromJSON(clContent)$Structure$CodeLists
#   message(cl)
# }

clVars <- c()
for(codelistID in codelistIDs){
  clVars <- c(clVars,codelistID$`@id`)
}

cd_url <- paste0(cd_base_url,dataflow$KeyFamilyRef$KeyFamilyID,"/A")
# http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/{database ID}/{frequency}.{item1 from
#   dimension1}+{item2 from dimension1}+{item N from dimension1}.{item1 from
#     dimension2}+{item2 from dimension2}+{item M from dimension2}?startPeriod={start
#       date}&endPeriod={end date}
cdContent <- getURL(cd_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")

