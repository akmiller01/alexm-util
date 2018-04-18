list.of.packages <- c("readr","lttr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(httr)
library(readr)
library(data.table)

setwd("~/job_scraper")

payload = "{\"params\":\"aroundLatLng=51.507351, -0.127758&aroundPrecision=20000&aroundRadius=40000&attributesToRetrieve=type,name,description,city,stateStr,state,country,published,url,orgID,orgUrl,orgName,groupID,groupUrl,groupName,orgType,objectID,_snippetResult&attributesToSnippet=description:20&facets=*&filters=type:'JOB'&getRankingInfo=true&hitsPerPage=100&page=0&query=\"}"

url = "https://nsv3auess7-dsn.algolia.net/1/indexes/idealist7-production/query?x-algolia-application-id=NSV3AUESS7&x-algolia-api-key=c2730ea10ab82787f2f3cc961e8c1e06"

response = POST(url,body=payload,encode="json")
idealist_content = content(response)


data.list = list()
data.index = 1

keywords = c("information","analysis","data")
for(hit in idealist_content$hits){
  job_title = hit$name
  employer = hit$orgName
  employer_type = hit$orgType
  if(is.null(employer_type)){employer_type=NA}
  location = hit$city
  summary = hit$description
  job_df = data.frame(job_title,employer,employer_type,location,summary)
  for(keyword in keywords){
    var_name = paste0("contains_",keyword)
    job_df[,var_name] = grepl(keyword, summary)
  }
  data.list[[data.index]] = job_df
  data.index = data.index + 1
}

all_jobs = rbindlist(data.list)
write_csv(all_jobs,"all_jobs_idealist.csv")
