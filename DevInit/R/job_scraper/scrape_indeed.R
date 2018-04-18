list.of.packages <- c("readr","scrapeR","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(scrapeR)
library(readr)
library(data.table)

setwd("~/job_scraper")

useragent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36"

base_link = "https://www.indeed.com"

data.list = list()
data.index = 1

keywords = c("information","analysis","data")

for(start in seq(0,100,by=10)){
  message(start)
  posting_url = paste0("https://www.indeed.com/jobs?q=&l=Washington%2FMetro%2C+DC&start=",start)
  job_posting_source = scrape(posting_url,userAgent=useragent , headers=T,follow=T,parse=T)[[1]]
  job_link_elems = getNodeSet(job_posting_source,"//a[@data-tn-element='jobTitle']")
  job_links = sapply(job_link_elems,xmlGetAttr,"href")
  
  for(job_link in job_links){
    job_source = scrape(paste0(base_link,job_link),userAgent=useragent,headers=T,follow=T,parse=T)[[1]]
    job_title = xmlValue(getNodeSet(job_source,"//b[@class='jobtitle']/font")[[1]])
    employer = xmlValue(getNodeSet(job_source,"//span[@class='company']")[[1]])
    location = xmlValue(getNodeSet(job_source,"//span[@class='location']")[[1]])
    summary = xmlValue(getNodeSet(job_source,"//span[@id='job_summary']")[[1]])
    summary = gsub("\n"," ",summary,fixed=T)
    job_df = data.frame(job_title,employer,location,summary)
    for(keyword in keywords){
      var_name = paste0("contains_",keyword)
      job_df[,var_name] = grepl(keyword, summary)
    }
    data.list[[data.index]] = job_df
    data.index = data.index + 1
  }

}

all_jobs = rbindlist(data.list)
write_csv(all_jobs,"all_jobs_indeed.csv")