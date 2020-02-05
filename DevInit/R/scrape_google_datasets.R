list.of.packages <- c("data.table","scrapeR","XML","rvest","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

url = "https://datasetsearch.research.google.com/search?query=uganda&docid=D1Wb%2BVI%2FzRCwkMoGAAAAAA%3D%3D&property=ZmlsZV9mb3JtYXRfY2xhc3M%3D"

source = scrape(url, headers=T,follow=T,parse=T)[[1]]

list_elems = getNodeSet(source,"//li[@class='UnWQ5']")
data_list=list()
data_index=1

for(list_elem in list_elems){
  title = xmlValue(getNodeSet(list_elem,"div/div/div/h1")[[1]])
  updated = trimws(sapply(getNodeSet(list_elem,"div/div/div/span"),xmlValue))
  if(length(updated)==0){
    updated = NA
  }
  urls = sapply(getNodeSet(list_elem,"div/div/div/div/ul/li[@class='iW1HZe']"),xmlValue)
  list.df = data.frame(title,updated,urls)
  data_list[[data_index]] = list.df
  data_index = data_index + 1
}

dat = rbindlist(data_list)
fwrite(dat,"ug_google_datasets.csv")
