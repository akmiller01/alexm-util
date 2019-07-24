list.of.packages <- c("data.table","jsonlite","tcltk","httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/git/IATI")

total_count = fromJSON("http://datastore.iatistandard.org/api/1/access/activity?limit=0")["total-count"][[1]]

base_url = "http://datastore.iatistandard.org/api/1/access/activity.csv?limit="

offset = 0
step = 1000
data_list = list()
data_index = 1
pb = tkProgressBar(max=total_count)
while(offset < total_count){
  iati_url = paste0(base_url,format(step,scientific=F),"&offset=",format(offset,scientific=F))
  csv_dat = GET(iati_url, timeout(600))
  tmp = fread()
  data_list[[data_index]] = tmp
  offset = offset + step
  data_index = data_index + 1
  setTkProgressBar(pb, offset)
}
close(pb)
iati = rbindlist(data_list)
save(iati,file="iati.RData")