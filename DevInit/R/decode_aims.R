list.of.packages <- c("data.table","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/Downloads")

raw = fromJSON("AfghanistanAMP.txt", simplifyVector=F)

columns = raw$crossTabGrouping$values

province_year_list = list()
list_index = 1
for(col in columns){
  year = col$name
  prov_vals = col$values
  for(prov_val in prov_vals){
    province = prov_val$name
    prov_df = data.frame(year,province)
    province_year_list[[list_index]] = prov_df
    list_index = list_index + 1
  }
}

province_years = rbindlist(province_year_list)

rows = raw$rows

afg_list = list()
afg_index = 1
for(row in rows){
  sector = row$itemName
  row_vals = as.numeric(gsub(",","",unlist(row$values)))
  copy_df = copy(province_years)
  copy_df$sector = sector
  copy_df$value = row_vals
  afg_list[[afg_index]] = copy_df
  afg_index = afg_index + 1
}

afg_aims = rbindlist(afg_list)
fwrite(afg_aims,"afg_aims_decoded.csv")
