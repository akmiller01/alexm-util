list.of.packages <- c("WDI","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
source("~/git/alexm-util/DevInit/R/ddw/connect.R")

wdi_id_map = ddw("dimension.wb_wdi_country_to_di_id_map")
deflator = ddw("dimension.imf_weo_usd_deflator_2014_2016_apr_pivoted")
setnames(deflator,"weo_country_code","imf_weo_country_code")
imf_wdi_map = ddw("dimension.wb_wdi_country_to_imf_weo_country_map")
deflator = merge(deflator,imf_wdi_map,by="imf_weo_country_code",all=T)
deflator = merge(deflator,wdi_id_map,by="wb_wdi_country_code",all=T)
keep = c("di_id","year","deflator")
deflator=deflator[keep]

convert_wb_wdi_series_simple = function(df,base_year,precision){
  base_year_deflator = subset(deflator,year==base_year)
  setnames(base_year_deflator,"deflator","base_year_deflator")
  
  df = merge(df,deflator,by=c("di_id","year"))
  df = merge(df,deflator,byc("di_id"))
  df$conversion_factor = df$base_year_deflator/df$deflator
  df$value = round(df$value,precision) * df$conversion_factor
  return(df)
}

dh_wdi = function(indicator,start=1960,end=2018){
  dat <- WDI(country = "all", 
             indicator = indicator, 
             start = start, 
             end = end,
             extra = TRUE
  )
  setnames(dat,"iso3c","wb_wdi_country_code")
  setnames(dat,indicator,"value")
  dat = merge(dat,wdi_id_map,by="wb_wdi_country_code")
  keep = c("di_id","year","value")
  dat = dat[keep]
  dat = subset(dat, !is.na(di_id))
  return(dat)
}

dh_combine = function(frame_list){
  var_names = names(frame_list)
  dat = Reduce(function(...) merge(..., all=T,by=c("id","year")), frame_list)
  names(dat)[3:length(dat)] = var_names
  return(dat)
}

pop_total = dh_wdi("SP.POP.TOTL")
pop_male = dh_wdi("SP.POP.TOTL.MA.IN")
pop_total_and_male = dh_combine(
  list(
    "pop_total"=pop_total,
    "pop_male"=pop_male
    )
)

gdp_pc_usd_current = dh_wdi("NY.GDP.PCAP.CD")

dbDisconnect(con)
