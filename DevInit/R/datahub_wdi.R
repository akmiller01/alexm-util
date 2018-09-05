list.of.packages <- c("WDI","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/ddw/connect.R")

# Pull in id mappings and deflators. Performing merges to append di_id to the deflator
wdi_id_map = ddw("dimension.wb_wdi_country_to_di_id_map")
deflator = ddw("dimension.imf_weo_usd_deflator_2014_2016_apr_pivoted")
setnames(deflator,"weo_country_code","imf_weo_country_code")
imf_wdi_map = ddw("dimension.wb_wdi_country_to_imf_weo_country_map")

# Function to convert from constant to current USD
convert_wb_wdi_series_simple = function(df,base_year,precision){
  df = merge(df,imf_wdi_map,by="wb_wdi_country_code",all.x=T)
  base_year_deflator = subset(deflator,year==base_year)
  setnames(base_year_deflator,"deflator","base_year_deflator")
  setnames(base_year_deflator,"year","base_year")
  
  df = merge(df,deflator,by=c("imf_weo_country_code","year"),all.x=T)
  df = merge(df,base_year_deflator,by=c("imf_weo_country_code"),all.x=T)
  df$conversion_factor = df$base_year_deflator/df$deflator
  df$value = round(df$value,precision) * df$conversion_factor
  keep = c("di_id","year","value")
  df = df[keep]
  return(df)
}

# Function to pull from WDI API
dh_wdi = function(indicator,start=1960,end=2018){
  dat <- WDI(country = "all", 
             indicator = indicator, 
             start = start, 
             end = end,
             extra = TRUE
  )
  setnames(dat,"iso3c","wb_wdi_country_code")
  setnames(dat,indicator,"value")
  dat = merge(dat,wdi_id_map,by="wb_wdi_country_code",all.x=T)
  keep = c("di_id","wb_wdi_country_code","year","value")
  dat = dat[keep]
  dat = subset(dat, !is.na(di_id))
  return(dat)
}

# Function to combine WDI pulled dataframes
dh_combine = function(frame_list){
  var_names = names(frame_list)
  dat = Reduce(function(...) merge(..., all=T,by=c("di_id","year")), frame_list)
  names(dat)[3:length(dat)] = var_names
  return(dat)
}

# Example usage
pop_total = dh_wdi("SP.POP.TOTL")
pop_total$wb_wdi_country_code = NULL
pop_male = dh_wdi("SP.POP.TOTL.MA.IN")
pop_male$wb_wdi_country_code = NULL
pop_total_and_male = dh_combine(
  list(
    "pop_total"=pop_total,
    "pop_male"=pop_male
    )
)
gdp_pc_usd_current = dh_wdi("NY.GDP.PCAP.CD")
gdp_pc_usd_2015 = convert_wb_wdi_series_simple(gdp_pc_usd_current,2015,2)

dbDisconnect(con)
