# Load packages
library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)

api <- 'http://api.worldbank.org/v2/' 

url_list_lic<- 'http://api.worldbank.org/v2/incomeLevels/LIC/countries'
# list_lic<-GET(url_list_lic)
# list_lic

# list_lic<-GET(url= paste(url_list_lic, '?format=json'))
# list_lic

mrv_url<-'http://api.worldbank.org/v2/countries/chn;bra/indicators/DPANUSIFS?MRV=5'

mrnev_url<-'http://api.worldbank.org/v2/countries/BR;CR/indicators/SH.STA.MMRT.NE?MRNEV=1'

gapfill_url_mrv<-'http://api.worldbank.org/v2/countries/chn;bra/indicators/DPANUSIFS?MRV=5&Gapfill=Y'

freq<-'http://api.worldbank.org/v2/en/countries/ind;chn/indicators/DPANUSSPF?date=2000:2006&MRV=5&frequency=Q'

# get_freq<-GET(freq)
# get_freq

# get_json_freq<-GET(url= paste(freq,'?format=json'))
# get_json_freq

list_indicators<-'http://api.worldbank.org/v2/indicators?format=json'
# list_indicators_json<-GET(list_indicators)
# list_indicators_content<- content(list_indicators_json)
# head(list_indicators_content[[1]])



# indicators_list <- fromJSON(list_indicators)
# indicators_list



# indicators_list[[2]]$id



# is.list(indicators_list)
# is.data.frame(indicators_list)



# indicators_list[[1]]



# indicators_list[[2]]



# df stands for data frame
# indicators_df<- indicators_list[[2]]



#Let's try if this worked?
# is.data.frame(indicators_df)



# names(indicators_df)



# indicators_df[,c("id","name")]



# indicators_list <- lapply(indicators_list, function(x) {
  # x[sapply(x, is.null)] <- NA
  # unlist(x)
# })




# library(plotly)



# a=rnorm(50)
# b=rnorm(50)



# help(rnorm)




# p<-plot_ly(x=a, y=b, type='scatter', mode='markers', size=a)
# p




brazil_url<-'http://api.worldbank.org/v2/countries/br/indicators/1.2.PGap.Poor4uds?format=json'

colombia_url<-'http://api.worldbank.org/v2/countries/co/indicators/1.2.PGap.Poor4uds?format=json'

# poverty_gap_content_br<-fromJSON(brazil_url)
# poverty_gap_content_co<-fromJSON(colombia_url)

##for Colombia the summary page would be
# poverty_gap_content_co[[1]]




##for Brazil the summary page would be
# poverty_gap_content_br[[1]]
# 
# 
# brazil_poverty_gap<-poverty_gap_content_br[[2]]
# colombia_poverty_gap<-poverty_gap_content_co[[2]]
# 
# #What we are interested in here is date and value
# colombia_poverty_gap_date_value<-colombia_poverty_gap[,c("date","value")]
# 
# brazil_poverty_gap_date_value<-brazil_poverty_gap[,c("date","value")]
# 
# 
# 
# 
# colombia_poverty_gap_date_value <- lapply(colombia_poverty_gap_date_value, function(x) {
#   x[sapply(x, is.null)] <- NA
#   unlist(x)
# })
# 
# brazil_poverty_gap_date_value <- lapply(brazil_poverty_gap_date_value, function(x) {
#   x[sapply(x, is.null)] <- NA
#   unlist(x)
# })
# 
# 
# 
# 
# brazil<-plot_ly(y=brazil_poverty_gap_date_value$value, x=brazil_poverty_gap_date_value$date, type='scatter', mode='lines')
# brazil
# 
# 
# 
# colombia<-plot_ly(y=colombia_poverty_gap_date_value$value, x=colombia_poverty_gap_date_value$date, type='scatter', mode='lines')
# colombia
# 
# 
# 
# combined<-plot_ly(y=colombia_poverty_gap_date_value$value, x=colombia_poverty_gap_date_value$date, name='Colombia', type='scatter', mode='lines') %>%
#   add_trace(y=brazil_poverty_gap_date_value$value, name="Brazil", mode='lines')
# combined

list.urls <- function(){
  message(url_list_lic)
  message(paste(url_list_lic, '?format=json'))
  message(freq)
  message(paste(freq,'?format=json'))
  message(list_indicators)
  message(brazil_url)
  message(colombia_url)
}
list.urls()

