library(jsonlite)

raw <- fromJSON("http://www.d-portal.org/ctrack/q?from=act%2Ctrans%2Ccountry&limit=-1&select=country_code%2Csum_of_percent_of_trans_usd&groupby=country_code&trans_code=D%7CE&trans_day_gteq=2017-01-01&trans_day_lt=2018-01-01&reporting_ref=DE-1&view=countries&_=1508420374887")

data <- raw$rows
