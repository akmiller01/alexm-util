source("~/git/alexm-util/DevInit/R/imf_sdmx_func.R")

cl <- imfCL("GFSR")

#All sectors, indicators, and units for AE
df <- imfDF("GFSR","A.AE","2000","2020",reshape=FALSE)
#Only 2011 to 2016...

db <- imfDB()


#Let's try Middle East EO
cl <- imfCL("MCDREO")

# Total revenue Excl. grants, Gen govt, percent of GDP for AE
df <- imfDF("MCDREO","A.AE.GGRXO_GDPXO_PT","2000","2020",reshape=FALSE)
#2000 to 2018. Not bad

#Total rev for all
df <- imfDF("MCDREO","A..GGRXO_GDPXO_PT","2000","2020")
#Still only 2000 to 2018. Where are they keeping the projections?

library(ggplot2)
df$value <- as.numeric(df$value)
isos <- c("AE","DJ","EG")
ggplot(subset(df,X.REF_AREA %in% isos),aes(x=year,y=value,colour=X.REF_AREA,group=X.REF_AREA)) + geom_line()

