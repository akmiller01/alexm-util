list.of.packages <- c("WDI","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

dh_wdi = function(indicator,start=1990,end=2018){
  dat <- WDI(country = "all", 
             indicator = indicator, 
             start = start, 
             end = end,
             extra = TRUE
  )
  setnames(dat,indicator,"value")
  setnames(dat,"iso2c","id")
  keep = c("id","year","value")
  dat = dat[keep]
  entity = read.csv(
    "https://raw.githubusercontent.com/devinit/datahub-cms/20d7ad6fe13e214cf50ecf7c109df84142861048/global/entity.csv"
    ,na.strings=""
    ,as.is=T
  )
  dat = subset(dat, id %in% entity$id)
  return(dat)
}

pop = dh_wdi("SP.POP.TOTL")