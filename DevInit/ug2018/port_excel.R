list.of.packages <- c("openxlsx","data.table","reshape2","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/mattie_xlsx"
setwd(wd)

xlsxs = list.files(pattern="*.xlsx")

dat.list = list()

type_matches = c(
  "Total Despesa de Funcionamento",
  "Total Despesa de Investimento",
  "Total Geral"
  )
type_names = c(
  "Operational",
  "Development",
  "Total"
)
names(type_names) = type_matches

tab_col_names = c("opp_code","description","group_code","group_desc","internal","external","total","sector_raw","type","sector")

for(i in 1:length(xlsxs)){
  one.xlsx = xlsxs[i]
  province = substr(one.xlsx,46,nchar(one.xlsx)-5)
  message(province)
  prov_dat_raw = read.xlsx(
    one.xlsx,
    colNames = F
  )
  first_codigo = min(which(prov_dat_raw$X1=="Código"))
  prov_dat_raw = prov_dat_raw[(first_codigo+1):nrow(prov_dat_raw),]
  prov_dat_raw = subset(prov_dat_raw,(!is.na(X7)) | (!is.na(X2)))
  prov_dat_raw = subset(prov_dat_raw,!is.na(X1) & X1!="Código")
  prov_dat_raw$sector_raw = NA
  prov_dat_raw$sector_raw[which(is.na(prov_dat_raw$X7))] = prov_dat_raw$X2[which(is.na(prov_dat_raw$X7))]
  prov_dat_raw$sector_raw = na.locf(prov_dat_raw$sector_raw)
  prov_dat_raw = subset(prov_dat_raw,!is.na(X7))
  prov_dat_raw$type_raw = NA
  prov_dat_raw$type_raw[which(prov_dat_raw$X1 %in% type_matches)] = prov_dat_raw$X1[which(prov_dat_raw$X1 %in% type_matches)]
  prov_dat_raw$type_raw = na.locf(prov_dat_raw$type_raw,fromLast=T)
  prov_dat_raw$type = type_names[prov_dat_raw$type_raw]
  prov_dat_raw$type_raw = NULL
  prov_dat_raw$sector = NA
  prov_dat_raw$sector[which(grepl("educacao",prov_dat_raw$sector_raw,ignore.case=T))] = "Education"
  prov_dat_raw$sector[which(grepl("saude",prov_dat_raw$sector_raw,ignore.case=T))] = "Health"
  
  prov_dat = subset(prov_dat_raw,!is.na(sector))
  names(prov_dat) = tab_col_names
  prov_dat$province = province
  prov_dat$district = colsplit(prov_dat$sector_raw," DE ",c("x1","x2","district"))$district
  prov_dat$internal = as.numeric(gsub(",",".",gsub(".","",prov_dat$internal,fixed=T)))
  prov_dat$external = as.numeric(gsub(",",".",gsub(".","",prov_dat$external,fixed=T)))
  prov_dat$total = as.numeric(gsub(",",".",gsub(".","",prov_dat$total,fixed=T)))
  
  dat.list[[province]] = prov_dat
}

total_dat = rbindlist(dat.list)
fwrite(total_dat,"total_province.csv")

setdiff(substr(one.xlsx,46,nchar(one.xlsx)-5),unique(total_dat$province))
