list.of.packages <- c("data.table","XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

remap_cov = function(x){
  cov_dict = c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov = Vectorize(remap_cov)

povcal_smy = function(pl=1.9,group.by="WB"){
  url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params = list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "RefYears"="all",
    "Display"="C",
    "GroupedBy"=group.by,
    "format"="csv"
  )
  param_names = names(params)
  for(param_name in param_names){
    param = params[[param_name]]
    url = paste0(url,param_name,"=",param,"&")
  }
  url = substr(url,1,nchar(url)-1)
  return(read.csv(url))
}

povcal_dist = function(C0="AGO_3",Y0=2015){
  dist_url = paste0("http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0=",C0,"&Y0=",Y0)
  dist_html = htmlParse(dist_url, isURL=T)
  dist_root = xmlRoot(dist_html)
  dist_txt = xmlValue(getNodeSet(dist_root, "//pre")[[1]])
  start_point = gregexpr(pattern="---\r\n", dist_txt)[[1]][4]
  end_point = gregexpr(pattern="\r\n---", dist_txt)[[1]][5]
  if(start_point<end_point){
    txt_table = substr(dist_txt,start_point+5,end_point-1)
    df = read.table(text=txt_table, header=F, col.names=c("i","P", "L"))
    return(df)
  }else{
    return(FALSE)
  }
}

ext = povcal_smy()
ext$svy_code = remap_cov(ext$CoverageType)
ext = subset(ext,!is.na(svy_code))
ext$C0 = paste(ext$CountryCode,ext$svy_code,sep="_")
svys = unique(ext$C0)
years = unique(ext$RequestYear)


data.list = list()
data.index = 1
for(year in years){
  for(svy in svys){
    message(year," ",svy)
    dist.tmp = tryCatch({povcal_dist(svy, year)},error=function(e){return(F)})
    if(dist.tmp!=FALSE){
      dist.tmp$svy = svy
      dist.tmp$year = year
      data.list[[data.index]] = dist.tmp
      data.index = data.index + 1
    }
  }
}

all_dist = rbindlist(data.list)
