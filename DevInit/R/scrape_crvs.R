list.of.packages <- c("data.table","rvest","XML","httr","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

base.url = "https://data.unicef.org/resources/crvs/uganda/"

start.html = htmlParse(content(GET(base.url)))

link.elems = getNodeSet(start.html,"//ul[@class='dropdown-menu']/li/a")
links = sapply(link.elems,xmlGetAttr,"href")

dat_list = list()
for(link in links){
  file = read_html(link)
  country_name = html_text(html_nodes(file,"h3")[3])
  message(country_name)
  tables = html_nodes(file, "table")
  birth = html_table(tables[1])[[1]]
  birth$type = "birth"
  processing_ind = which(birth$X1=="Processing")
  if(length(processing_ind)>1){
    birth$X1[processing_ind[2]] = "Vital Statistics Processing"
  }
  death = html_table(tables[3])[[1]]
  death$type = "death"
  reg_ind = which(death$X1=="Legal framework for death registration")
  if(length(reg_ind)>1){
    death$X1[reg_ind[2]] = "Legal framework for death registration explanation"
  }
  tmp = rbind(birth,death)
  tmp$country = country_name
  dat_list[[country_name]] = tmp
}

dat = rbindlist(dat_list)
names(dat) = c("Question","Answer","Type","Country")
dat_m = melt(dat,id.vars=c("Question","Type","Country"))

dat_w = dcast(dat_m,Question+Type~Country)
dat_w = dat_w[order(dat_w$Type),]

fwrite(dat_w,"unicef_crvs_scrape.csv")
