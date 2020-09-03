list.of.packages <- c("XML", "data.table", "httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

url = "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE1/20005+20001+801+1+2+301+68+3+18+4+5+40+75+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302+20002+918+20006+611+72+62+30+82+546+613+552+83+70+84+45+77+87+566+732+764+765+55+576+20007.1.1010+1820.1140+1160.A+D+N/all?startTime=2009&endTime=2018"

content <- GET(url)

#check the presence of a BOM
BOM <- "\ufeff"
if(attr(regexpr(BOM, content), "match.length") != - 1){
  content <- gsub(BOM, "", content)
}

xml_doc <- xmlParse(content)
xml_root = xmlRoot(xml_doc)

dataset <- xml_root[["DataSet"]]

data_list = list()
data_index = 1
dataset_children = xmlChildren(dataset)
series = dataset_children[which(names(dataset_children)=="Series")]
for(row in series){
  row_children = xmlChildren(row)
  obs_list = row_children[which(names(row_children)=="Obs")]
  times = c()
  values = c()
  for(obs in obs_list){
    time = xmlValue(obs[["Time"]])
    times = c(times,time)
    value = xmlGetAttr(obs[["ObsValue"]], "value")
    values = c(values,value)
  }
  tmp = data.frame(time=times, value=values)
  keys = row[["SeriesKey"]]
  for(key_child in xmlChildren(keys)){
    key_concept = xmlGetAttr(key_child, "concept")
    key_value = xmlGetAttr(key_child, "value")
    tmp[,key_concept] = key_value
  }
  attrs = row[["Attributes"]]
  for(attr_child in xmlChildren(attrs)){
    attr_concept = xmlGetAttr(attr_child, "concept")
    attr_value = xmlGetAttr(attr_child, "value")
    tmp[,attr_concept] = attr_value
  }
  data_list[[data_index]] = tmp
  data_index = data_index + 1
}

dat = rbindlist(data_list,fill=T)
