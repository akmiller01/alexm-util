list.of.packages <- c("data.table","tcltk","XML","xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/Users/leightonj/Documents")

# all_xlsx = list.files(path="S:/",pattern="*.xlsx",recursive=T,ignore.case=T,full.names=T)
# all_xls = list.files(path="S:/",pattern="*.xls",recursive=T,ignore.case=T,full.names=T)
# 
# save(all_xlsx,all_xls,file="s_drive_analysis_files.RData")
load("s_drive_analysis_files.RData")

pb = tkProgressBar(title="S drive link analysis", min=0, max = length(all_xlsx)+length(all_xls), width=600)

linked.xlsx = function(xlsx.file){
  tmp = tempdir()
  unzipped.xlsx = unzip(xlsx.file,overwrite=T,exdir=tmp,files="xl/externalLinks/_rels/externalLink1.xml.rels")
  if(length(unzipped.xlsx)>0){
    unlink(tmp,recursive=T)
    return(TRUE)
  }else{
    unlink(tmp,recursive=T)
    return(FALSE)
  }
}

linked.xls = function(xls.file){
  wb = loadWorkbook(xls.file)
  sheets = getSheets(wb)
  is.linked = FALSE
  for(i in 1:length(sheets)){
    raw = unlist(read.xlsx(file=xls.file,sheetIndex=i,header=F,keepFormulas=T))
    contains.linked = sum(grepl(".xls]",raw,ignore.case=T)) | sum(grepl(".xlsx]",raw,ignore.case=T))
    is.linked = is.linked | contains.linked
  }
  return(is.linked)
}

link_list = list()
link_index = 1

for(xlsx.file in all_xlsx){
  setTkProgressBar(pb, link_index, label=xlsx.file)
  xlsx_is_linked = tryCatch({return(linked.xlsx(xlsx.file))},error={return(NA)})
  link_list[[link_index]] = data.frame(filename=xlsx.file,linked=xlsx_is_linked)
  link_index = link_index + 1
}

save(link_list,file="s_drive_xlsx_list.RData")

for(xls.file in all_xls){
  setTkProgressBar(pb, link_index, label=xls.file)
  xls_is_linked = tryCatch({return(linked.xls(xls.file))},error={return(NA)})
  link_list[[link_index]] = data.frame(filename=xls.file,linked=xls_is_linked)
  link_index = link_index + 1
}

link.df = rbindlist(link_list)
fwrite(link.df,"s_drive_links.csv")
close(pb)
