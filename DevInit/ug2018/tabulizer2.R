list.of.packages <- c("tabulizer","data.table","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/Documents/mattie_pdfs"
setwd(wd)

tab_col_names = c("sprog","desc","recurrent","domestic","external","total")

pdfs = list.files(pattern="*.pdf")

dat.list = list()


for(i in 1:length(pdfs)){
  pdf = pdfs[i]
  district = substr(pdf,1,nchar(pdf)-15)
  message(district)
  tabs = extract_tables(
    pdf
    ,columns=list(c(95.7,395.6,482,576.5,667,756.6))
    ,guess=F
  )
  tab_dfs = lapply(tabs,data.frame)
  tab_df = rbindlist(tab_dfs)
  relevant_rows = subset(tab_df,X1 %in% c("D101","D102","D201","D202","D203"))
  names(relevant_rows) = tab_col_names
  rel_rows_long = melt(relevant_rows,id.vars=c("sprog","desc"))
  rel_rows_long$value = as.numeric(gsub(",","",rel_rows_long$value))
  rel_rows_long$district = district
  dat.list[[district]] = rel_rows_long
}

total_dat = rbindlist(dat.list)
fwrite(total_dat,"total_educ_dat.csv")

setdiff(substr(pdfs,1,nchar(pdfs)-15),unique(total_dat$district))
