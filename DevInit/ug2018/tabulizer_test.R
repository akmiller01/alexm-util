list.of.packages <- c("tabulizer","data.table","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

file_path = "/home/alex/Downloads/Basic District Parameters.pdf"

educ_cols = c(
  "Nursery",
  "KG.Total",
  "P.1",
  "P.2",
  "P.3",
  "P.4",
  "P.5",
  "P.6",
  "P.Total",
  "JH.1",
  "JH.2",
  "JH.3",
  "JH.Total",
  "KG.1",
  "KG.2"
)
educ_col_order = c(1,14,15,2:13)

educ.list = list()

# Too big to do all at once. First half

districts_regions = trimws(extract_text(file_path, pages=c(1:108),area=list(c(36,407,59,793))))
dist_reg_tab = data.frame(colsplit(districts_regions,",",c("dist","reg")))
educ_extract = extract_tables(file_path, pages=c(1:108),area=list(c(110,35,228,771)),guess=F)
educ_dims = data.frame(t(sapply(educ_extract,dim)))
names(educ_dims)=c("rows","cols")

for(i in 1:length(educ_extract)){
  educ_tab = data.frame(educ_extract[[i]])
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)trimws(x)))
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)gsub(',', '',x)))
  
  educ_tab = subset(educ_tab,!X1 %in% c("","Private"))
  if(ncol(educ_tab)==16){
    educ_tab[,c("X1","X4")] = NULL
  }else if(ncol(educ_tab)==17){
    educ_tab[,c("X1","X4","X15")] = NULL
  }
  
  educ_tab[,c("KG.1","KG.2")] = colsplit(educ_tab$X3, " ",  c("a","b"))
  educ_tab$X3 = NULL
  names(educ_tab) = educ_cols
  educ_tab = educ_tab[,educ_col_order]
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)as.numeric(x)))
  
  educ_tab$gender = c("Boys","Girls","Total","Boys","Girls","Total")
  educ_tab$public = c(T, T, T, F, F, F)
  educ_tab_long = melt(educ_tab,id.vars=c("gender","public"),variable_name = "grade")
  educ_tab_long[,c("district","region")] = dist_reg_tab[i,]
  educ.list[[i]] = educ_tab_long
}

# Second half

districts_regions = trimws(extract_text(file_path, pages=c(109:216),area=list(c(36,407,59,793))))
dist_reg_tab = data.frame(colsplit(districts_regions,",",c("dist","reg")))
educ_extract = extract_tables(file_path, pages=c(109:216),area=list(c(110,35,228,771)),guess=F)
educ_dims = data.frame(t(sapply(educ_extract,dim)))
names(educ_dims)=c("rows","cols")

for(i in 1:length(educ_extract)){
  educ_tab = data.frame(educ_extract[[i]])
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)trimws(x)))
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)gsub(',', '',x)))
  
  educ_tab = subset(educ_tab,!X1 %in% c("","Private"))
  if(ncol(educ_tab)==16){
    educ_tab[,c("X1","X4")] = NULL
  }else if(ncol(educ_tab)==17){
    educ_tab[,c("X1","X4","X15")] = NULL
  }
  
  educ_tab[,c("KG.1","KG.2")] = colsplit(educ_tab$X3, " ",  c("a","b"))
  educ_tab$X3 = NULL
  names(educ_tab) = educ_cols
  educ_tab = educ_tab[,educ_col_order]
  educ_tab = as.data.frame(apply(educ_tab,2,function(x)as.numeric(x)))
  
  educ_tab$gender = c("Boys","Girls","Total","Boys","Girls","Total")
  educ_tab$public = c(T, T, T, F, F, F)
  educ_tab_long = melt(educ_tab,id.vars=c("gender","public"),variable_name = "grade")
  educ_tab_long[,c("district","region")] = dist_reg_tab[i,]
  educ.list[[i+108]] = educ_tab_long
}

all_educ = rbindlist(educ.list)
setwd("~")
save(all_educ,file="all_educ.RData")
fwrite(all_educ,"all_data.csv")
