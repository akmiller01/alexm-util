list.of.packages <- c("data.table","Hmisc","tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

load("~/Downloads/s_drive_xlsx_list.RData")
s_drive_links = rbindlist(link_list)

describe(s_drive_links$linked)

split_path <- function(path) {
  setdiff(strsplit(path,"/|\\\\")[[1]], "")
} 

s_drive_links$filename = as.character(enc2utf8(s_drive_links$filename))

s_drive_links_paths = sapply(s_drive_links$filename, split_path)

s_drive_links$top = sapply(s_drive_links_paths,`[`,2)
table(s_drive_links$top,s_drive_links$linked)

s_drive_links$second = sapply(s_drive_links_paths,`[`,3)

project_links = subset(s_drive_links,top=="Projects")
table(project_links$second,project_links$linked)

s_drive_links$basename = basename(enc2utf8(s_drive_links$filename))
length(unique(s_drive_links$basename))
