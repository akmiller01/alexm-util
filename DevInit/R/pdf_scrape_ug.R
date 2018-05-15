Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_171")
setwd("C:/Users/Alex/Documents/Data/Uganda2018/")

list.of.packages <- c("remotes","readr","data.table","rJava","miniUI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library(tabulizer)

pdf = "Development Volume I0.pdf"
# pages = 910
# page.seq = c(seq(0,pages,50),pages)
# out.list = c()
# for(i in 2:length(page.seq)){
#   start = page.seq[i-1]+1
#   end = page.seq[i]
#   message(start," ",end)
#   out.tmp = extract_tables(pdf,pages=c(start:end))
#   out.list = c(out.list,out.tmp)
# }
# 
# save(out.list,file="ug_scrape_2018.RData")

pages123 = extract_areas(pdf,pages=c(20:22))
