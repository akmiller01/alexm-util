list.of.packages <- c("readr","data.table","tools","devtools","rJava")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T)

# install_github("ropenscilabs/tabulizerjars")
# install_github("ropenscilabs/tabulizer")
library(tabulizer)

setwd("/home/alex/Documents/budget_scrape_2018/")

tables = extract_tables(
  "Federal Ministry of Health 2016.pdf"
  , pages=c(4:88)
  , guess=F
  , columns = list(c(145, 432, 486))
  , area = list(c(35, 40, 785, 565))
  )

output.list = list()

for(i in 1:length(tables)){
  table = data.frame(tables[i])
  message(i, " ", dim(table)[1], " ", dim(table)[2])
  output.list[[i]] = table
}

output = rbindlist(output.list)
names(output) = c("CODE","LINE ITEM", "TYPE", "AMOUNT")
write_csv(output,"output/2016-budget.csv")
