list.of.packages <- c("data.table", "RPostgreSQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only=T)

type_mapping = c(
  "integer"="N",
  "text"="C",
  "double precision"="N",
  "boolean"="C"
)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv,
                dbname="analyst_ui"
                ,user="postgres")

table.name = "fts"
table.query = paste0("SELECT column_name, data_type FROM information_schema.columns where table_name= '",table.name,"';")


tab = dbGetQuery(con,table.query)
tab$data_type = type_mapping[tab$data_type]

dbDisconnect(con)
fwrite(tab,"~/tmp_col_types.csv")
