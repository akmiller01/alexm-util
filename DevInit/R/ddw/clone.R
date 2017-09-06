library(data.table)

setwd("C:/git/visbox/core/static/core/data")
source("C:/git/alexm-util/DevInit/R/ddw/connect.R")

facts <- data.frame(schema="fact",table=schemaListTables("fact"))
dataSeries <- data.frame(schema="data_series",table=schemaListTables("data_series"))

meta <- rbind(facts,dataSeries)
write.csv(meta,"ddw.csv",na="",row.names=FALSE)

for(i in 1:nrow(meta)){
  name <- paste(meta$schema[i],meta$table[i],sep=".")
  message(name)
  filename <- paste0(name,".csv")
  dat <- ddw(name)
  if(nrow(dat)<50000){
    write.csv(dat,filename,na="",row.names=FALSE)
  }
}