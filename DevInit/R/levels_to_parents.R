library(data.table)

setwd("C:/git/digital-platform/country-year")

dat <- read.csv("domestic.csv",na.strings="",as.is=TRUE)

id.refs <- c("id","year","budget.type")
levels <- c("l1","l2","l3","l4","l5","l6")
value.ref <- "value"


data.list <- list()
data.index <- 1

#Root exists at every unique combination of id.refs values
root <- apply(unique(dat[id.refs]),1,paste0,collapse=";")
dataf <- data.frame(root,id.string=root,parent.id=NA,value=NA)
data.list[[data.index]] <- dataf
data.index <- data.index + 1

for(i in 1:nrow(dat)){
  if(i %% 10000 == 0){
    message(paste("Row:",i))
  }
  for(j in 1:length(levels)){
    if(!is.na(dat[i,levels[j]])){
      #Level is not blank, overwritten until the end
      depth <- j
      parent.depth <- j-1
    }
  }
  root <- paste(dat[i,id.refs],collapse=";")
  id.levels <- levels[1:depth]
  id.string <- paste(c(root,dat[i,id.levels]),collapse=";")
  if(parent.depth>1){
    parent.id.levels <- levels[1:parent.depth]
    parent.id.string <- paste(c(root,dat[i,parent.id.levels]),collapse=";")
  }else{
    parent.id.string <- root
  }
  value <- dat[i,value.ref]
  dataf <- data.frame(root,id.string,parent.id.string,value)
  data.list[[data.index]] <- dataf
  data.index <- data.index + 1
  
}

hier.dat <- rbindlist(data.list)
write.csv(hier.dat,"C:/Users/Alex/Documents/Data/hierdat.csv",na="",row.names=FALSE)
