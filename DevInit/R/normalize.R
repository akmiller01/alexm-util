# A function to take any arbitrary data frame, and depending on column types and statistics,
# return a data frame that contains values constrained between 0 and 1
#Arguments are:
#df, the dataframe containing the information used to generate the wealth index
#catvars, a vector of variable names or column numbers that contain categorical data for the index
#numvars, a vector of variable names or column numbers that contain numerical/continuous data for the index
#Function returns another data frame, with all generated dummy vars and numvars linearly transformed between 0 and 1

#By Alex Miller, Development Initiatives, September 2016

require(data.table)

normalize <- function(dataf,catvars=NULL,numvars=NULL){
  
  #Function to name dummies nicely if only column numbers are provided
  name.i <- function(i,df){
    return(names(df)[i])
  }
  if(length(catvars)>0){
    #Turn catvars into variable names, if integer or double
    if(typeof(catvars)=="double" |typeof(catvars)=="integer"){
      catvars <- sapply(catvars,name.i,df=dataf)
    }
  }
  
  #Turn numvars into variable names, if integer or double
  if(length(numvars)>0){
    if(typeof(numvars)=="double" |typeof(numvars)=="integer"){
      numvars <- sapply(numvars,name.i,df=dataf)
    }
  }
  
  #Function to generate binary dummy-variables from categorical variables
  #A bit messy with eval(parse(...)) but it was the only way I could find to make nicely named dummies
  generateDummies <- function(df,vars){
    dummyList <- list()
    listIndex <- 1
    for(i in 1:length(vars)){
      var = vars[i]
      df[,var] <- factor(df[,var])
      cmd = paste0("dum = model.matrix( ~ ",var," - 1, data=df)")
      eval(parse(text=cmd))
      dummyList[[listIndex]] = dum
      listIndex <- listIndex + 1
    }
    return(dummyList)
  }
  
  if(length(catvars)>0){
    dummyList <- generateDummies(dataf,catvars) 
  }
  
  #Function to bind a list by of dfs by rowname, make our dummies line up
  cbindlist <- function(list) {
    n <- length(list)
    res <- list[[1]]
    for (i in 2:n){
      item <- list[[i]]
      res <- cbind(res, item[match(rownames(res),rownames(item)),]) 
    }
    return(res)
  }
  
  if(length(catvars)>0){
    dummies <- cbindlist(dummyList)
  }
  
  ## In case there are categories you don't want generated, edit this and uncomment it
#   dummy.columns <- colnames(dummies)
#   deleted <- 0
#   
#   for(i in 1:length(dummy.columns)){
#     dummy.column <- dummy.columns[i]
#     if(
#       grepl("FALSE",dummy.column,ignore.case=TRUE) 
#       | grepl("99",dummy.column)
#       #         |  grepl("refuse",dummy.column,ignore.case=TRUE)
#     ){
#       index <- i-deleted
#       dummies <- dummies[,-index]
#       deleted <- deleted + 1
#     }
#   }
  
  if(length(catvars)>0){
    dataf <- cbindlist(list(dataf,dummies))
  }
  
  #Make sure our catvars and numvars are in the df, and have some non-zero standard deviation
  good.keep <- c()
  if(length(catvars)>0 & length(numvars)>0){
    wealth.vars <- c(numvars,colnames(dummies)) 
  }else if(length(catvars)>0){
    wealth.vars <- c(colnames(dummies)) 
  }else if(length(numvars)>0){
    wealth.vars <- c(numvars) 
  }
  for(i in 1:length(wealth.vars)){
    varname <- wealth.vars[i];
    if((varname %in% names(dataf))){
      dataf[[varname]][!complete.cases(dataf[[varname]])] <- 0
      var.sd <- sd(dataf[[varname]],na.rm=TRUE)
      if(var.sd!=0){
        good.keep <- c(good.keep,varname) 
      } 
    }
  }
  
  dat.numeric <- dataf[good.keep]
  
  for(i in 1:length(dat.numeric)){
    row = dat.numeric[i]
    max = max(row,na.rm=TRUE)
    min = min(row,na.rm=TRUE)
    if(min<0){
      row = (row-min)/(max-min)
    }else{
      row = row/max
    }
    dat.numeric[i] = row
  }
  
  return(dat.numeric)
}