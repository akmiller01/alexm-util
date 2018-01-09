list.of.packages <- c("RCurl", "rjson","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RCurl)
library(rjson)
library(data.table)

imfDB <- function(){
  user_agent = "di-imf-rsdmx/0.0.1"
  
  data_flow_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow"
  
  content <- getURL(data_flow_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  Sys.sleep(1)
  
  rawJson <- fromJSON(content)
  structure <- rawJson$Structure
  dataflows <- structure$Dataflows$Dataflow
  
  db.key <- c()
  db.name <- c()
  for(dataflow in dataflows){
    db.key <- c(db.key,dataflow$KeyFamilyRef$KeyFamilyID)
    db.name <- c(db.name,dataflow$Name$`#text`)
  }
  df <- data.frame(db.key,db.name)
  return(df)
}

imfDS <- function(db.key){
  user_agent = "di-imf-rsdmx/0.0.1"
  
  ds_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/"
  
  ds_url <- paste0(ds_base_url,db.key)
  dsContent <- getURL(ds_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  Sys.sleep(1)
  
  #Clean unescaped quotes by removing HTML tags
  dsContent = gsub("\\<[^>]*>", "", dsContent, perl=TRUE)
  ds <- fromJSON(dsContent)$Structure
  return(ds)
}

imfCL <- function(db.key){
  # user_agent = "di-imf-rsdmx/0.0.1"
  # cl_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CodeList/"
  # ds <- imfDS(db.key)
  # codelistIDs <- ds$CodeLists$CodeList
  # codelists <- list()
  # for(codelistID in codelistIDs){
  #   message(codelistID$`@id`)
  #   cl_url <- paste0(cl_base_url,codelistID$`@id`)
  #   clContent <- getURL(cl_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  #   Sys.sleep(1)
  #   cl <- fromJSON(clContent)$Structure$CodeLists$CodeList
  #   codelists[[codelistID$`@id`]] <- cl
  # }
  # return(codelists)
  
  listOfLists <- list()
  lolIndex <- 1
  ds <- imfDS(db.key)
  codelists <- ds$CodeLists$CodeList
  for(codelist in codelists){
    dim.name <- codelist$Name$`#text`
    dim.code <- codelist$`@id`
    for(code in codelist$Code){
      code.val <- code$`@value`
      code.desc <- code$Description$`#text`
      df <- data.frame(dim.name,dim.code,code.val,code.desc)
      listOfLists[[lolIndex]] <- df
      lolIndex <- lolIndex + 1
    }
  }
  output.df <- rbindlist(listOfLists)
  return(output.df)
}

imfCD <- function(db.key,params="",startPeriod="",endPeriod=""){
  user_agent = "di-imf-rsdmx/0.0.1"
  cd_base_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/"
  
  cd_url <- paste0(cd_base_url,db.key,"/",params)
  if(startPeriod!=""){
    if(endPeriod!=""){
      cd_url <- paste0(cd_url,"?startPeriod=",startPeriod,"&endPeriod=",endPeriod)
    }
    else{
      cd_url <- paste0(cd_url,"?startPeriod=",startPeriod)
    }
  }else{
    if(endPeriod!=""){
      cd_url <- paste0(cd_url,"?endPeriod=",endPeriod)
    }
  }
  cdContent <- getURL(cd_url, httpheader = list('User-Agent' = user_agent), ssl.verifypeer = FALSE, .encoding = "UTF-8")
  Sys.sleep(1)
  cd <- fromJSON(cdContent)
  return(cd)
}

imfDF <- function(db.key,params="",startPeriod="",endPeriod=""){
  dfList <- list()
  dfIndex <- 1
  
  cd <- imfCD(db.key,params,startPeriod,endPeriod)
  series <- cd$CompactData$DataSet$Series
  # Special case for series being a single list
  if("@FREQ" %in% names(series)){
    df <- data.frame(db.key)
    for(var in names(series)){
      # For those not named "obs" we can assign them to vars right away
      if(var!="Obs"){
        value = series[[var]]
        if(typeof(value)=="character"){
          df[[var]] <- series[[var]]
        }
        # Otherwise, we need to check whether "Obs" is a list of observations,
        # or a list containing the attributes of one observation
      }else{
        obs = series[[var]]
        single.ob <- FALSE
        for(ob in obs){
          if(typeof(ob)=="list"){
            multivar <- paste0("value.",ob$`@TIME_PERIOD`)
            df[[multivar]] <- ob$`@OBS_VALUE`
          }else{
            single.ob <- TRUE
          }
        }
        if(single.ob){
          multivar <- paste0("value.",obs$`@TIME_PERIOD`)
          df[[multivar]] <- obs$`@OBS_VALUE`
        }
      }
    }
    dfList[[dfIndex]] <- df
    dfIndex = dfIndex + 1
  }else{
    # Each element is like a row, except it can contain more than one observation
    for(element in series){
      df <- data.frame(db.key)
      for(var in names(element)){
        # For those not named "obs" we can assign them to vars right away
        if(var!="Obs"){
          value = element[[var]]
          if(typeof(value)=="character"){
            df[[var]] <- element[[var]]
          }
          # Otherwise, we need to check whether "Obs" is a list of observations,
          # or a list containing the attributes of one observation
        }else{
          obs = element[[var]]
          single.ob <- FALSE
          for(ob in obs){
            if(typeof(ob)=="list"){
              multivar <- paste0("value.",ob$`@TIME_PERIOD`)
              df[[multivar]] <- ob$`@OBS_VALUE`
            }else{
              single.ob <- TRUE
            }
          }
          if(single.ob){
            multivar <- paste0("value.",obs$`@TIME_PERIOD`)
            df[[multivar]] <- obs$`@OBS_VALUE`
          }
        }
      }
      dfList[[dfIndex]] <- df
      dfIndex = dfIndex + 1
    }
  }
  
  full.df <- rbindlist(dfList,fill=TRUE)
  #Reorder the value columns if at least 2
  if(sum(grepl("value.",names(full.df)))>1){
    valueOrder <- order(as.numeric(substr(names(full.df)[which(substr(names(full.df),1,6)=="value.")],7,10)))
    names(full.df)[(length(full.df)+1-length(valueOrder)):length(full.df)] <- names(full.df)[(length(full.df)+1-length(valueOrder)):length(full.df)][valueOrder]
  }
  return(full.df)
}

rm(list.of.packages,new.packages)

### How to use:

#1. Get db.keys from imfDB function
# dbs <- imfDB()
#2. Use db.key from step 1 to find codelist using imfCL function
# gfsrCL <- imfCL("GFSR")
#3. Use codelist dimensions from step 2 to construct parameters for imfDF function
# gfsrDF <- imfDF("GFSR","A..S1311B.XDC.W0_S1_G1","2000","2017")

### How to construct imfDF params argument:

# {frequency}.{item1 from dimension1}+{item2 from dimension1}+{item N from dimension1}.{item1 from
# dimension2}+{item2 from dimension2}+{item M from dimension2}.{etc}
# Leave a blank string for wildcard filters. In the above example "A..S1311B" is filtering wildcard for
# the second dimension (ref_area)

### How to use intermediate functions (not exported, check source):

# gfsrDS <- imfDS("GFSR")
# cd <- imfCD("GFSR","A..S1311B.XDC.W0_S1_G1","2014","2016")

### How to source this package:

#source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/imf_sdmx_func.R")

### Help docs:

# http://datahelp.imf.org/knowledgebase/articles/788715-data-services-how-to-get-the-code-of-a-dataset-an
# http://datahelp.imf.org/knowledgebase/articles/667681-using-json-restful-web-service
# http://dataservices.imf.org/REST/SDMX_JSON.svc/help
# http://datahelp.imf.org/knowledgebase/articles/937155-how-to-query-large-datasets-with-data-services

