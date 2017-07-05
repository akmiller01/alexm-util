library(readr)

readdct = function(dct_file){
  first_line <- readLines(dct_file)[1]
  file_name <- gsub('.*\\"(.*)\\".*', "\\1", first_line)
  base_name <- basename(file_name)
  file_path <- dirname(dct_file)
  full_path_file <- paste(file_path,base_name,sep="/")
  tab <- read.table(dct_file,header=FALSE,skip=2,sep="",fill=TRUE
                    ,col.names=c("data.type","var.name","start.string1","start.string2")
                    ,as.is=TRUE
                    )
  tab$start.string = paste(tab$start.string1,tab$start.string2)
  tab <- subset(tab,start.string!=" ")
  tab$start <- as.integer(gsub(".*\\:(.*)\\-.*", "\\1", tab$start.string))
  tab$end <- as.integer(gsub(".*\\-(.*)", "\\1", tab$start.string))
  tab$width <- (tab$end-tab$start)+1
  dat <- read_fwf(full_path_file
                 ,fwf_widths(tab$width,col_names=tab$var.name)
  )
  return(data.frame(dat))
}