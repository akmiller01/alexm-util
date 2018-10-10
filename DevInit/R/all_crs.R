# Check whether packages are installed, and install them
list.of.packages <- c("readr","scrapeR","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Change WD to wherever you want to save CRS files
wd = "~/Downloads/"
setwd(wd)

# This is the URL where all the manual download links are located
download_url = "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"
data_root = "https://stats.oecd.org/FileView2.aspx?IDFile="

# Download the page source
download_source = scrape(download_url, headers=T,follow=T,parse=T)[[1]]

# Extract the link elements
link_elems = getNodeSet(download_source,"//a")

# The links don't store normal href objects, but we can construct new links from the `onclick` function
download_funcs = sapply(link_elems,xmlGetAttr,"onclick")
file_ids = gsub("_","-",substr(download_funcs,16,51))

# Get filenames from link names
download_names = sapply(link_elems,xmlValue)
file_names = paste0(sapply(strsplit(download_names," / "),`[`,1),".zip")

# Download the files if they don't already exist
for(i in 1:length(file_ids)){
  file_id = file_ids[i]
  file_name = file_names[i]
  if(!file.exists(file_name)){
    message("Downloading ", file_name)
    download.file(paste0(data_root, file_id), file_name)  
  }
}

# Unzip the files, put them in a list
crs.list = list()
for(file_name in file_names){
  contents = unzip(file_name)
  tmp = read_delim(contents,delim="|",col_types=cols(.default=col_character()),locale=locale())
  tmp = subset(tmp,Year!="\u001a")
  crs.list[[file_name]] = tmp
}

# Bind the list into a dataset
crs = rbindlist(crs.list)
save(crs, file="crs.RData")