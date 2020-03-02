list.of.packages <- c("googledrive", "tidyverse", "gargle","httr", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("/tmp")

previous.owners = c(
  "dale.potter@devinit.org",
  "wendy.rogers.devinit@gmail.com",
  "john.askew.devinit@gmail.com",
  "imogen.kutz@devinit.org",
  "jonihillman@gmail.com"
)

mimeDict = c(
  "application/vnd.google-apps.document" = "docx",
  "application/vnd.google-apps.spreadsheet" = "xlsx",
  "application/vnd.google-apps.presentation" = "pptx"
)

# for(previous.owner in previous.owners){}
previous.owner = previous.owners[1]

all.files = drive_find(q=paste0("'", previous.owner,"' in owners"))
# for(i in 1:nrow(all.files)){}
i = 3

g.drive.file = all.files[i,]
# Skip folders
if(g.drive.file$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder"){
  next
}

# Only copy static files
new.file = drive_cp(
  g.drive.file,
  g.drive.file$name
)
if(length(g.drive.file$drive_resource[[1]]$exportLinks)==0){
  next
}

fileId = g.drive.file$id
req <- request_generate(
  "drive.revisions.list",
  params = list(fileId = fileId),
  token = drive_token()
)
revs <-  response_process(request_make(req))

rev_ids = c()
for(rev_list in revs$revisions){
  rev_ids = c(rev_ids,rev_list$id)
}

# Only one revision, copy and move on, else
if(length(rev_ids)>1){
  for(revisionId in rev_ids){
    ext = mimeDict[g.drive.file$drive_resource[[1]]$mimeType]
    download_link = modify_url(
      url = "https://docs.google.com/feeds/download/documents/export/Export",
      query = list(
        id = fileId,
        revision = revisionId,
        exportFormat = ext
      )
    )
    tmp = tempfile(fileext=paste0(".",ext))
    download.file(download_link,tmp)
    drive_update(
      new.file,
      media=tmp
    )
  }
}

# drive_share(
#   file=new.file,
#   role="owner",
#   type="user",
#   emailAddress=new.owner
# )