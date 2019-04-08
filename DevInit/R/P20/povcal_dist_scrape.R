list.of.packages <- c("data.table","XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

povcal_dist = function(C0="AGO_3",Y0=2015){
  dist_url = paste0("http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0=",C0,"&Y0=",Y0)
  dist_html = htmlParse(dist_url, isURL=T)
  dist_root = xmlRoot(dist_html)
  dist_txt = xmlValue(getNodeSet(dist_root, "//pre")[[1]])
  start_point = gregexpr(pattern="---\r\n", dist_txt)[[1]][4]
  end_point = gregexpr(pattern="\r\n---", dist_txt)[[1]][5]
  if(start_point<end_point){
    txt_table = substr(dist_txt,start_point+5,end_point-1)
    df = read.table(text=txt_table, header=F, col.names=c("i","P", "L"))
    return(df)
  }else{
    return(FALSE)
  }
}

ago = povcal_dist()
alg = povcal_dist("DZA_3")
