# Requires Python and `pip install youtube_transcript_api` on your system

list.of.packages <- c("data.table","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

extract_subs = function(youtube_url, language_code="en"){
  vid = regmatches(youtube_url, regexec("v=([^&]+)", youtube_url))[[1]][2]
  py_cmd = "youtube_transcript_api"
  py_args = c(vid, "--language", language_code, "--json")
  result_json = system2(command=py_cmd, args=py_args, stdout=T)
  result_json_concat = paste(result_json, collapse=" ")
  result = data.frame(fromJSON(txt=result_json_concat)[[1]])
  return(result)
}

youtube_url = "https://www.youtube.com/watch?v=EEBWAGYT090"
sub_df = extract_subs(youtube_url)