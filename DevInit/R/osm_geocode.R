list.of.packages <- c("data.table","httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

geo_cache = list()

osm_geocode = function(place_name, agent="dean.breed@devinit.org;IATI_geocoder"){
  if(!(place_name %in% names(geo_cache))){
    Sys.sleep(1)
    query_url = paste0(
      "https://nominatim.openstreetmap.org/search?format=json&q=",
      URLencode(place_name)
    )
    geo_cache[[place_name]] <<- fromJSON(query_url)
  }
  return(geo_cache[[place_name]])
}
