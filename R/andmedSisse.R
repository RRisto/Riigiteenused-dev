#' andmete downloadi abifunktsioon
#' @export
andmedSisse=function(url="https://www.riigiteenused.ee/api/et/all") {
  #loeme andmed sisse
  library(rjson)
  library(jsonlite)
  fromJSON(url, flatten=T)
}


