#' andmete downloadi abifunktsioon
#' @export
andmedSisse2=function(url="https://www.riigiteenused.ee/api/et/all") {
  #library(rjson)
  library(jsonlite)
  #loeme andmed sisse
  lai=fromJSON(url)
  flatten(lai[[2]])
}


