#andmed pikaks, võtab eelenvad funktsioonid kokku, iga
#aasta andmed teeb pikaks, kasutab data.table'i formaati
#' @export
andmedPikaks=function(andmedLai) {
  library(data.table)
  andmed=data.table(andmedLai)
  #turn into list based on measuring years
  andmedLaiList=list(
    andmedLai.2015=andmed[, !grepl("empty.|2011.|2013.|2012.|2014.",
                                names(andmed)), with=F],
  andmedLai.2014=andmed[, !grepl("empty.|2011.|2013.|2012.|2015.",
                                names(andmed)), with=F],
  andmedLai.2013=andmed[, !grepl("empty.|2011.|2012.|2014.|2015.",
                                names(andmed)), with=F],
  andmedLai.2012=andmed[, !grepl("empty.|2011.|2013.|2014.|2015.",
                                names(andmed)), with=F],
  andmedLai.2011=andmed[, !grepl("empty.|2014.|2013.|2012.|2015.",
                                names(andmed)), with=F],
  andmedLai.Empty=andmed[, !grepl("2014.|2011.|2013.|2012.|2015.",
                                 names(andmed)), with=F])

  years=gsub("[^0-9]","",names(andmedLaiList))#get years
  years=ifelse(nchar(years)==0, "empty", years)#replace empty with "empty"
  years=as.list(years)#make it list

  andmedLaiList=Map(cbind, andmedLaiList, year = years)#add measuring years
  #lapply through korrastaja
  andmedLaiList <- lapply(andmedLaiList,
                          function(df) {
    korrastaja(andmed=df, eemalda=paste0(df$year,".")[1],
               mootmiseAasta=df$year[1])
  })
  #return as one data frame/tables
  andmedPikk=rbindlist(andmedLaiList, fill=TRUE)
  andmedPikk[, value:=as.numeric(as.character(value))]#make char to value
  andmedPikk[, MootmiseAasta:=gsub("empty", "pole moodetud", MootmiseAasta)]
}
