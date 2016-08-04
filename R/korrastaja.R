#abifunktsioon kõikide kanalite pikaks tegemiseks
#kasutab data.table'i formaati
#' @export
korrastaja=function(andmed, eemalda, mootmiseAasta) {
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  setnames(andmed, names(andmed), gsub(eemalda, "", names(andmed)))
  #kanalite lõikes meldime
  kanal=c("Veebileht / portaal.","E-iseteenindus.","Eesti.ee.", "Nutirakendus.",
          "Digitelevisioon.","E-post.","Tekstisõnum.","Telefon.","Faks.","Post.",
          "Letiteenus.","Kliendi juures.")
  koos=mapply(meltimine, kanal=kanal,MoreArgs=list(data=andmed))
  #keevitame üheks dfks
  koos=rbindlist(koos, fill=TRUE)
  #eemaldame kanali ja näitaja ning paneme eraldi veergu
  if(length(koos)==0) {
    return(NULL)
  } else {
    koos[,variable:=gsub(".ee.", ".", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Letiteenus büroos", "Letiteenus",
                         as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-iseteenindus", "Eiseteenindus",
                         as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-post", "Epost",
                         as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Veebileht / portaal", "Veebileht",
                         as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Kliendi juures", "Kliendijuures",
                         as.character(koos[,variable]), fixed=T)]
    koos[, c("kanal", "naitaja") := tstrsplit(
      as.character(koos[["variable"]]), "\\.(?=[^\\.]+$)", perl=T)]
    koos[,kanal:=gsub("^.*\\.", "", koos[, kanal])]
    #viskame välja tühjad read, kus pole linki
    koos=koos[link!="NA"]
    koos[,MootmiseAasta:=mootmiseAasta]
    koos
  }
}
