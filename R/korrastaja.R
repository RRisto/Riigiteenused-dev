#abifunktsioon kõikide kanalite pikaks tegemiseks
#kasutab data.table'i formaati
#' @export
korrastajaDT=function(andmed, eemalda, mootmiseAasta) {
  library(data.table)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  setnames(andmed, names(andmed), gsub(eemalda, "", names(andmed)))
  #kanalite lõikes meldime
  veeb=meltimineDT("Veebileht / portaal.", data=andmed)
  iseteen=meltimineDT("E-iseteenindus.", data=andmed)
  eesti=meltimineDT("Eesti.ee.", data=andmed)
  nuti=meltimineDT("Nutirakendus.", data=andmed)
  digitv=meltimineDT("Digitelevisioon.", data=andmed)
  epost=meltimineDT("E-post.", data=andmed)
  sms=meltimineDT("Tekstisõnum.", data=andmed)
  telefon=meltimineDT("Telefon.", data=andmed)
  faks=meltimineDT("Faks.", data=andmed)
  post=meltimineDT("Post.", data=andmed)
  lett=meltimineDT("Letiteenus.", data=andmed)
  kodus=meltimineDT("Kliendi juures.", data=andmed)

  #rbindime
  koos=rbindlist(list(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon,
                      faks, post, lett, kodus))

  #eemaldame kanali ja näitaja ning paneme eraldi veergu
  if (length(koos)==0) {
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
