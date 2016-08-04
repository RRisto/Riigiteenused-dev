#abifunktsioon kõikide kanalite pikaks tegemiseks
#kasutab data.table'i formaati
#' @export
korrastaja=function(andmed, mootmiseAasta) {
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  #kanalite lõikes meldime
  kanalid=c("Veebileht / portaal.","E-iseteenindus.","Eesti.ee.",
            "Nutirakendus.","Digitelevisioon.","E-post.","Tekstisõnum.",
          "Telefon.","Faks.","Post.","Letiteenus.","Kliendi juures.")

  koos=mapply(meltimine, kanal=kanalid,MoreArgs=list(data=andmed),
               SIMPLIFY = F)
  #keevitame üheks dfks
  koos=rbindlist(koos, fill=TRUE)
  #eemaldame kanali ja näitaja ning paneme eraldi veergu
  if(length(koos)==0) {
    return(NULL)
  } else {
    koos[,variable:=gsub(".ee.", ".", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Letiteenus büroos", "Teeninduslett",
                         as.character(koos[,variable]), fixed=T)]
    # koos[,variable:=gsub("E-iseteenindus", "Eiseteenindus",
    #                      as.character(koos[,variable]), fixed=T)]
    # koos[,variable:=gsub("E-post", "Epost",
                        # as.character(koos[,variable]), fixed=T)]
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
