#andmed pikaks, võtab eelenvad funktsioonid kokku, iga
#aasta andmed teeb pikaks, kasutab data.table'i formaati
#' @export
andmedPikaks2=function(andmedLai) {
  tulem=data.frame(NULL)
  for (i in 1:nrow(andmedLai)) {
    #id=ver2$identifier[i]
    sub=andmedLai$serviceStatistics[i]
    aastad=sub[[1]][2]
    teenustulem=data.frame(NULL)
    for (j in 1:nrow(aastad)) {
      abikas=sub[[1]][3]$availableChannel[[j]]
      moodikud=abikas$channelStatistics
      taisrida=cbind(moodikud, abikas[, c("url", "additionalType", "payment")])
      taisrida=cbind(taisrida, andmedLai[i, c("domain", "subdomain", "serviceType",
                                              "provider.name", "provider.memberOf.name")])
      taisrida$aasta=aastad$value[j]
      teenustulem=rbind(teenustulem, taisrida)
    }
    teenustulem$id=andmedLai$identifier[i]
    tulem=rbind(tulem, teenustulem)
  }


  library(reshape2)
  pikk=melt(tulem, measure.vars=c("transaction", "satisfaction", "cost",
                                  "activeTimeSpent", "totalTimeSpent"))

  #kus on müüdik puudu paenme NA
  pikk$value=ifelse(pikk$value=="", NA, pikk$value)
  #kui mõõdik on NA, siis paneme ka aasta emtpyks
  pikk$aasta=ifelse(is.na(pikk$value), "empty", pikk$aasta)
  #eemaldame duplikaadid
  pikk[!duplicated(pikk), ]
}
