#' Finnur stórhátíðardaga gefins árs.
#'
#' Finnur stórhátíðardaga skv kjarasamningi fyrir gefið ár
# með því að nota fallið 'holiday' í pakkanum 'timeDate'.
#
#' @param yyyy Ár.
#' @return Stórhátíðardagar gefið ár.
#' @references https://www.fin.is/media/samningar/Kjarasamningur-FIN-og-rikis-28.februar-2018.pdf
#' @export

storhatidir <- function(yyyy)
{
  hatidir <- c(
    as.Date(paste0(yyyy,"-01-01")),                     # Nýársdagur
    as.Date(timeDate::holiday(yyyy,"GoodFriday")),      # Föstudagurinn langi
    as.Date(timeDate::holiday(yyyy,"EasterSunday")),    # Páskadagur
    as.Date(timeDate::holiday(yyyy,"Pentecost")),       #  Hvítasunnudagur
    as.Date(paste0(yyyy,"-06-17")),                     # 17. júní
    as.Date(paste0(yyyy,"-12-24"))                      # Aðfangadagur
  )
  names(hatidir) <- c("Nýársdagur","Föstudagurinn langi","Páskadagur",
    "Hvítasunnudagur","17. júní","Aðfangadagur")
  hatidir
}
