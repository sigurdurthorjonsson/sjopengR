serstakir <- function(yyyy)
{
  sumardagur <- seq(as.Date(paste0(yyyy,"-04-19")),length=7)
  sumardagur <- sumardagur[as.POSIXlt(sumardagur)$wday==4]

  fridagur <- seq(as.Date(paste0(yyyy,"-08-01")),length=7)
  fridagur <- fridagur[as.POSIXlt(fridagur)$wday==1]

  tibble::tibble(
    serstakurd=c(
      as.Date(timeDate::holiday(yyyy,"GoodFriday"))-1,       # Skírdagur
      as.Date(timeDate::holiday(yyyy,"EasterMonday")),       # Annar í páskum
      sumardagur,                                            # Sumardagurinn fyrsti
      as.Date(paste0(yyyy,"-05-01")),                        # 1. maí
      as.Date(timeDate::holiday(yyyy,"Ascension")),          # Uppstigningardagur
      as.Date(timeDate::holiday(yyyy,"Pentecost"))+1,        # Annar í hvítasunnu
      fridagur,                                              # Frídagur verslunarmanna
      as.Date(paste0(yyyy,"-12-25")),                        # Jóladagur
      as.Date(paste0(yyyy,"-12-26")),                        # Annar í jólum
      as.Date(paste0(yyyy,"-12-31"))                         # Gamlársdagur
    ),
    spes=1
  )
}
