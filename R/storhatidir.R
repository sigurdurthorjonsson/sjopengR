storhatidir <- function(yyyy)
{
  tibble::tibble(
    hatidisd=c(
      as.Date(paste0(yyyy,"-01-01")),           # Nýársdagur
      as.Date(timeDate::holiday(yyyy,"GoodFriday")),      # Föstudagurinn langi
      as.Date(timeDate::holiday(yyyy,"EasterSunday")),    # Páskadagur
      as.Date(timeDate::holiday(yyyy,"Pentecost")),       #  Hvítasunnudagur
      as.Date(paste0(yyyy,"-06-17")),           # 17. júní
      as.Date(paste0(yyyy,"-12-24"))            # Aðfangadagur
    ),
    stor=1
  )
}
