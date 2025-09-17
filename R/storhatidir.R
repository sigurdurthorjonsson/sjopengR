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
