#' Reiknar sjópeningar útfrá forsendum í töflu.
#'
#' Reiknar sjópeningar útfrá forsendum í töflu, í einingum mánaðarlauna, 
#' eða í krónum ef grunnlaun eru gefin  
#' með því að nota fallið 'holiday' í pakkanum 'timeDate'.
#' ATH að stórhátíðadögum í upptalningu kjarasamnings er sleppt.
#
#' @param tafla Sjópeningatafla, úttak fallsins 'sjopengR::sjopeng'.
#' @param manadar_laun Sjálfgefið 1, annar hægt að gefa upp kaupið sitt.
#' @return Yfirlit sjópeninga skipt á launaliði.
#' @references https://www.fin.is/media/samningar/Kjarasamningur-FIN-og-rikis-28.februar-2018.pdf
#' @export

reikn_sjpng <- function(tafla, manadar_laun=1)
{
  tafla |>
    dplyr::summarize(
      sjopeningar = manadar_laun*sum(sjopeningar*prosentu_alag/100),
      oalagI = manadar_laun*sum(oalagI*0.3333*prosentu_alag/100),
      oalagII = manadar_laun*sum(oalagII*0.55*prosentu_alag/100)
    ) |>
  dplyr::rowwise() |>
  dplyr::mutate(alls=sum(sjopeningar,oalagI,oalagII))
}
