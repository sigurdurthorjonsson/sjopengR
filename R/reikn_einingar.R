#' Reiknar sjópeningaeiningar útfrá forsendum í töflu.
#'
#' Reiknar sjópeningaeiningar útfrá forsendum í töflu, í einingum 
#' yfirvinnutíma skipt á launaliðina sjópeninga, óþægindaálag I og óþægindalág II.
#
#' @param tafla Sjópeningatafla, úttak fallsins 'sjopengR::sjopeng'.
#' @return Yfirlit sjópeningaeininga skipt á launaliði.
#' @references https://www.fin.is/media/samningar/Kjarasamningur-FIN-og-rikis-28.februar-2018.pdf,
#'   https://www.fin.is/media/samningar/Hafro-2017.pdf
#' @export

reikn_einingar <- function(tafla)
{
  tafla |>
    select(dags,sjopeningar:oalagII) |>
    summarize(across(sjopeningar:oalagII,sum)) |>
    rename(
      `Sjópeningar`=sjopeningar,
      `Óþægindaálag I`=oalagI,
      `Óþægindaálag II`=oalagII
    )
}
