#' Reiknar sjópeninga útfrá spönn daga og fleiri þáttum.
#'
#' Reiknar sjópeninga útfrá spönn daga, helgum og helgidögum skv sjókjarasamning Hafró.
#' Stuðst er við fallið 'holiday' í pakkanum 'timeDate'.
#
#' @param upphaf, endir Upphafs og lokadagur leiðangurs sem ISO-dagsetningar strengur (YYYY-MM-DD).
#' @param  fjarvist_hefst Dagur í leiðangri sem fjarvistaruppbót byrjar, þriðji dagur ef ekkert hefur verið róið
#' @param  leidangursstjorn Leiðangurstjóraálag, sjálgefið 0, 0.5 fyrir 1-2 leiðangurmenn, 1 fyrir 4 og fleiri.
#' @param  leiguskip Er róið á leiguskipi og greitt álag? Sjálgefið 0, en 1/2 tími ef um það er að ræða. 
#' @details Yfirvinna er greidd með tímakaupi. Tímakaup fyrir yfirvinnu í hverjum launaflokki 
#' er 1,0385% af mánaðarlaunum starfsmann.
#'
#' Öll vinna sem unnin er á stórhátíðum skv. gr. 2.1.4.3 greiðist með tímakaupi 
#' sem nemur 1,375% af mánaðarlaunum starfsmanns.
#'
#' Sjópeningar - virkir dagar: 4,92 fyrstu tvo dagana og síðan 5,92 með fjarvistaruppbót.
#'
#' Sjópeningar - helgar: 12,92 fyrstu tvo dagana og síðan 13,92 með fjarvistaruppbót.
#'
#' Fjarvistaruppbót er 1 klst. pr. dag frá og með 3. degi samfelldrar fjarvistar,
#' eða frá og með 6. fjarvistardegi í hverjum almanaksmánuði.
#'
#' Óþægindaálag I (33%) - 3 klst. á dag mánudaga til fimmtudaga.
#'
#' Óþægindaálag II (55%) - 3 klst. á dag á föstudögum.
#'
#' Yfirvinna fyrir leiðangursstjóra í ferðum með þremur eða fleiri er 1 klst á dag
#' (stórhát.kaup ef rauður dagur).
#' Yfirvinna fyrir leiðangursstjóra í ferðum með færri en þremur er 0,5 klst á dag
#' (stórhát.kaup ef rauður dagur).
#' 
#' Ennfremur er litið svo á að allt sem greitt er með yfirvinnutímum
#' sé á stórhátíðarálgi ef þannig ber undir (óþægindaálag og fjarvistaruppbót auk Ó-álaganna.
#'
#' Yfirvinna fyrir að vera á öðrum skipum en Hafró er 0,5 klst. á dag
#' 
#' Ef sérstakur frídagur fellur á mánudag til föstudags eru greiddir sjópeningar
#' eins og það væri helgi (12,92 eða 13,92 - Ath. ekkert óþ.álag.).
#'
#' Ef stórhátíðardagur fellur á helgi skal greiða sjópeningana (12,92 eða 13,92)
#' með stórhátíðarkaupi.

#' @references https://www.fin.is/media/samningar/Kjarasamningur-FIN-og-rikis-28.februar-2018.pdf
#' @return Nokkuð stór tafla með forsendum sjópengingareiknings. 
#' @export

sjopeng <- function(upphaf, endir, leidangursstjorn=0, fjarvist_hefst=3, leiguskip=0)
{
  Sys.getlocale("LC_TIME") -> oldLC_TIME
  Sys.setlocale("LC_TIME","is_IS.UTF-8")

  if(!(leidangursstjorn %in% c(0,0.5,1))) stop("ógilt leiðangursstjóraálag")
  if(!(fjarvist_hefst %in% 1:3)) stop("fjarvistaruppbót byrjar eigi síðar en á 3. degi")
  if(!(leiguskip %in% c(0,0.5))) 
    stop("Ef um álag vegna leiðangurs á leiguskipi er að ræða skal það vera 0.5")

  yyyy <- year(as.Date(d1))

  daga_sponn <- seq(as.Date(d1), as.Date(d2), by = "1 day")
  vikudagur <- as.POSIXlt(daga_sponn)$wday
  man_fim <- ifelse(vikudagur %in% 1:4, 1, 0)
  fos <- ifelse(vikudagur == 5, 1, 0)
  helgi <- ifelse(vikudagur == 0 | vikudagur == 6, 1, 0)

  sjodagar <- tibble(
    dags = daga_sponn,
    vikudagur = weekdays(dags,abbreviate=TRUE),
    man_fim = man_fim,
    fos = fos,
    helgi = helgi,
    fjarvist = c(rep(0,n1-1),rep(1,length(daga_sponn)-n1+1))
  )

  Sys.setlocale("LC_TIME",oldLC_TIME)

  stor <- tibble::tibble(
    hatidisd=storhatidir(yyyy),
    stor=1
  )
  spes <- tibble::tibble(
    serstakurd = serstakir(yyyy),
    spes = 1
  )

  sjodagar |>
    left_join(stor,join_by(dags == hatidisd)) |>
    mutate(stor = ifelse(is.na(stor), 0, stor)) |>
    left_join(spes, join_by(dags == serstakurd)) |>
    mutate(spes = ifelse(is.na(spes), 0, spes)) |>
    mutate(man_fim = ifelse(stor|spes, 0, man_fim),
      fos = ifelse(stor | spes,0,fos),
      helgi = ifelse(stor | spes, 0, helgi)) |>
    mutate(sjopeningar= 4.92*(man_fim + fos) + 12.92*(helgi + spes + stor) +
        fjarvist + leiguskip + leidangursstjorn,
      oalagI = 3*man_fim,
      oalagII = 3*fos,
      prosentu_alag=ifelse(stor,1.375,1.0385)
}
