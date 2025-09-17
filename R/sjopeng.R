sjopeng <- function(d1,d2,leiguskip=NULL,stada=0,n1=3,spes=NULL,stor=NULL)
{

  if(!(status %in% c(0,0.5,1))) stop("ógilt leiðangursstjóraálag")
  if(!(n1 %in% 1:3)) stop("fjarvistaruppbót byrjar eigi síðar en á 3. degi")

  yyyy <- year(as.Date(d1))

  daga_sponn <- seq(as.Date(d1),as.Date(d2),by="1 day")
  vikudagur <- as.POSIXlt(daga_sponn)$wday
  man_fim <- ifelse(vikudagur %in% 1:4,1,0)
  fos <- ifelse(vikudagur==5,1,0)
  helgi <- ifelse(vikudagur==0|vikudagur==6,1,0)

  sjodagar <- tibble(
    dags=daga_sponn,
    vikudagur=vikudagur,
    man_fim=man_fim,
    fos=fos,
    helgi=helgi,
    fjarvist=c(rep(0,n1-1),rep(1,length(daga_sponn)-n1+1))
  )

  stor <- storhatidir(yyyy)
  spes <- serstatkir(yyyy)

  sjodagar |>
    left_join(stor,join_by(dags==hatidisd)) |>
    mutate(stor=ifelse(is.na(stor),0,stor)) |>
    left_join(spes,join_by(dags==serstakurd)) |>
    mutate(spes=ifelse(is.na(spes),0,spes)) |>
    mutate(man_fim=ifelse(stor|spes,0,man_fim),
      fos=ifelse(stor|spes,0,fos))
}
