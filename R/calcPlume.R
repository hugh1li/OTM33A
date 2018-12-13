#' Calculate plume characteristics from data.table
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return data.table with attributes set containing plume calculations
#' @export
#' @examples
#' dat = calcPlume(dat)

calcPlume <- function(dat) {
  # Input:  dat:  data set
  # Output:  calculated plume characteristics

  # Constants from Gryning et al 1983

  # next statement imports psigma, turbint.breaks,wdsd.breaks
  data("pgsigma")

#  pgsigma=get(z[1])
#  turbint.breaks=get(z[2])
#  wdsd.breaks=get(z[3])

  # the following names depend upon renaming (i.e. rename.OTM33A)
  Tbar <- dat[sub==TRUE & theta.filter==TRUE,mean(Temp)]+273.15
  Pbar <- dat[sub==TRUE & theta.filter==TRUE,mean(Pressure)]
  Ubar <- dat[sub==TRUE & theta.filter==TRUE,mean(ws3)]
  ws.sd <- dat[sub==TRUE & theta.filter==TRUE,sd(ws3)]

  wd3.sd <- dat[sub==TRUE & theta.filter==TRUE,sd.wd.yam(wd3)]
  wd2.sd <- dat[sub==TRUE & theta.filter==TRUE,sd.wd.yam(wd2)]
  turbint <- dat[sub==TRUE&theta.filter==TRUE,sd(ws3w)]/Ubar

  # PGI from turbulence, turbint.breaks imported from data(pgsigma)
  PGturbi <- as.numeric(as.character((cut(turbint, turbint.breaks, labels=rev(seq(1,7,1))))))
  # PGI from wd3_sd, wdsd.breaks imported from data(pgsigma)
  PG.sd.3 <- as.numeric(as.character(cut(wd3.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  PG.sd.2 <- as.numeric(as.character(cut(wd2.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  # Calculate average PGI, round up if 0.5
  PGI <- round((PG.sd.3 + PGturbi)/2 + 0.0001)
  dist.int <- round(attr(dat,"distance"))
  # pgsigma imported from pgsigma.Rdata
  pgsigmay <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                        pgsigma$PGI == PGI), "sigmay"])
  pgsigmaz <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                       pgsigma$PGI == PGI), "sigmaz"])

  setattr(dat,"PGI",as.numeric(PGI))
  setattr(dat,"PG.sd.3",as.numeric(PG.sd.3))
  setattr(dat,"PG.sd,2",as.numeric(PG.sd.2))
  setattr(dat,"PG.t",as.numeric(PGturbi))
  setattr(dat,"Ubar",as.numeric(Ubar))
  setattr(dat,"Pbar",as.numeric(Pbar))
  setattr(dat,"Tbar",as.numeric(Tbar))
  setattr(dat,"turbint",as.numeric(turbint))
  setattr(dat,"pgsigmay",as.numeric(pgsigmay))
  setattr(dat,"pgsigmaz",as.numeric(pgsigmaz))
  setattr(dat,"PG.ti",as.numeric(PGturbi))
  return(dat)
}
