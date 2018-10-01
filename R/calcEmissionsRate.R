#' Calculate emissions rates from data.table and gaussian peak value estimated from calcLyProjection
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' gauss.peak:  Peak of Gaussian fit from calcLyProjection
#' @return vector of length 3 containing information from emissions rate calculation
#' @export
#' @examples
#' calcEmissionsRate(dat)

calcEmissionsRate <- function(dat) {
  # check to make sure that filter from calcLY is applied at this step
  # Input:  dat:  data set,
  #         gauss.peak:  gaussian value at plume center estimated by calcLyProjection
  # Output:  calculated emissions in g/m^3

  # Constants from Gryning et al 1983

  # next statement imports psigma, turbint.breaks,wdsd.breaks
  data("pgsigma")

#  pgsigma=get(z[1])
#  turbint.breaks=get(z[2])
#  wdsd.breaks=get(z[3])

#  return(pgsigma)

  # To do:  figure out a better way to select appropriate columns from data table but still apply filter
  # to do:  is it best to calculate values below using theta.filter?
  # wd2_raw <- "X3DS.Azimuth"
  # temperature <- "X3DS.Sonic.Temp"
  # pressure <- "AirMar.Barometer..mBar."
  # ws3w <- "X3DS.W"
  Tbar <- dat[sub==TRUE & theta.filter==TRUE,mean(X3DS.Sonic.Temp)]+273.15
  Pbar <- dat[sub==TRUE & theta.filter==TRUE,mean(WS.Bar..Pressure)]
  Ubar <- dat[sub==TRUE & theta.filter==TRUE,mean(ws3)]
  ws.sd <- dat[sub==TRUE & theta.filter==TRUE,sd(ws3)]

  wd3.sd <- dat[sub==TRUE & theta.filter==TRUE,sd.wd3.yam(X3DS.Aziumth)]
  wd2.sd <- dat[sub==TRUE & theta.filter==TRUE,sd.wd3.yam(WS.Wind.Direction)]
  turbint <- dat[sub==TRUE&theta.filter==TRUE,sd(ws3w)]/Ubar

  # PGI from turbulence, turbint.breaks imported from data(pgsigma)
  PGturbi <- as.numeric(as.character((cut(turbint, turbint.breaks, labels=rev(seq(1,7,1))))))
  # PGI from wd3_sd, wdsd.breaks imported from data(pgsigma)
  PG.sd3 <- as.numeric(as.character(cut(wd3.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  PG.sd.2 <- as.numeric(as.character(cut(wd2.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  # Calculate average PGI, round up if 0.5
  PGI <- round((PG.sd.3 + PGturbi)/2 + 0.0001)
  dist.int <- round(attr(dat,"distance"))
  # pgsigma imported from pgsigma.Rdata
  pgsigmay <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                        pgsigma$PGI == PGI), "sigmay"])
  pgsigmaz <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                       pgsigma$PGI == PGI), "sigmaz"])

  # Unit conversion constants
  rt1rp1 <- 298/1013.25; gasconst <- 8.314510; mw <- 16.04;
  rt0rp0 <- (gasconst*298)/101.325; opt <- (1e-06 * mw*1000)/rt0rp0;
  # Convert Ly.peak to g/m3
  Ly.peak=attr(dat,"Ly.peak")
  a1_gperm3 <- Ly.peak*opt*rt1rp1*Pbar/Tbar
  # Calculate PSG
  PSG <- as.numeric(2*pi*a1_gperm3*Ubar*pgsigmay*pgsigmaz)
  setattr(dat,"PSG",PSG)
  setattr(dat,"PGI",PGI)
  setattr(dat,"PG.sd.3",PG.sd.3)
  setattr(dat,"PG.sd,2",PG.sd.2)
  setattr(dat,"PG.t",PGturbi)
  setattr(dat,"Ubar",Ubar)
  setattr(dat,"turbint",turbint)
  setattr(dat,"pgsigmay",pgsigmay)
  setattr(dat,"pgsigmaz",pgsigmaz)
  setattr(dat,"PG.ti",PGturbi)
  return(PSG)
}
