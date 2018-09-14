#' Calculate emissions rates from data.table and gaussing peak value estimated from calcLyProjection
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#' gauss.peak:  Peak of Gaussian fit from calcLyProjection
#' @return list containing information from emissions rate calculation
#' @keywords
#' @export
#' @examples
#' read.OTM33A(file.name,numskip=33)

calcEmissionsRate <- function(dat,gauss.peak) {
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
  Tbar <- dat[theta.filter==TRUE,mean(X3DS.Sonic.Temp)]+273.15
  Pbar <- dat[theta.filter==TRUE,mean(AirMar.Barometer..mBar.)]
  Ubar <- dat[theta.filter==TRUE,mean(ws3)]
  ws_sd <- dat[theta.filter==TRUE,sd(ws3)]

  ep <- sqrt(1-(mean(sin(dat[[wd2_raw]]*pi/180))^2+mean(cos(dat[[wd2_raw]]*pi/180))^2))
  wd_sd <- asin(ep)*(1 + (2/sqrt(3) - 1)*ep^3) * 180/pi # Yamartino Method
  turbint <- dat[theta.filter==TRUE,sd(X3DS.W)]/Ubar

  # PGI from turbulence, turbint.breaks imported from data(pgsigma)
  PGturbi <- as.numeric(as.character((cut(turbint, turbint.breaks, labels=rev(seq(1,7,1))))))
  # PGI from wd sd, wdsd.breaks imported from data(pgsigma)
  PGstddevi <- as.numeric(as.character(cut(wd_sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  # Calculate average PGI, round up if 0.5
  PGI <- round((PGstddevi + PGturbi)/2 + 0.0001)
  dist.int <- round(attr(dat,"distance"))

  # pgsigma imported from pgsigma.Rdata
  pgsigmay <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                        pgsigma$PGI == PGI), "sigmay"])
  pgsigmaz <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                       pgsigma$PGI == PGI), "sigmaz"])

  # Unit conversion constants
  rt1rp1 <- 298/1013.25; gasconst <- 8.314510; mw <- 16.04;
  rt0rp0 <- (gasconst*298)/101.325; opt <- (1e-06 * mw*1000)/rt0rp0;
  # Convert gauss.peak to g/m3
  a1_gperm3 <- gauss.peak*opt*rt1rp1*Pbar/Tbar
  # Calculate PSG
  PSG <- 2*pi*a1_gperm3*Ubar*pgsigmay*pgsigmaz
  return(c(PSG=as.numeric(PSG),PGI=as.numeric(PGI),Ubar=as.numeric(Ubar)))
}
