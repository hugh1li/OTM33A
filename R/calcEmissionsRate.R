#' Calculate emissions rate for Analyte from data.table
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return data.table with attributes set containing emissions calculations
#' @export
#' @examples
#' dat = calcEmissionsRate(dat,Analyte="CH4")

calcEmissionsRate <- function(dat,Analyte="CH4") {
  PGI = attr(dat,"PGI")
  Ubar = attr(dat,"Ubar")
  Pbar = attr(dat,"Pbar")
  Tbar = attr(dat,"Tbar")
  pgsigmay = attr(dat,"pgsigmay")
  pgsigmaz = attr(dat,"pgsigmaz")
  rt1rp1 <- 298/1013.25; gasconst <- 8.314510; mw <- 16.04;
  rt0rp0 <- (gasconst*298)/101.325; opt <- (1e-06 * mw*1000)/rt0rp0;
  # Convert Ly.peak to g/m3
  Ly.peak=attr(dat,paste(Analyte,"Ly.peak",sep="."))
  Ly.peak.g.per.m3 <- Ly.peak*opt*rt1rp1*Pbar/Tbar
  # Calculate PSG
  PSG <- as.numeric(2*pi*Ly.peak.g.per.m3*Ubar*pgsigmay*pgsigmaz)
  setattr(dat,paste(Analyte,"PSG",sep="."),as.numeric(PSG))
  setattr(dat,paste(Analyte,"Ly.peak.g.per.m3",sep="."),as.numeric(Ly.peak.g.per.m3))
  return(dat)
}
