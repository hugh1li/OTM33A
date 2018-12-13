#' run batch files for a data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' run.fits(dat)

run.fits <- function(dat,Analyte="CH4",wdfilt=60,binwidth=10,min.n=0) {
  tryCatch({
  dat <-  calcPSG.bin(dat,Analyte=Analyte,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcLyProjection.bin(dat,Analyte=Analyte,thetafilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcPlume(dat)
  dat <- calcEmissionsRate(dat,Analyte=Analyte)
#  retval=fetch.results(dat)
  return(dat)})
}
