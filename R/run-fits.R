#' run batch files for a data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' run.fits(dat)

run.fits <- function(dat,wdfilt=60,binwidth=10,min.n=0) {
  dat <-  calcPSG.bin(dat,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcLyProjection.bin(dat,thetafilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcEmissionsRate(dat)
#  retval=fetch.results(dat)
  return(dat)
}
