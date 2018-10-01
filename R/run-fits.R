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
  fit1 <-  calcPSG.bin(dat,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
  fit2 <- calcLyProjection.bin(dat,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
  calcEmissionsRate(dat)
  retval=fetch.results(dat)
  return(retval)
}
