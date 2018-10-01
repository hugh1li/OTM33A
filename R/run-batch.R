#' run batch files for a data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' run.batch(dat)

run.batch <- function(dat,wdfilt=60,binwidth=10) {
  subtract.background(dat)
  align.time(dat,4)
  rotateWindDirection(dat)
  fit1 <-  calcPSG.bin(dat,wdfilt=wdfilt,binwidth=binwidth)
  fit2 <- calcLyProjection.bin(dat,wdfilt=wdfilt,binwidth=binwidth)
  calcEmissionsRate(dat)
  retval=fetch.results(dat)
  return(retval)
}
