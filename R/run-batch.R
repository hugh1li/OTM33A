#' run batch files for a data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' run.batch(dat)

run.batch <- function(dat,wdfilt=60,binwidth=10,min.n=0.01) {
  dat <- subtract.background(dat)
  dat <- align.time(dat,4)
  dat <- rotateWindDirection(dat)
  dat <- calcPSG.bin(dat,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcLyProjection.bin(dat,thetafilt=wdfilt,binwidth=binwidth,min.n=min.n)
  dat <- calcEmissionsRate(dat)
#  retval=fetch.results(dat)
  return(dat)
}
