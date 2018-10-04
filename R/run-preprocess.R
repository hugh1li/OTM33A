#' Preprocess OTM33A data set prior to Gaussian fits
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' dat <- run.preprocess(dat)

run.preprocess <- function(dat) {
  dat <- subtract.background(dat)
  dat <- align.time(dat,4)
  dat <- rotateWindDirection(dat)
  return(dat)
}
