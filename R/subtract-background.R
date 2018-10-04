#' Calculate background as the average of the lowest five percent of data from data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return None, but has the side effect of adding column CH4 and attribute bg to dat
#' @export
#' @examples
#' subtract.background(dat)

subtract.background <- function(dat) {
  if(is.null(attr(dat,"CH4.bg"))) {
    ch4_bg <- dat[CH4<quantile(CH4,probs=.05)&sub==TRUE,mean(CH4)]
    setnames(dat,'CH4','CH4.nbg')
    dat[sub==TRUE,CH4 := CH4.nbg -ch4_bg]
    setattr(dat,"CH4.bg",ch4_bg)
  }
  dat
}
