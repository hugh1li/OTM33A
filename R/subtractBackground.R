#' Calculate background as the average of the lowest 5% of data from data set
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return None, but has the side effect of adding column CH4 and attribute bg to dat
#' @keywords
#' @export
#' @examples
#' subtract.background(dat)

subtract.background <- function(dat) {
  #ch4_bg <- mean(dat$ch4[which(dat$ch4 < quantile(dat$ch4, 0.05))])
  ch4_bg <- dat[order(CH4)][1:(.N*0.05),mean(CH4)]
  setnames(dat,'CH4','CH4.raw')
  dat[,CH4 := CH4.raw -ch4_bg]
  setattr(dat,"CH4.bg",ch4_bg)
}
