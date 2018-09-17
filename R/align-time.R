#' Align sonic anenometer and concentration measurements in time by offsetting concentration measurements by a fixed delay, in seconds
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' delay:  Numeric, number of seconds in the delay
#' @return None, but has the side effect of adding column CH4.nta (not time aligned) to dat
#'  overwrites CH4 with delayed signal
#' @export
#' @examples
#' align.time(dat, 40) # 40 second time delay

align.time <- function(dat, delay) {
   dat.delay <- dat[,list(DateTime,CH4)]
   dat.delay[, DateTime := DateTime - delay]
   setnames(dat,"CH4","CH4.nta")
   setkey(dat.delay,DateTime)
   setkey(dat,DateTime)
   dat[dat.delay,CH4:=i.CH4]
}

