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
  if(!is.null(attr(dat,"align.time"))) {
    dat[,row.id := 1:.N]
    mintime <- dat[1,DateTime]+delay
    dat.delay = dat[DateTime > mintime, list(row.id,CH4)]
    dat.delay[,row.id := row.id-min(row.id)+1]
    setnames(dat,"CH4","CH4.nta")
    setkey(dat.delay,row.id)
    setkey(dat,row.id)
    dat[dat.delay,CH4:=i.CH4]
    dat[, sub := ifelse(row.id<1,FALSE,sub)]
    dat[, sub := ifelse(is.na(CH4),FALSE,sub)]
    setattr(dat,"align.time",delay)
  }
}

