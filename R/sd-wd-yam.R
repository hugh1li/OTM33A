#' Calculate standard deviation of wind direction using Yamartino Method
#' @param
#' dat:  Wind direction,
#' @return Numeric, value of standard deviation
#' @export
#' @examples
#' wd.sd <- sd.wd.yam(wd)

sd.wd.yam <- function(wd) {
  ep <- sqrt(1-(mean(sin(wd*pi/180))^2+mean(cos(wd*pi/180))^2))
  wd.sd <- asin(ep)*(1 + (2/sqrt(3) - 1)*ep^3) * 180/pi # Yamartino Method
  wd.sd
}
