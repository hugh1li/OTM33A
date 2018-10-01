sd.wd3.yam <- function(wd3_raw) {
  ep <- sqrt(1-(mean(sin(wd3_raw*pi/180))^2+mean(cos(wd3_raw*pi/180))^2))
  wd_sd <- asin(ep)*(1 + (2/sqrt(3) - 1)*ep^3) * 180/pi # Yamartino Method
  wd_sd
}
