#' Rename data columns from read.OTM33A for subsequent processing
#' @param dat Data table
#' @export
#' @examples
#' dat=rename.OTM33A(dat)

rename.STR <- function(dat) {
  setnames(dat,"WS.Bar..Pressure","Pressure")
  setnames(dat,"X3DS.Sonic.Temp","Temp")
  setnames(dat,"WS.Wind.Direction","wd2")
  setnames(dat,"WS.Temperature","Temp.Alt")
}
