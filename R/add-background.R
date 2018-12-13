#' Calculate background as the average of the lowest five percent of data from data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return None, but has the side effect of adding column CH4 and attribute bg to dat
#' @export
#' @examples
#' dat = add.background(dat)

add.background <- function(dat,Analyte="CH4") {
  bg.name = paste(Analyte,".bg",sep="")
  bg = attr(dat,bg.name)
  if(!is.null(bg)) {
    setnames(dat,Analyte,"Analyte")
    dat[sub==TRUE,Analyte := Analyte + bg]
    setattr(dat,bg.name,NULL)
    setnames(dat,"Analyte",Analyte)
  }
  dat
}
