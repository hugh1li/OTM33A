#' Calculate background as the average of the lowest five percent of data from data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return None, but has the side effect of adding column CH4 and attribute bg to dat
#' @export
#' @examples
#' subtract.background(dat)

subtract.background <- function(dat,Analyte="CH4") {
  bg.name = paste(Analyte,".bg",sep="")
  if(is.null(attr(dat,bg.name))) {
    setnames(dat,Analyte,"Analyte")
    bg <- dat[Analyte<quantile(Analyte,probs=.05)&sub==TRUE,mean(Analyte)]
    dat[sub==TRUE,Analyte := Analyte - bg]
    setattr(dat,bg.name,bg)
    setnames(dat,"Analyte",Analyte)
  }
  dat
}
