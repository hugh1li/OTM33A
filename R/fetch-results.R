#' Fetch final results from data
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return Vector of named results, suitable for storing in a data.table
#' @export
#' @examples
#' fetch.results(dat)
fetch.results <- function(dat) {
  x <- NULL
  try(x <- round(c(PSG=attr(dat,"PSG"),
           PGI=trunc(attr(dat,"PGI")),
           Ubar = attr(dat,"Ubar"),
           distance=attr(dat,"distance"),
           CH4.bg=attr(dat,"CH4.bg"),
           wd.rot=as.numeric(attr(dat,"wd.rot")),
           Ly.rot =as.numeric(attr(dat,"Ly.rot")),
           Ly.peak = as.numeric(attr(dat,"Ly.peak"))),2))
  x=as.list(x)
  x$ID=attr(dat,"file.name")
  return(x)
}
