#' Read data from text file and return data.table
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return data.table containing data contained in file.name, with headers and attributes read from the file
#' @keywords
#' @export
#' @examples
#' read.OTM33A(file.name,numskip=33)

read.OTM33A <- function(file.name, numskip=33) {
  # save mast heading
  header = data.table(read.table(file.name,skip=0,nrows =numskip-2,sep="\t"))
  rawdat <- data.table(read.table(file.name, header=T, sep="\t",skip=numskip))
  DateTime = as.POSIXct(strptime(as.character(rawdat$Time),format = "%m/%d/%y %H:%M:%S"))
  rawdat[,DateTime := DateTime]
    # Only keep rows with a timestamp (removes extra rows at end of file)
  rawdat[,sub := !is.na(rawdat$Time)]
  setattr(rawdat,"distance",header[V1=="Distance to Source",as.numeric(as.character(V2))])
  rawdat
}
