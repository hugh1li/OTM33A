#' Read data from CSv file and return data.table
#' @param dat Data table, with names obtained from STR data output from OTM3A Appendicies
#' @return data.table containing data contained in file.name, with headers and attributes read from the file
#' @export
#' @examples
#' read.STR.xls(file.name)

read.STR.xls <- function(file.name) {
  # save mast heading
  # header = data.table(read.table(file.name,skip=0,nrows =numskip-2,sep="\t"))
  # setnames(header,names(header),c("Name","Value"))
  rawdat <- readxl::read_excel(file.name)
  rawdat <- data.table(rawdat)
  nm <- names(rawdat)
  nm <- stringr::str_replace_all(nm," ",".")
  nm <- stringr::str_replace_all(nm,"3DS","X3DS")
  setnames(rawdat,names(rawdat),nm)
  setnames(rawdat,"Time","DateTime")
  # DateTime = as.POSIXct(strptime(as.character(rawdat$Time),format = "%Y-%m-%d %H:%M:%S"))
  # rawdat[,DateTime := DateTime]
  # Only keep rows with a timestamp (removes extra rows at end of file)
  rawdat[,sub := !is.na(DateTime)]
  # setattr(rawdat,"distance",header[Name=="Distance to Source",as.numeric(as.character(Value))])
  setattr(rawdat,"file.name",file.name)
  rawdat
}
