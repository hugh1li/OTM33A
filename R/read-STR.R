#' Read data from CSv file and return data.table
#' @param dat Data table, with names obtained from STR data output from OTM3A Appendicies
#' @return data.table containing data contained in file.name, with headers and attributes read from the file
#' @export
#' @examples
#' read.STR(file.name)

read.STR <- function(file.name) {
  # save mast heading
#  header = data.table(read.table(file.name,skip=0,nrows =numskip-2,sep="\t"))
#  setnames(header,names(header),c("Name","Value"))
  rawdat <- readxl::read_excel(file.name)
  nm <- names(rawdat)
  nm <- stringr::str_replace_all(nm," ",".")
  rawdat <- data.table(rawdat)
  setnames(rawdat,names(rawdat),nm)
  DateTime = as.POSIXct(strptime(as.character(rawdat$Time),format = "%H:%M:%S"))
  rawdat[,DateTime := DateTime]
    # Only keep rows with a timestamp (removes extra rows at end of file)
  rawdat[,sub := !is.na(rawdat$Time)]
#  setattr(rawdat,"distance",header[Name=="Distance to Source",as.numeric(as.character(Value))])
#  setattr(rawdat,"heading",header[Name=="Mast Heading",as.numeric(as.character(Value))])
  rawdat
}
