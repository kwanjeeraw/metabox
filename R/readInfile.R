#'Read a file
#'@description read a file overriding read.csv for GUI
#'@usage readInfile(file, header, sep, rowname)
#'@param file
#'@param header
#'@param sep
#'@param rowname
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{read.csv}}
#'@examples
#'# readInFile("location")
#'@export
readInfile <- function(file, header=TRUE, sep="\t", rowname=NULL) UseMethod("readInfile")
#'@export
readInfile.default <- function(file, header=TRUE, sep="\t", rowname=NULL){
  read.csv(file, header = header, sep = sep, row.names = rowname, stringsAsFactors = FALSE)
}
