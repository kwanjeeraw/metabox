#'Read a file
#'@description read a file overriding read.csv for GUI
#'@usage readInfile(file, header = TRUE, sep ="\t")
#'@param file
#'@param header
#'@param set
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{read.csv}}
#'@examples
#'# Set database location to local server
#'# readInFile("http://localhost:7474/db/data/cypher")
#'@export
readInfile <- function(file, header=TRUE, sep="\t") UseMethod("readInfile")
#'@export
readInfile.default <- function(file, header=TRUE, sep="\t"){
  read.csv(file, header = header, sep = sep, stringsAsFactors = FALSE)
}
