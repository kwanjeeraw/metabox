#'Read a file
#'@description read a file in table format by overriding read.csv for using in opencpu app.
#'@usage readInfile(file, header, sep, rowname)
#'@param file a string specifying name of the file
#'@param header a logical value specifying whether the first line is the names of variables. Default = TRUE
#'@param sep a string specifying a field separator character. Default = "\t"
#'@param rowname a column number or column name specifying the column of the table that contains the row names.
#'@return data frame of the table
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{read.table}}
#'@examples
#'# readInFile("location")
#'@export
readInfile <- function(file, header=TRUE, sep="\t", rowname=NULL) UseMethod("readInfile")
#'@export
readInfile.default <- function(file, header=TRUE, sep="\t", rowname=NULL){
  inpfile = read.csv(file, header = header, sep = sep, row.names = rowname, stringsAsFactors = FALSE)
  inpfile[is.na(inpfile)] = ""
  inpfile
}
