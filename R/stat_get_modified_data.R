#'stat_get_modified_data
#'@description stat_get_modified_data
#'@usage load_aggregated_data(file, type, ...)
#'@param file the file in read.csv or read.xlsx2.
#'@param type a string of file name ended either with .xlsx or .csv.
#'@param ... Additional arguments for xlsx::read.xlsx2 or read.csv.
#'@details
#'
#'@return a list of three data frames: "expression"(sample in row), "feature"(compoud in row) and "phenotype"(sample in row).
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso \code{\link{load_expression_data}}, \code{\link{load_expression_data}}, \code{\link{load_expression_data}}
#'@examples
#'load_aggregated_data(input$inputID,startRow=2)
#'@export

stat_get_modified_data = function(DATA){
  pData = DATA$phenotype
  eData = DATA$expression
  fData = DATA$feature

  result = matrix(nrow = ncol(pData)+ncol(eData) + 1, ncol = ncol(fData)+nrow(eData))
  result[(ncol(pData)+2):nrow(result),1:ncol(fData)] = sapply(fData, as.character)
  result[(ncol(pData)+1),1:ncol(fData)] = colnames(fData)
  result[1:ncol(pData),(ncol(fData)+1):ncol(result)] = t(sapply(pData, as.character))
  result[1:ncol(pData),ncol(fData)] = colnames(pData)
  result[(ncol(pData)+2):nrow(result),(ncol(fData)+1):ncol(result)] = t(sapply(eData,as.numeric))
  result = data.frame(result,stringsAsFactors = FALSE, check.names = FALSE)
  return(result)
}
