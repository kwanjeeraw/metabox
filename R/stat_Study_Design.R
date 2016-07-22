#'Study_Design
#'@description Study_Design
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

stat_Study_Design = function(DATA, between_factor = NULL, within_factor = NULL){
  eData = DATA$expression
  fData = DATA$feature
  pData = DATA$phenotype
  if(length(within_factor) == 0){
    within_factor = NULL
  }
  if(length(between_factor) == 0){
    between_factor = NULL
  }




  Sample_Size = tryCatch(table(pData[,c(between_factor,within_factor)]),
                         error = function(err){
                           "Waiting user to select factor."
                         })


  result = Sample_Size
  return(result)
}
