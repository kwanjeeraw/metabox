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

stat_Study_Design = function(DATA, between_factor, within_factor){
  eData = DATA$expression
  fData = DATA$feature
  pData = DATA$phenotype
  if(length(between_factor)==0){
    Sample_Size = table(pData[,within_factor])
  }else if(length(within_factor)==0){
    Sample_Size = table(pData[,between_factor])
  }else if((length(within_factor)+length(between_factor))<4){
    Sample_Size = table(pData[,c(between_factor,within_factor)])
  }else{
    Sample_Size = "Too many factors you've selected. You can only select three factors in total."
  }


  result = Sample_Size
  return(result)
}
