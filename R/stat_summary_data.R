#'Summary Data
#'@description Summary Data
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

stat_summary_data = function(DATA){
  eData = DATA$expression
  fData = DATA$feature
  pData = DATA$phenotype

  type_of_each_colum_pData = sapply(pData, function(x){class(x)})

  pData_columns_num = sapply(pData, function(x){length(unique(x))})
  fData_columns_num = sapply(fData, function(x){length(unique(x))})

  why_not_able = vector()
  for(i in 1:ncol(pData)){
    why_not_able[i] = paste(ifelse(type_of_each_colum_pData[i]=="numeric","numeric",""),ifelse(pData_columns_num[i]>(nrow(eData)/3),"too_many_levels",""))
  }



  result = list(number_of_sample=nrow(pData),number_of_feature = nrow(fData),column_names_of_pData = colnames(pData),column_names_of_fData = colnames(fData),
                ncol_of_p = ncol(pData), ncol_of_f = ncol(fData),
                type_of_each_colum_pData = type_of_each_colum_pData, pData_columns_num = pData_columns_num,fData_columns_num=fData_columns_num,
                why_not_able = why_not_able)
  return(result)
}
