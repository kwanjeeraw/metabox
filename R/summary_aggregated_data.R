#'summarize aggregated data.
#'@description summarize aggregated data.
#' Summarize the information that is important for mETABOX statistical analysis, e.g. factor_name, repeated.factor_name, etc.
#'@usage
#'summary_aggregated_data(aggregated_data, factor_name, repeated.factor_name)
#'@param aggregated_data an list with three dataframes named as "expression", "feature" and "phenotype". Or simply a object returned from
#'load_aggregated_data().
#'@param factor_name The factor names that are related to the study design.
#'factor_name must be one of the column name of the "phenotype" dataframe. Although the default is NULL, it shouldn't be NULL for mETABOX
#'statistical analysis.
#'@param repeated.factor_name The repeated factor name indicates which factor is repeated measure. Must be one of the factor_name.
#'If NULL means that it is independed study design.
#'@details
#'
#'@return
#'a list. If the input aggregated_data is not standard, then it returns a warning message telling user to upload needed file.
#'dataset(list), factor_name(vector of string), repeated.factor_name(vector of string), confound(vector which is a variable),
#'batch(vector which is a variable of factor)
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
### Summarize dataset.
summary_aggregated_data <- function(e=NULL,f=NULL,p=NULL,
                                    factor_name = NULL,repeated_factor_name = NULL,confound = NULL,batch=NULL){
  result <- list()
  if(is.null(e)|is.null(f)|is.null(p)){
    result[["warnings"]] = paste("Waiting Users To Upload",
                                 "Expression Dataset,"[is.null(e)],
                                 "Feature Datasets,"[is.null(f)], "Phenotype Datasets."[is.null(p)])
  }else{
    #The aggregated_data is a list containing first the expression data, then feature data and then the phenotype data.
    # e <- aggregated_data[["expression"]]; f<-aggregated_data[["feature"]]; p<-aggregated_data[["phenotype"]]
    ## First check if the dimension is correct. If it is not correct should return a message to let the user know.
    if(!nrow(e)==nrow(p)){
      result[["warnings"]][[length(result[["warnings"]])+1]] = paste0(length(result[["warnings"]])+1,
                                                                      ". The sample size doesn't match between expression dataset and phenotype dataset.")
    }
    if(!ncol(e)==nrow(f)){
      result[["warnings"]][[length(result[["warnings"]])+1]] = paste0(length(result[["warnings"]])+1,
                                                                      ". The number of compounds doesn't match between expression dataset and feature dataset.")
    }


    if(is.null(result[["warnings"]])){#If there is no warnings so we can proceed.



      result[["dataset"]] = list("expression" = e, "feature" = f, "phenotype" = p)


      result[["factor_name"]] = ifelse(is.null(factor_name),"none",factor_name)
      result[["repeated_factor_name"]] = ifelse(is.null(repeated_factor_name),"none",repeated_factor_name)
      result[["confound"]] = ifelse(is.null(confound),"none",confound)
      result[["batch"]] = ifelse(is.null(batch),"none",batch)
    }
  }
  return(result)
}



