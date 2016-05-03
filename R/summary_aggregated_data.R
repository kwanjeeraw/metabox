#'summarize aggregated data.
#'@description summarize aggregated data.
#' Summarize the information that is important for mETABOX statistical analysis, e.g. factor_name, repeated.factor_name, etc.
#'@usage
#'summary_aggregated_data(aggregated_data, factor.name, repeated.factor.name)
#'@param aggregated_data an list with three dataframes named as "expression", "feature" and "phenotype". Or simply a object returned from
#'load_aggregated_data().
#'@param factor.name The factor names that are related to the study design.
#'factor.name must be one of the column name of the "phenotype" dataframe. Although the default is NULL, it shouldn't be NULL for mETABOX
#'statistical analysis.
#'@param repeated.factor.name The repeated factor name indicates which factor is repeated measure. Must be one of the factor.name.
#'If NULL means that it is independed study design.
#'@details
#'
#'@return
#'a list. If the input aggregated_data is not standard, then it returns a warning message telling user to upload needed file.
#'dataset(list), factor.name(vector of string), repeated.factor.name(vector of string), confound(vector which is a variable),
#'batch(vector which is a variable of factor)
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
### Summarize dataset.
summary_aggregated_data <- function(e=NULL,f=NULL,p=NULL, factor.name = NULL,repeated.factor.name = NULL){
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

      if(length(factor.name)==0){
        factor.index = sapply(p,function(x){# guess which columns are experimental factors.
          length(unique(x))
        })
        factor.name = colnames(p)[factor.index < nrow(p)/10 & factor.index > 1]#!!!
      }else{
        factor.name = factor.name
      }

      confounds = NULL
      repeated.factor.name = repeated.factor.name
      batch = NULL
      result[["dataset"]] = list("expression" = e, "feature" = f, "phenotype" = p)
      result[["factor.name"]] = factor.name
      result[["repeated.factor.name"]] = repeated.factor.name
      result[["confound"]] = confounds
      result[["batch"]] = batch
    }
  }
  return(result)
}



