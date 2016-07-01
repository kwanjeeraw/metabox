#'stat_delete_result_column
#'@description stat_delete_result_column
#'
#'@usage
#'@param norm.

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'
stat_delete_result_column = function(stat_result,selected_feature_by_check){
  selected_feature_by_check = c(selected_feature_by_check,rep(TRUE,ncol(stat_result) - length(selected_feature_by_check)))
  return(data.frame(stat_result[,selected_feature_by_check],check.names = FALSE))
}
