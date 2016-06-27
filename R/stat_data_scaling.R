#'sample normalization
#'@description sample normalization
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
stat_data_scaling = function(e_after_transformation, f, p,
                             data_scaling = NULL){
  if(length(data_scaling)==0){
    e_after_scaling = e_after_transformation
    data_scaling = "None"
  }else if(data_scaling == "autoscaling"){
    e_after_scaling = scale(e_after_transformation)
  }else if(data_scaling =="paretoscaling"){
    e_after_scaling = stat_pareto_scale(e_after_transformation)
  }else if(data_scaling =="rangescaling"){
    e_after_scaling = stat_range_scale(e_after_transformation)
  }else{
    e_after_scaling = e_after_transformation
    data_scaling = "None"
  }
  return(data_scaling)
}
