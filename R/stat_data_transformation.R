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
stat_data_transformation = function(e_after_sample_normalization, f, p,
                                    data_transformation = NULL){
  if(length(data_transformation)==0){
    e_after_transformation = e_after_sample_normalization
    data_transformation = "None"
  }else if(data_transformation == "log"){
    e_after_sample_normalization[e_after_sample_normalization<=0] = 0.001 #!!!
    e_after_transformation = log(e_after_sample_normalization)
  }else if(data_transformation =="Cube_root"){
    e_after_transformation = e_after_sample_normalization^(1/3)
  }else{
    e_after_transformation = e_after_sample_normalization
    data_transformation = "None"
  }
  return(e_after_transformation)
}
