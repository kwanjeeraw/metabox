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
stat_sample_norm = function(e, f, p,
                     sample_normalization = NULL){
  if(length(sample_normalization)==0){
    e_after_sample_normalization = e
    sample_normalization = "None"
  }else if(sample_normalization == "mTIC"){
    e_after_sample_normalization = stat_mTIC(e)
  }else if(sample_normalization == "loess"){
    #NA
  }else if(sample_normalization == "medianfoldchange"){
    e_after_sample_normalization = stat_medFC(e)
  }else if(sample_normalization == "BatchMedian"){
    #NA
  }else{
    e_after_sample_normalization = e
    sample_normalization = "None"
  }
  return(list(result = e_after_sample_normalization))
}
