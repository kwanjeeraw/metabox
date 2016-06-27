#'stat_rm_sample
#'@description stat_rm_sample
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
#'
#'
stat_rm_sample = function(e,p,f,
                          sample_index){
  sample_index = as.numeric(sample_index)
  if(sample_index == 0 || is.null(sample_index) || sum(is.na(as.numeric(sample_index)))>0){
    e_result = e
    p_result = p
  }else{
    e_result = e[!p$phenotype_index%in%sample_index,]
    p_result = p[!p$phenotype_index%in%sample_index,]
  }

  return(list(expression = e_result, phenotype = p_result, feature = f))
}
