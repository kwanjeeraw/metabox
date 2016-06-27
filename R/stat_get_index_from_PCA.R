#'get Index from PCA
#'@description get Index from PCA
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
stat_get_index_from_PCA = function(e,p,f,selected_sample){
  if(is.null(selected_sample)||length(selected_sample)==0){
    result = ""
  }else{
    e = as.matrix(e)
    pca = prcomp(e, center = F, scale. = F)
    score = pca$x
    result = p$phenotype_index[which(round(score[,1],4)%in%round(selected_sample,4))]
  }

  return(list(result = result))
}
