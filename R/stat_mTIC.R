#'mTIC
#'@description mTIC
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
stat_mTIC = function(e,f,p){
  means = sapply(unique(p$sampleID),function(id){
    mean(unlist(e[p$sampleID%in%id,f$KnownorUnknown=="TRUE"]))
  })
  M = mean(means)
  for(id in unique(p$sampleID)){
    e[p$sampleID%in%id,] = e[p$sampleID%in%id,]/means[names(means)%in%id] * M
  }
  return(e)
}
