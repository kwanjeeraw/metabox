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

  if("QC"%in%colnames(p)){# QC must have negative sampleID!
    if(sum(p$sampleID[p$QC=="TRUE"]>0)>0){
      stop("The sampleID or QC must be negative of zero! For example, 0, -1, -2, ... Please correct it before upload your file.")
    }
  }

  means = sapply(unique(p$sampleID)[unique(p$sampleID)>0],function(id){
    mean(unlist(e[p$sampleID%in%id,f$KnownorUnknown=="TRUE"]))
  })
  M = mean(means)



  for(id in unique(p$sampleID)[unique(p$sampleID)>0]){
    e[p$sampleID%in%id,] = e[p$sampleID%in%id,]/means[names(means)%in%id] * M
  }
  return(e)
}
