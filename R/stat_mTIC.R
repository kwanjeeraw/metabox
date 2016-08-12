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
stat_mTIC = function(e,f,p,KnownorUnknown){

  if("QC"%in%colnames(p)){# QC must have negative subjectID!
    if(sum(p$subjectID[p$QC=="TRUE"]>0)>0){
      stop("The subjectID or QC must be negative of zero! For example, 0, -1, -2, ... Please correct it before upload your file.")
    }
  }

  means = sapply(unique(p$subjectID)[unique(p$subjectID)>0],function(id){
    mean(unlist(e[p$subjectID%in%id,f[,KnownorUnknown]=="TRUE"]))
  })
  M = mean(means)



  for(id in unique(p$subjectID)[unique(p$subjectID)>0]){
    e[p$subjectID%in%id,] = e[p$subjectID%in%id,]/means[names(means)%in%id] * M
  }
  return(e)
}
