#'stat_QC_RSD
#'@description stat_QC_RSD
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
stat_QC_RSD = function(e,p,f,cl){


  return(data.frame(QC_RSD = parSapply(cl,e[p$QC=="TRUE",],function(x){
    sd(x)/mean(x)
  })))

}
