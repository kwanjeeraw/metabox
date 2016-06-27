#'stat_pareto_scale
#'@description stat_pareto_scale
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
stat_pareto_scale = function(e,f,p){
  means = sapply(e,mean,na.rm = T)
  sds = sapply(e,sd,na.rm = T)
  for(i in 1:ncol(e)){
    e[,i] = (e[,i] - means[i])/sqrt(sds[i])
  }
  return(e)
}
