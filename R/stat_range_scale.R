#'stat_range_scale
#'@description stat_range_scale
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
stat_range_scale = function(e,f,p){
  means = sapply(e,mean,na.rm = T)
  ranges = apply(sapply(e,range,na.rm = T),2,function(x){
    abs(diff(x))
  })
  for(i in 1:ncol(e)){
    e[,i] = (e[,i] - means[i])/ranges[i]
  }
  return(e)
}
