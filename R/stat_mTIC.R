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
  means = apply(e[,f$KnownorUnknown=="TRUE"],1,mean)
  M = mean(means)
  for(i in 1:nrow(e)){
    e[i,] = e[i,]/means[i] * M
  }
  return(e)
}
