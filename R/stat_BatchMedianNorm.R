#'stat_BatchMedianNorm
#'@description stat_BatchMedianNorm
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
stat_BatchMedianNorm = function(e,f,p,BatchIndicator){



  for(i in 1:ncol(e)){
    means = by(e[,i],p[,BatchIndicator],function(x){
      mean(x)
    })
    for(j in 1:length(means)){
      e[p[,BatchIndicator]==names(means)[j],i] =e[p[,BatchIndicator]==names(means)[j],i] / (means[j] / mean(means))
    }
  }
  return(e)
}
