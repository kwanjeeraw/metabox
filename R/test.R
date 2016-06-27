#'hypothesis test
#'@description hypothesis test
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examplesz
#'@export
#

test = function(n){
  library(parallel);
  cl <- makeCluster(getOption("cl.cores", 2))
  n = as.numeric(n)
  m = 100



  result=  parSapply(cl, 1:20, function(x,n,m){
    x + n + m
  },n = n, m = m)
  stopCluster(cl)

  return(result)
}
