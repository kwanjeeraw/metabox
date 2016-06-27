#'median fold change
#'@description median fold change
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
stat_medFC <- function(e,p,f) {

  medSam <- apply(e, 1, median)
  medSam[which(medSam==0)] <- 0.0001
  e <- apply(e, 2, function(x, medSam){
    medFDiSmpl <- x/medSam
    vec<-x/median(medFDiSmpl)
    return(vec)
  }, medSam)
  return (e)
}
