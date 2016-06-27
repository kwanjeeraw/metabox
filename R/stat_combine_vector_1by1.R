#'combine value one by one for two vectors
#'@description combine value one by one for two vectors
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
stat_combine_vector_1by1 = function(x,y){
  if(!length(x)==length(y)){
    stop("x and y have different lengths")
  }else{
    c(mapply(function(p,q){
      c(p,q)
    },x,y))
  }
}
