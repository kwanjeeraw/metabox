#'normalization
#'@description normalization
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


Norm = function(e, f, p, method = "log"){
  result = list(dataset = list(expression = e, feature = f, phenotype = p))
  if(method %in% c("log", "log10", "log2")){
    normalized = get(method)(e)
  }
  if(method == "none"){
    normalized = e
  }

  result[["normalized_data"]] = normalized
  return(result)
}
