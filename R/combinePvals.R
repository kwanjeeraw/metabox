#'Combine p-values
#'@description The function can combine p-values using Fisher's method.
#'@usage combinePvals(x)
#'@param x numeric vectors of p-values
#'@return numeric number of combined p-values
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Fisher R. (1932) Statistical methods for research workers. Oliver and Boyd, Edinburgh.
#'@seealso \code{\link{pchisq}}
#'@examples
#'#result <- combinePvals(c(0.01,0.005,0.1))
#'@export
combinePvals <- function(x) UseMethod("combinePvals")
#'@export
combinePvals.default <- function(x){
  x = x[!x<=0]
  lt <- log(x) #log transform
  chisqu <- (-2) * sum(lt)
  degreeof <- 2 * length(lt)
  pchisq(chisqu, degreeof, lower.tail = FALSE)
}
