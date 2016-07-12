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
  identity(x = {
    x = rnorm(1000)
    plot(x)
    mean(x)
    write.table(x, "aaaa.txt")
  })
}
