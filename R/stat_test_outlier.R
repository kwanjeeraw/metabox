#'test outleir
#'@description test outlier
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
stat_test_outlier = function(e,f,p,class){
  dta = data.frame(value = e[,1], class = apply( p[class] , 1 , paste , collapse = " " ))
  outlier = vector()
  for(i in 1:ncol(e)){
    dta$value = e[,i]
    outlier[i] = sum(by(dta$value, dta$class, function(x){
      sum(x>(quantile(x,.75)+1.5*IQR(x))) + sum(x<(quantile(x,.25)-1.5*IQR(x)))
    }))
  }

  return(outlier>0)

}
