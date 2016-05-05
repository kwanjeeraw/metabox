#'Guess Factor and return all the column name of p.
#'@description Guess factor.
#' XX
#'@usage
#' XX
#'@param p the data set p.

#'@details
#'
#'@return
#'option is all the column names of p.
#'guess_factor_name is the guessed factor name. only support independent factor now.!!!
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
### Summarize dataset.
guess_factor = function(p=NULL){
  factor.index = sapply(p,function(x){# guess which columns are experimental factors.
        length(unique(x))
      })
  guess_factor_name = colnames(p)[factor.index < nrow(p)/10 & factor.index > 1]#!!!

  result = list(options=p,guess_factor_name=guess_factor_name)
  return(result)
}





# if(length(factor_name)==0){
#   factor.index = sapply(p,function(x){# guess which columns are experimental factors.
#     length(unique(x))
#   })
#   factor_name = colnames(p)[factor.index < nrow(p)/10 & factor.index > 1]#!!!
# }else{
#   factor_name = factor_name
# }
