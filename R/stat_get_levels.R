#'stat_get_levels
#'@description stat_get_levels
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'
#'
stat_get_levels = function(e,f,p,factor){
  return(paste0(paste0(unique(p[,factor]),collapse= ';'),';'))
}
