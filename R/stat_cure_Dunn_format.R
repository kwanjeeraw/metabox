#'format bad sequence of DunnTest
#'@description format bad sequence of DunnTest
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

stat_cure_Dunn_format = function(x,sudo_matrix){
  sudo_matrix[lower.tri(sudo_matrix)] = x
  return(t(sudo_matrix)[upper.tri(t(sudo_matrix))])

}
