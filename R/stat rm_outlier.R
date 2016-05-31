#'Remove Outlier
#'@description Remove Outlier
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
#'
 Deal_with_outlier = function(data,method = "remove"){#https://statistics.laerd.com/premium/spss/istt/independent-t-test-in-spss-10.php
   quantile = quantile(data, probs = c(.25,.75))
   IQR = diff(quantile)
   outlier_index = vector()
   if(method == "remove"){
     while(sum((data<quantile[1]-IQR |data>quantile[2]+IQR))){
       quantile = quantile(data, probs = c(.25,.75))
       IQR = diff(quantile)
       outlier_index[i] = which(data<quantile[1]-IQR |data>quantile[2]+IQR)
       data = data[!(data<quantile[1]-IQR |data>quantile[2]+IQR)]
     }
   }
   # if(method == "")

   return(list(data = data,outlier_index = outlier_index))
 }

