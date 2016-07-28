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
#'
#'
stat_t_test = function(data,data2,i,cl,
                       ttestmethod,ttestFDRmethod, nonpara_ttestmethod,nonpara_ttestFDRmethod){ # i tells which column of data2 is group.
  # determine the dimension of result.

  ttestmethod = ttestmethod=="t test"

  data2$value = data[,1]
  # data2_nonPara$value = data[,1]
  p_value = oneway.test(data2$value ~ data2[,i],var.equal = ttestmethod)$p.value
  p_value_nonPara = wilcox.test(data2$value ~ data2[,i])$p.value
  result = matrix(nrow = ncol(data),ncol = length(c(p_value = p_value,p_value_nonPara)) + 1 + 1)#fdr




  o  = parLapply(cl, 1:ncol(data), fun = function(j,data2,data,i,ttestmethod){
    data2$value = data[,j]
    # data2_nonPara$value = data[,j]
    p_value = oneway.test(data2$value ~ data2[,i],var.equal = ttestmethod)$p.value
    p_value_nonPara = wilcox.test(data2$value ~ data2[,i])$p.value
    c(p_value = p_value,p_value = p_value_nonPara)#fdr


  },data2,data,i,ttestmethod)
  result[,c(1,3)] = matrix(unlist(o),nrow = ncol(data),byrow = T)


    result[,2] = p.adjust(result[,1],method=ttestFDRmethod)

    result[,4] = p.adjust(result[,3],method=nonpara_ttestFDRmethod)


  result = data.frame(result,stringsAsFactors = F,check.names = F)
  temp = paste0("p_value_",paste(levels(data2[,i])[levels(data2[,i])%in%unique(data2[,i])],collapse  = "_vs_"))
  colnames(result) = c(temp,paste0(ttestFDRmethod,"_Ajusted_",temp),paste0("_non_parametric_",temp),paste0(nonpara_ttestFDRmethod,"_Adjusted_non_parametric_",temp))
  rownames(result) = colnames(data)
  return(result)
}
