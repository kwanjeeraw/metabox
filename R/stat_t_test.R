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
                       ttestmethod,ttestcorrection, nonparattestmethod,nonparattestcorrection){ # i tells which column of data2 is group.

  if(ttestmethod == 'none'){
    p_value = p_value_adj = rep(NA,ncol(data))
  }else{
    ttestmethod = ttestmethod=="t test" # if FALSE, use Welch
    p_value = parSapply(cl,1:ncol(data),FUN = function(j,data2,data,i,ttestmethod){
      data2$value = data[,j]
      oneway.test(data2$value ~ data2[,i], var.equal = ttestmethod)$p.value
    },data2,data,i,ttestmethod)
    p_value_adj = p.adjust(p_value,ttestcorrection)
  }

  if(nonparattestmethod == 'none'){
    nonparap_value = nonparap_value_adj = rep(NA,ncol(data))
  }else{
    nonparap_value = parSapply(cl,1:ncol(data),FUN = function(j,data2,data,i){
      data2$value = data[,j]
      wilcox.test(data2$value ~ data2[,i])$p.value
    },data2,data,i)
    nonparap_value_adj = p.adjust(nonparap_value,nonparattestcorrection)
  }

  result = data.frame(p_value,p_value_adj,nonparap_value,nonparap_value_adj,stringsAsFactors = F,check.names = F)
  temp = paste0("p_value_",paste(levels(data2[,i])[levels(data2[,i])%in%unique(data2[,i])],collapse  = "_vs_"))
  colnames(result) = c(temp,paste0(ttestcorrection,"_adjusted_",temp),paste0("_non_parametric_",temp),paste0(nonparattestcorrection,"_adjusted_non_parametric_",temp))
  rownames(result) = colnames(data)

  result = result[,sapply(result,function(x){
    sum(is.na(x))
  })<ncol(data2)]




  return(result)
}
