#'stat_paired_t_test
#'@description stat_paired_t_test
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
stat_paired_t_test = function(data,data2,i,cl,
                              pairedttestmethod,pairedttestcorrection,nonparapairedttestmethod,nonparapairedttestcorrection){ # i tells which column of data2 is group.
  # determine the dimension of result.


  data_wide <- data.frame(# to prevent that the id are not sorted in both group.
    ID=1:max(as.numeric(as.character(data2$id))),
    group1=data2$value[data2[,i]==levels(data2[,i])[1]],
    group2=data2$value[data2[,i]==levels(data2[,i])[2]]
  )


  if(pairedttestmethod == 'none'){
    p_value = p_value_adj =  rep(NA, ncol(data))
  }else{
    p_value  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,i){
      data2$value = data[,j]
      data_wide <- data.frame(# to prevent that the id are not sorted in both group.
        ID=1:max(as.numeric(as.character(data2$id))),
        group1=data2$value[data2[,i]==levels(data2[,i])[1]],
        group2=data2$value[data2[,i]==levels(data2[,i])[2]]
      )
      p_value = t.test(data_wide$group1 - data_wide$group2, mu=0)$p.value
      # p_value_nonPara = wilcox.test(data_wide$group1 - data_wide$group2)$p.value
      p_value
    },data2,data,i)

    p_value_adj = p.adjust(p_value, pairedttestcorrection)

  }

  if(nonparapairedttestmethod == 'none'){
    nonparap_value = nonparap_value_adj = rep(NA, ncol(data))
  }else{
    nonparap_value =    parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,i){
      data2$value = data[,j]
      data_wide <- data.frame(# to prevent that the id are not sorted in both group.
        ID=1:max(as.numeric(as.character(data2$id))),
        group1=data2$value[data2[,i]==levels(data2[,i])[1]],
        group2=data2$value[data2[,i]==levels(data2[,i])[2]]
      )
      p_value_nonPara = wilcox.test(data_wide$group1 - data_wide$group2)$p.value
      p_value_nonPara
    },data2,data,i)
    nonparap_value_adj = p.adjust(nonparap_value, nonparapairedttestcorrection)
  }

  result  = data.frame(p_value, p_value_adj, nonparap_value, nonparap_value_adj, stringsAsFactors = F, check.names = F)
  temp = paste0("p_value_",paste(levels(data2[,i])[levels(data2[,i])%in%unique(data2[,i])],collapse  = "_vs_"))
  colnames(result) = c(temp,paste0(pairedttestcorrection,"_adjusted_",temp),paste0("_non_parametric_",temp),paste0(nonparapairedttestcorrection,"_adjusted_non_parametric_",temp))
  rownames(result) = colnames(data)


  result = result[,sapply(result,function(x){
    sum(is.na(x))
  })<ncol(data2)]



  return(result)
}
