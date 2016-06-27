#'stat_one_way_repeated_ANOVA
#'@description stat_one_way_repeated_ANOVA
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
stat_one_way_repeated_ANOVA = function(data,data2,i,sudo_matrix,factor_name){#factor_name: make the result a better column name.

  data2$value = data[,1]
  colnames(data2)[i] = "repvariable"
  p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`$'p[GG]'
  test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
  post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]


  test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
                                                       color.blocks.in.cor.plot = F)
  p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
  post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix)


  result = matrix(nrow = ncol(data),ncol = length(c(ANOVA_p_value = p_value,ANOVA_p_value_nonPara=p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))))#no need for fdr
  for(j in 1:ncol(data)){
    data2$value = data[,j]
    colnames(data2)[i] = "repvariable"
    p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`$'p[GG]'
    test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
    post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]


    test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
                                                         color.blocks.in.cor.plot = F)
    p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
    post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix)

    result[j,] = c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))#fdr
  }

  result = data.frame(result,stringsAsFactors = F,check.names = F)
  colnames(result) = rep(c(paste0("ANOVA_p_value_of_",factor_name[i-1]),
                           paste0("p_value_",apply(combn(levels(data2[,i]), 2),2,function(x){paste(x[1],x[2],sep="_vs_")}))),each = 2)
  rownames(result) = colnames(data)
  for(j in seq(2,ncol(result),2)){
    colnames(result)[j] = paste0("non_para_",colnames(result)[j])
  }
  return(result)
}
