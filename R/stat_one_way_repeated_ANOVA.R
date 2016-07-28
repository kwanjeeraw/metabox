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
stat_one_way_repeated_ANOVA = function(data,data2,i,sudo_matrix,factor_name,cl,
                                       onewaySpher_Corr){#factor_name: make the result a better column name.




  data2$value = data[,1]
  colnames(data2)[i] = "repvariable"


  data2$id = as.character(data2$id)






  for(j in levels(data2$repvariable)){
    data2$id[data2$repvariable%in%j] = 1:sum(data2$repvariable%in%j)
  }




  data2$id = as.factor(  data2$id )
  data2$repvariable = as.factor(data2$repvariable )

  if(!onewaySpher_Corr == 'none'){
    p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`[paste0('p[',onewaySpher_Corr,']')]
    if(is.null(p_value)){
      p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
    }
  }else{
    p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
  }


  test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
  post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]


  test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
                                                       color.blocks.in.cor.plot = F)
  p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
  post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
  post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]

  result = matrix(nrow = ncol(data),ncol = length(c(ANOVA_p_value = p_value,ANOVA_p_value_nonPara=p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))))#no need for fdr



  o  = parLapply(cl, 1:ncol(data), fun = function(j,data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
                                                  stat_cure_Dunn_format,stat_combine_vector_1by1,result,onewaySpher_Corr){
    data2$value = data[,j]
    if(!onewaySpher_Corr == 'none'){
      p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`[paste0('p[',onewaySpher_Corr,']')]
      if(is.null(p_value)){
        p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
      }
    }else{
      p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
    }

    test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
    post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]


    test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
                                                         color.blocks.in.cor.plot = F)
    p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
    post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
    post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]
    c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))#fdr
  },data2,data,ezANOVA,stat_friedman_test_with_post_hoc,stat_cure_Dunn_format,stat_combine_vector_1by1,result,onewaySpher_Corr)
  result = matrix(unlist(o),nrow = ncol(data),byrow = T)


  # microbenchmark::microbenchmark(
  #   loop = {
  #     for(j in 1:ncol(data)){
  #       data2$value = data[,j]
  #       p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`$'p[GG]'
  #       if(is.null(p_value)){
  #         p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
  #       }
  #       test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
  #       post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]
  #
  #
  #       test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
  #                                                            color.blocks.in.cor.plot = F)
  #       p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
  #       post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
  #       post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]
  #       result[j,] = c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))#fdr
  #     }
  #   },
  #   parallel = {
  #     o  = parLapply(cl, 1:ncol(data), fun = function(j,data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
  #                                                stat_cure_Dunn_format,stat_combine_vector_1by1,result){
  #       data2$value = data[,j]
  #       p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`$'p[GG]'
  #       if(is.null(p_value)){
  #         p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
  #       }
  #       test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
  #       post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]
  #
  #
  #       test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
  #                                                            color.blocks.in.cor.plot = F)
  #       p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
  #       post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
  #       post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]
  #       result[j,] = c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))#fdr
  #     },data2,data,ezANOVA,stat_friedman_test_with_post_hoc,stat_cure_Dunn_format,stat_combine_vector_1by1,result)
  #     result = matrix(unlist(o),nrow = ncol(data),byrow = T)
  #   },
  #   times = 10L
  # )
  #
  #
  #
  #
  #
  #
  #
  #
  #
  # for(j in 1:ncol(data)){
  #   data2$value = data[,j]
  #   p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`$'p[GG]'
  #   if(is.null(p_value)){
  #     p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
  #   }
  #   test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
  #   post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]
  #
  #
  #   test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
  #                                                        color.blocks.in.cor.plot = F)
  #   p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)
  #   post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
  #   post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]
  #   result[j,] = c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post_hoc, post_hoc_nonPara))#fdr
  # }

  result = data.frame(result,stringsAsFactors = F,check.names = F)
  colnames(result) = rep(c(paste0("ANOVA_p_value_of_",factor_name[i-1]),
                           paste0("p_value_",apply(combn(levels(data2[,i]), 2),2,function(x){paste(x[1],x[2],sep="_vs_")}))),each = 2)
  rownames(result) = colnames(data)
  for(j in seq(2,ncol(result),2)){
    colnames(result)[j] = paste0("non_parametric_",colnames(result)[j])
  }
  return(result)
}
