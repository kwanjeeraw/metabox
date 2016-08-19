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
                                       pairedANOVAmethod, pairedANOVAadjust, pairedANOVAposthoc, nonparapairedANOVAmethod, nonparapairedANOVAposthoc){#factor_name: make the result a better column name.




  data2$value = data[,1]
  colnames(data2)[i] = "repvariable"


  data2$id = as.character(data2$id)






  for(j in levels(data2$repvariable)){
    data2$id[data2$repvariable%in%j] = 1:sum(data2$repvariable%in%j)
  }




  data2$id = as.factor(  data2$id )
  data2$repvariable = as.factor(data2$repvariable )




  if(!pairedANOVAmethod == 'none'){
    para  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
                                                       stat_cure_Dunn_format,stat_combine_vector_1by1,i,
                                                       pairedANOVAmethod,pairedANOVAadjust,pairedANOVAposthoc){
      # for(j in 1:ncol(data)){
        data2$value = data[,j]
        if(!pairedANOVAadjust == 'none'){
          p_value = tryCatch(ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$`Sphericity Corrections`[paste0('p[',pairedANOVAadjust,']')],
                             error=function(e){
                               return("NA(too few samples)")
                             })
          if(is.null(p_value)){
            p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
          }
        }else{
          p_value = ezANOVA(data = data2, dv = value, wid = id, within = .(repvariable), type = 3)$ANOVA$p
        }

        if(!pairedANOVAposthoc == 'none'){
          test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value
          post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]
        }else{
          post_hoc = rep(NA, sum((length(unique(data2[,i]))-1):1))
        }

        p_value = as.numeric(p_value)
        c(p_value,post_hoc)#fdr
      # }

    },data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,stat_combine_vector_1by1,i,
    pairedANOVAmethod,pairedANOVAadjust,pairedANOVAposthoc)
  }else{
    para = matrix(NA, nrow = 1 + sum((length(unique(data2[,i]))-1):1), ncol = ncol(data))
  }




  test.temp = pairwise.t.test(paired = T, x = data2$value, g = data2$repvariable, p.adjust.method  = "bonf")$p.value # helping correct the format. stat_cure_Dunn_format
  post_hoc = as.numeric(test.temp)[!is.na(as.numeric(test.temp))]



  if(!nonparapairedANOVAmethod == 'none'){
    nonpara  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
                                                       stat_cure_Dunn_format,stat_combine_vector_1by1,i,
                                                       nonparapairedANOVAmethod,nonparapairedANOVAposthoc,test.temp,sudo_matrix){
      data2$value = data[,j]
      test_with_posthoc = stat_friedman_test_with_post_hoc(value ~ repvariable | id ,data2,to.print.friedman = F,to.plot.parallel = F,to.plot.boxplot = F,
                                                           color.blocks.in.cor.plot = F)
      p_value_nonPara = pvalue(test_with_posthoc$Friedman.Test)


      if(!nonparapairedANOVAposthoc == 'none'){
        if(nonparapairedANOVAposthoc=='dunn'){
          post_hoc_nonPara = stat_cure_Dunn_format(test_with_posthoc$PostHoc.Test,sudo_matrix,temp = test.temp)
          post_hoc_nonPara = as.numeric(post_hoc_nonPara)[!is.na(as.numeric(post_hoc_nonPara))]
        }else{
          post_hoc_nonPara = as.vector(pairwise.wilcox.test(data2$value, g=data2[,i], p.adjust.method = "bonferroni",
                                         paired = T)$p.value)
          post_hoc_nonPara = post_hoc_nonPara[!is.na(post_hoc_nonPara)]
        }
      }else{
        post_hoc_nonPara = rep(NA, sum((length(unique(data2[,i]))-1):1))
      }

      c(p_value_nonPara , post_hoc_nonPara)#fdr
    },data2,data,ezANOVA,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,stat_combine_vector_1by1,i,
    nonparapairedANOVAmethod,nonparapairedANOVAposthoc,test.temp,sudo_matrix)
  }else{
    nonpara = matrix(NA, nrow = 1 + sum((length(unique(data2[,i]))-1):1), ncol = ncol(data))
  }


  result=parSapply(cl, 1:ncol(data), FUN = function(j,para,nonpara,stat_combine_vector_1by1){
    stat_combine_vector_1by1(para[,j], nonpara[,j])
  },para,nonpara,stat_combine_vector_1by1)
  result = data.frame(t(result),stringsAsFactors = F)
 #!! how to assign colnames and rownames.
  # test.temp
  colnames(result) = rep(c(paste0("ANOVA_p_value_of_",factor_name[i-1]),
                           paste0("p_value_",apply(combn(levels(data2[,i]), 2),2,function(x){paste(x[1],x[2],sep="_vs_")}))),each = 2)
  rownames(result) = colnames(data)
  for(j in seq(2,ncol(result),2)){
    colnames(result)[j] = paste0("non_parametric_",colnames(result)[j])
  }

  result = result[,!sapply(result, function(x){sum(is.na(x))})==ncol(data)]

  return(result)
}
