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
stat_one_way_ANOVA = function(data,data2,i,sudo_matrix,factor_name,cl,
                              ANOVAmethod,ANOVAposthoc,nonparaANOVAmethod,nonparaANOVAposthoc){#factor_name: make the result a better column name.


  if(ANOVAmethod == 'none'){

    para = matrix(NA,nrow = sum(1:(length(unique(data2[,i]))-1))+1,ncol = ncol(data))

  }else{
    ANOVAmethod = ANOVAmethod == "ANOVA" # if true, then equal variance.

    para  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,stat_friedman_test_with_post_hoc,
                                                    stat_cure_Dunn_format,i,
                                                    posthocTGH,dunnTest,ANOVAmethod,ANOVAposthoc){
      data2$value = data[,j]
      p_value = oneway.test(data2$value ~ data2[,i], var.equal = ANOVAmethod)$p.value

      if(ANOVAposthoc == 'none'){
        post.hoc = rep(NA,sum(1:(length(unique(data2[,i]))-1)))
      }else{
        post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[["games.howell"]][,6]
      }



      return(c(p_value = p_value, post.hoc))
    },data2,data,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,i,
    posthocTGH,dunnTest,ANOVAmethod,ANOVAposthoc)
  }

  if(nonparaANOVAmethod == 'none'){
    nonpara = matrix(NA,nrow = sum(1:(length(unique(data2[,i]))-1))+1,ncol = ncol(data))
  }else{

    post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[['games.howell']][,6]
    temp = data.frame(post.hoc)
    rownames(temp) = rownames(posthocTGH(data2$value , data2[,i], digits=4)$output[['games.howell']])
    rownames(temp) = gsub("-", " - ", rownames(temp)) # the " - " is important for stat_cure_Dunn_format

    nonpara  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,stat_friedman_test_with_post_hoc,
                                                          stat_cure_Dunn_format,i,temp,
                                                          posthocTGH,dunnTest,nonparaANOVAposthoc){
      data2$value = data[,j]
      p_value_nonPara = kruskal.test(data2$value ~ data2[,i])$p.value


      # if(!nonparaANOVAposthoc=='none'){
      #   x = data.frame(dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res[,c(1,4)],row.names =1)
      #   post.hoc_nonPara =stat_cure_Dunn_format(x = x,sudo_matrix,temp = temp)[,1]
      # }else
        if(nonparaANOVAposthoc == 'none'){
        post.hoc_nonPara = rep(NA,sum(1:(length(unique(data2[,i]))-1)))
      }else{
        x = as.vector(pairwise.wilcox.test(data2$value, g=data2[,i], p.adjust.method = "bonferroni",
                                           paired = FALSE)$p.value)
        post.hoc_nonPara = x[!is.na(x)]
      }


      c(p_value = p_value_nonPara, post.hoc_nonPara)
    },data2,data,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,i,temp,
    posthocTGH,dunnTest,nonparaANOVAposthoc)


  }


  result=parSapply(cl, 1:ncol(data), FUN = function(j,para,nonpara,stat_combine_vector_1by1){
    stat_combine_vector_1by1(para[,j], nonpara[,j])
  },para,nonpara,stat_combine_vector_1by1)
  result = data.frame(t(result),stringsAsFactors = FALSE)

  # assign rownames and colnames.
  post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[['games.howell']][,6]
  temp = data.frame(post.hoc)
  rownames(temp) = rownames(posthocTGH(data2$value , data2[,i], digits=4)$output[['games.howell']])
  rownames(temp) = gsub("-", "_vs_", rownames(temp)) #last three lines are for the correction of the format of the Dunn procedure.
  colnames(result) = c(paste0("p_value_of_",factor_name),paste0("non_parametric_p_value_of_",factor_name),
                       stat_combine_vector_1by1(paste0("p_value_of_",rownames(temp)),paste0("non_parametric_p_value_of_",rownames(temp))))

  result2 = result[,!sapply(result, function(x){sum(is.na(x))})==ncol(data)] #get rid of columns with ALL NA's.
  result2 = data.frame(result2, check.names = F, stringsAsFactors = F)
  colnames(result2) = names(!sapply(result, function(x){sum(is.na(x))})==ncol(data))[!sapply(result, function(x){sum(is.na(x))})==ncol(data)]
  return(result2)
}
