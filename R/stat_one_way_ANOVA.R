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

    para = matrix(NA,nrow = sum(1:(length(unique(data2[,i]))-1))+1,ncol = ncol(e))

  }else{
    ANOVAmethod = ANOVAmethod == "ANOVA" # if true, then equal variance.

    para  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,stat_friedman_test_with_post_hoc,
                                                    stat_cure_Dunn_format,stat_combine_vector_1by1,i,
                                                    posthocTGH,dunnTest,ANOVAmethod,ANOVAposthoc){
      data2$value = data[,j]
      p_value = oneway.test(data2$value ~ data2[,i], var.equal = ANOVAmethod)$p.value

      post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[[ANOVAposthoc]][,3]
      temp = data.frame(post.hoc)
      rownames(temp) = gsub(":", " - ", rownames(temp))

      c(p_value = p_value, post.hoc)
    },data2,data,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,stat_combine_vector_1by1,i,
    posthocTGH,dunnTest,ANOVAmethod,ANOVAposthoc)
  }

  if(nonparaANOVAmethod == 'none'){

  }else{

    post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[['games.howell']][,3]
    temp = data.frame(post.hoc)
    rownames(temp) = gsub(":", " - ", rownames(temp)) #last three lines are for the correction of the format of the Dunn procedure.




    nonpara  = parSapply(cl, 1:ncol(data), FUN = function(j,data2,data,stat_friedman_test_with_post_hoc,
                                                          stat_cure_Dunn_format,stat_combine_vector_1by1,i,
                                                          posthocTGH,dunnTest,temp){
      data2$value = data[,j]
      p_value_nonPara = kruskal.test(data2$value ~ data2[,i])$p.value

      x = data.frame(dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res[,c(1,4)],row.names =1)
      post.hoc_nonPara =stat_cure_Dunn_format(x = x,sudo_matrix,temp = temp)[,1]

      c(p_value = p_value_nonPara, post.hoc_nonPara)
    },data2,data,stat_friedman_test_with_post_hoc,
    stat_cure_Dunn_format,stat_combine_vector_1by1,i,
    posthocTGH,dunnTest,temp)
    rownames(nonpara) = c(paste0("non_parametric_interaction_p_value"),names(post.hoc))

  }













  if(onewayANOVAmethod == "none"){
    result = data.frame(no_parametricANOVA_applied = rep(NA,ncol(data)))
  }else{
    onewayANOVAmethod = onewayANOVAmethod=='ANOVA' #(otherwise use Welch ANOVA)
  }


  # determine the dimension of result.
  data2$value = data[,1]




  p_value = oneway.test(data2$value ~ data2[,i], var.equal = onewayANOVAmethod)$p.value
  p_value_nonPara = kruskal.test(data2$value ~ data2[,i])$p.value



  post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[[onewayANOVAposthocmethod]][,3]



  # post.hoc_nonPara = dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res$P.adj
  temp = data.frame(post.hoc)
  rownames(temp) = gsub(":", " - ", rownames(temp))
  x = data.frame(dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res[,c(1,4)],row.names =1)
  post.hoc_nonPara = stat_cure_Dunn_format(x = x,
                                                                          sudo_matrix,temp = temp)
  post.hoc_nonPara = post.hoc_nonPara[,1]
  result = matrix(nrow = ncol(data),ncol = length(c(ANOVA_p_value = p_value,ANOVA_p_value_nonPara=p_value_nonPara, stat_combine_vector_1by1(post.hoc, post.hoc_nonPara))))#no need for fdr

  o  = parLapply(cl, 1:ncol(data), fun = function(j,data2,data,stat_friedman_test_with_post_hoc,
                                                  stat_cure_Dunn_format,stat_combine_vector_1by1,i,
                                                  posthocTGH,dunnTest,onewayANOVAmethod,onewayANOVAposthocmethod){
    data2$value = data[,j]
    p_value = oneway.test(data2$value ~ data2[,i], var.equal = onewayANOVAmethod)$p.value
    p_value_nonPara = kruskal.test(data2$value ~ data2[,i])$p.value
    post.hoc = posthocTGH(data2$value , data2[,i], digits=4)$output[[onewayANOVAposthocmethod]][,3]
    # post.hoc_nonPara = dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res$P.adj
    temp = data.frame(post.hoc)
    rownames(temp) = gsub(":", " - ", rownames(temp))
    x = data.frame(dunnTest(data2$value,data2[,i],kw =T, method="bonferroni")$res[,c(1,4)],row.names =1)
    post.hoc_nonPara = stat_cure_Dunn_format(x = x,
                                             sudo_matrix,temp = temp)
    post.hoc_nonPara = post.hoc_nonPara[,1]
    c(p_value = p_value,p_value_nonPara, stat_combine_vector_1by1(post.hoc, post.hoc_nonPara))#fdr
  },data2,data,stat_friedman_test_with_post_hoc,
  stat_cure_Dunn_format,stat_combine_vector_1by1,i,
  posthocTGH,dunnTest,onewayANOVAmethod,onewayANOVAposthocmethod)
  result = matrix(unlist(o),nrow = ncol(data),byrow = T)



  result = data.frame(result,stringsAsFactors = F,check.names = F)
  colnames(result) = rep(c(paste0("ANOVA_p_value_of_",factor_name[i-1]),
                           paste0("p_value_",apply(combn(levels(data2[,i]), 2),2,function(x){paste(x[1],x[2],sep="_vs_")}))),each = 2)
  rownames(result) = colnames(data)
  for(j in seq(2,ncol(result),2)){
    colnames(result)[j] = paste0("non_parametric_",colnames(result)[j])
  }
  return(result)
}
