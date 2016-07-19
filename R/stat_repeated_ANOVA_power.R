#'stat_repeated_ANOVA_power
#'@description stat_repeated_ANOVA_power
#'
#'@usage
#'@param result_stat: it must be the basic statistics result.

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'
stat_repeated_ANOVA_power = function(e,p,f,dta,i, result_stat, sig.level = 0.05, desired_power = 0.8,factor_name, epsilon = 1, k = 1, cl){
  # if k = 1, then it is one way repeated ANOVA.

  sample_size = as.numeric(table(dta[,i]))
  m = length(sample_size)
  N = sum(sample_size)
  df1 = (m - 1)*epsilon
  df2 = (N-k)*(m-1)*epsilon #N-1, the 1 means k=1



  which_col_Gsd = which(colnames(result_stat)%in%"Global Standard Deviation")
  which_col_Gmean = which(colnames(result_stat)%in%"Global Mean")



  power_sampleSize = parSapply(cl=cl,
                               X = 1:nrow(result_stat),FUN = function(j,e,dta,result_stat,m, N, sample_size,which_col_Gsd,which_col_Gmean,sig.level,df1,df2,desired_power,pwr.anova.test){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 reshape = tryCatch(reshape(dta, idvar = "id", timevar = "repeated1", direction = "wide"),error=function(err){
                                   reshape(dta, idvar = "id", timevar = "repeated2", direction = "wide")
                                 })
                                 reshape = sapply(reshape,function(x){
                                   as.numeric(as.character(x))
                                 })[,-1]

                                 cor = cor(reshape)
                                 rho = mean(cor[lower.tri(cor)])
                                 mu = m/(1-rho)

                                 sigma_mu = sqrt(sum((result_stat[j,(which_col_Gmean+1):(which_col_Gmean+m)] - result_stat[j,which_col_Gmean])^2 * sample_size/N) )
                                 sigma = result_stat[j,which_col_Gsd]
                                 f = sigma_mu/sigma
                                 ncp = f^2 * mu * N

                                 p.body = quote({
                                   ncp = f^2 * mu * N
                                   pf(qf(sig.level,df1,df2,lower.tail = F),df1,df2,ncp,lower.tail = F)
                                 })

                                 size = tryCatch(uniroot(function(N) eval(p.body) - desired_power, c(2 +
                                                                                               1e-10, 1e+05))$root,error = function(e){return("NA")})
                                 # }


                                 return(c(pf(qf(sig.level,df1,df2,lower.tail = F),df1,df2,ncp,lower.tail = F),size))
                               },e,dta,result_stat,m, N, sample_size,which_col_Gsd,which_col_Gmean,sig.level,df1,df2,desired_power,pwr.anova.test)
  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)
  if(length(i) == 1){
    colnames(power_sampleSize) = paste0(ifelse(colnames(dta)[i]=="repeated1",factor_name[1],factor_name[2]),"_",c("post_hoc_Power", paste0("Sample_Size_Required_per_group_Given_Desired_Power_Equals_",desired_power*100,"_percent")))
  }else{
    colnames(power_sampleSize) = paste0("Interaction","_",c("post_hoc_Power", paste0("Total_Sample_Size_Required_per_group_Given_Desired_Power_Equals_",desired_power*100,"_percent")))
  }

  return(power_sampleSize)
}
