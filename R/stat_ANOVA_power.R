#'stat_ANOVA_power
#'@description stat_ANOVA_power
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
stat_ANOVA_power = function(e,dta,i,sig.level = 0.05, desired_power = 0.8,independent_factor_name, cl){

  sample_size = as.numeric(table(dta[,i]))
  k = length(sample_size)
  N = sum(sample_size)
  df1 = k - 1
  df2 = N-k


  power_sampleSize = parSapply(cl=cl,
                               X = 1:ncol(e),FUN = function(j,k, N, sample_size,sig.level,df1,df2,desired_power,pwr.anova.test){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 Gmean = mean(dta$value,na.rm = T)
                                 sigma_mu = sqrt(sum((by(dta$value, dta[,i], mean, na.rm = T) - Gmean)^2 * sample_size/N) )
                                 sigma = sd(dta$value,na.rm = T)
                                 f = sigma_mu/sigma
                                 ncp = f^2*N
                                 size = tryCatch(pwr.anova.test(k=k,f = f,sig.level=sig.level,power = desired_power)$n*k,error = function(e){return("NA")})
                                 # }


                                 return(c(pf(qf(sig.level,df1,df2,lower.tail = F),df1,df2,ncp,lower.tail = F),size))
                               },k, N, sample_size,sig.level,df1,df2,desired_power,pwr.anova.test)
  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)
  if(length(i) == 1){
    colnames(power_sampleSize) = paste0(ifelse(colnames(dta)[i]=="variable1",independent_factor_name[1],independent_factor_name[2]),"_",c("post_hoc_Power", paste0("Sample_Size_at_Power_",desired_power*100,"_percent")))
  }else{
    colnames(power_sampleSize) = paste0("Interaction","_",c("post_hoc_Power", paste0("Total_Sample_Size_at_Power_",desired_power*100,"_percent")))
  }

  return(power_sampleSize)
}
