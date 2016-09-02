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
stat_repeated_ANOVA_power = function(e,p,f,dta,i, sig.level = 0.05, desired_power = 0.8,factor_name, epsilon = 1, k = 1, cl){
  # if k = 1, then it is one way repeated ANOVA.

  sample_size = as.numeric(table(dta[,i]))
  m = length(sample_size)
  N = sum(sample_size)
  df1 = (m - 1)*epsilon
  df2 = (N-k)*(m-1)*epsilon #N-1, the 1 means k=1



  power_sampleSize = parSapply(cl=cl,
                               X = 1:ncol(e),FUN = function(j,e,dta,m, N, sample_size,sig.level,df1,df2,desired_power,pwr.anova.test){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 rho = cor(dta$value,as.numeric(dta$repeated1))
                                 mu = m/(1-rho)

                                 Gmean = mean(dta$value, na.rm = T)

                                 sigma_mu = sqrt(sum((by(dta$value, dta$repeated1, mean, na.rm = T) - Gmean)^2* sample_size) /N)
                                 sigma = sd(dta$value, na.rm = T)
                                 f = sigma_mu/sigma
                                 ncp = f^2 * mu * N * epsilon

                                 p.body = quote({
                                   ncp = f^2 * mu * N * epsilon
                                   df2 = (N-k)*(m-1)*epsilon
                                   pf(qf(sig.level,df1,df2,lower.tail = F),df1,df2,ncp,lower.tail = F)
                                 })

                                 size = tryCatch(uniroot(function(N) eval(p.body) - desired_power, c(k +
                                                                                               1e-10, 1e+05))$root*1,error = function(e){return("NA")})
                                 # }


                                 return(c(pf(qf(sig.level,df1,df2,lower.tail = F),df1,df2,ncp,lower.tail = F),size))
                               },e,dta,m, N, sample_size,sig.level,df1,df2,desired_power,pwr.anova.test)
  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)

    colnames(power_sampleSize) = paste0(ifelse(colnames(dta)[i]=="repeated1",factor_name[1],factor_name[2]),"_",c("post_hoc_Power", paste0("Sample_Size_at_Power_",desired_power*100,"_percent")))


  return(power_sampleSize)
}
