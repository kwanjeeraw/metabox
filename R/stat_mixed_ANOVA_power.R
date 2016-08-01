#'stat_mixed_ANOVA_power
#'@description stat_mixed_ANOVA_power
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
stat_mixed_ANOVA_power = function(e,p,dta,sig.level = 0.05, desired_power = 0.8,factor_name, epsilon = 1, cl){

  sample_size = as.numeric(table(dta$repeated1))
  m = length(sample_size)
  k = length(unique(dta$variable1))
  N = nrow(e)
  df1_r = (m - 1)*epsilon
  df2_r = (N-k)*(m-1)*epsilon #N-1, the 1 means k=1
  df1_idp = k -1
  df2_idp = N - k
  df1_inter = (k-1)*(m-1)*epsilon
  df2_inter = (N-k)*(m-1)*epsilon








  n_r = as.numeric(table(dta$repeated1))
  n_IDP = as.numeric(table(dta$variable1))
  n_inter = as.numeric(table(dta[,c("variable1","repeated1")]))

  power_sampleSize = parSapply(cl=cl,
                               X = 1:ncol(e),FUN = function(j,e,dta,m, N, n_r,n_IDP,n_inter,
                                                                      sig.level,df1_r,df2_r,df1_idp,df2_idp,df1_inter,df2_inter,desired_power,epsilon){

                                 dta$value = e[,j]

                                 Gmean = mean(dta$value,na.rm = T)
                                 Gsd = sd(dta$value,na.rm = T)


                                 rho = cor(dta$value,as.numeric(dta$repeated1))
                                 mu = m/(1-rho)

                                 sigma_mu_r = sqrt(sum((by(dta$value, dta$repeated1,mean,na.rm = T) - Gmean)^2 * sample_size/N) )
                                 sigma_r = Gsd
                                 f_r = sigma_mu_r/sigma_r
                                 ncp_r = f_r^2 * mu * N

                                 p.body_r = quote({
                                   ncp_r = f_r^2 * mu * N
                                   df2 = (N-k)*(m-1)*epsilon
                                   pf(qf(sig.level,df1_r,df2,lower.tail = F),df1_r,df2,ncp_r,lower.tail = F)
                                 })

                                 size_r = tryCatch(uniroot(function(N) eval(p.body_r) - desired_power, c(k +
                                                                                                       1e-10, 1e+05))$root,error = function(e){return("NA(Maybe Effect Size Too Small)")})




                                 sigma_mu_IDP = sqrt(sum((by(dta$value, dta$variable1, mean, na.rm = T) - Gmean)^2 * n_IDP/N) )
                                 sigma_IDP = Gsd
                                 f_IDP = sigma_mu_IDP/sigma_IDP
                                 mu_IDP = m/(1+(m-1)*rho)
                                 ncp_IDP = f_IDP^2 * mu_IDP * N * epsilon


                                 p.body_IDP = quote({
                                   ncp_IDP = f_IDP^2 * mu_IDP * N * epsilon
                                   df2 = N-k
                                  pf(qf(sig.level,df1_idp,df2,lower.tail = F),df1_idp,df2,ncp_IDP,lower.tail = F)
                                 })
                                 size_ind = tryCatch(uniroot(function(N) eval(p.body_IDP) - desired_power, c(k +
                                                                                                            1e-10, 1e+05))$root,error = function(e){return("NA(Maybe Effect Size Too Small)")})


                                 sigma_mu_inter = sqrt(sum((by(dta$value, paste(dta$variable1,dta$repeated1, sep = "*"),mean,na.rm = T) - Gmean)^2 * n_inter/N) )
                                 sigma_inter = Gsd
                                 f_inter = sigma_mu_inter/sigma_inter
                                 ncp_inter = f_inter^2 * mu * N * epsilon
                                 p.body_inter = quote({
                                   ncp_inter = f_inter^2 * mu * N * epsilon
                                   df2 = (N-k)*(m-1)*epsilon
                                   pf(qf(sig.level,df1_inter,df2,lower.tail = F),df1_inter,df2,ncp_inter,lower.tail = F)
                                 })
                                 size_inter = tryCatch(uniroot(function(N) eval(p.body_inter) - desired_power, c(k +
                                                                                                               1e-10, 1e+05))$root,error = function(e){return("NA(Maybe Effect Size Too Small)")})




                                 return(c(inter = pf(qf(sig.level,df1_inter,df2_inter,lower.tail = F),df1_inter,df2_inter,ncp_inter,lower.tail = F),size_inter,
                                          ind = pf(qf(sig.level,df1_idp,df2_idp,lower.tail = F),df1_idp,df2_idp,ncp_IDP,lower.tail = F),size_ind,
                                          r = pf(qf(sig.level,df1_r,df2_r,lower.tail = F),df1_r,df2_r,ncp_r,lower.tail = F), size_r))
                               },e,dta,m, N, n_r,n_IDP,n_inter,
                               sig.level,df1_r,df2_r,df1_idp,df2_idp,df1_inter,df2_inter,desired_power,epsilon)


  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)

  colnames(power_sampleSize) = c("Interaction_term_power",paste0("Interaction_term_Size_Required_at_Power_",desired_power*100,"_percent"),
                                 paste0(factor_name[1],"_term_power"),paste0(factor_name[1],"Size_at_Power_",desired_power*100,"_percent"),
                                 paste0(factor_name[2],"_term_power"),paste0(factor_name[2],"Size_at_Power_",desired_power*100,"_percent"))

  return(power_sampleSize)
}
