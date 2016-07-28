#'stat_two_way_ANOVA_power
#'@description stat_two_way_ANOVA_power
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
stat_two_way_ANOVA_power = function(e,dta,sig.level = 0.05, desired_power = 0.8,independent_factor_name, cl){

  sample_size_v1 = as.numeric(table(dta$variable1))
  sample_size_v2 = as.numeric(table(dta$variable2))
  sample_size_v12 = as.numeric(table(dta$variable1,dta$variable2))

  k1 = length(unique(dta$variable1))
  k2 = length(unique(dta$variable2))
  N = nrow(dta)
  df1_v1 = k1-1
  df1_v2 = k2-1
  df1_v12= (k1-1)*(k2-1)
  df2 = N - k1*k2






  power_sampleSize = parSapply(cl=cl,
                               X = 1:ncol(e),FUN = function(j,dta,e,k1,k2, N, sample_size_v1,sample_size_v2,sample_size_v12,sig.level,df1_v1,df1_v2,df1_v12,df2,desired_power,pwr.anova.test){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 Gmean = mean(dta$value,na.rm = T)
                                 sigma = sd(dta$value,na.rm = T)

                                 #v1
                                 sigma_mu_v1= sqrt(sum((by(dta$value, dta$variable1, mean, na.rm = T) - Gmean)^2 * sample_size_v1/N))
                                 f_v1 = sigma_mu_v1/sigma
                                 ncp_v1 = f_v1^2*N

                                 p.body_v1 = quote({
                                   ncp_v1 = f_v1^2*N
                                   df2 = N - k1*k2
                                   pf(qf(sig.level,df1_v1,df2,lower.tail = F),df1_v1,df2,ncp_v1,lower.tail = F)
                                 })

                                 size_v1 = tryCatch(uniroot(function(N) eval(p.body_v1) - desired_power, c(k1*k2+1e-10, 1e+05))$root,error = function(e){return("NA (Maybe Effect Size Too Small)")})

                                 #v2
                                 sigma_mu_v2= sqrt(sum((by(dta$value, dta$variable2, mean, na.rm = T) - Gmean)^2 * sample_size_v2/N))
                                 f_v2 = sigma_mu_v2/sigma
                                 ncp_v2 = f_v2^2*N

                                 p.body_v2 = quote({
                                   ncp_v2 = f_v2^2*N
                                   df2 = N - k1*k2
                                   pf(qf(sig.level,df1_v2,df2,lower.tail = F),df1_v2,df2,ncp_v2,lower.tail = F)
                                 })

                                 size_v2 = tryCatch(uniroot(function(N) eval(p.body_v2) - desired_power, c(k1*k2+1e-10, 1e+05))$root,error = function(e){return("NA (Maybe Effect Size Too Small)")})



                                 # v12
                                 sigma_mu_v12= sqrt(sum((by(dta$value, paste(dta$variable1,dta$variable2, sep = "*"), mean, na.rm = T) - Gmean)^2 * sample_size_v12/N))
                                 f_v12 = sigma_mu_v12/sigma
                                 ncp_v12 = f_v12^2*N

                                 p.body_v12 = quote({
                                   ncp_v12 = f_v12^2*N
                                   df2 = N - k1*k2
                                   pf(qf(sig.level,df1_v12,df2,lower.tail = F),df1_v12,df2,ncp_v12,lower.tail = F)
                                 })

                                 size_v12 = tryCatch(uniroot(function(N) eval(p.body_v12) - desired_power, c(k1*k2+1e-10, 1e+05))$root,error = function(e){return("NA (Maybe Effect Size Too Small)")})

                                  # }



                                 return(c(pf(qf(sig.level,df1_v1,df2,lower.tail = F),df1_v1,df2,ncp_v1,lower.tail = F),size_v1,
                                        pf(qf(sig.level,df1_v2,df2,lower.tail = F),df1_v2,df2,ncp_v2,lower.tail = F),size_v2,
                                        pf(qf(sig.level,df1_v12,df2,lower.tail = F),df1_v12,df2,ncp_v2,lower.tail = F),size_v12
                                        ))
                               },dta,e,k1,k2, N, sample_size_v1,sample_size_v2,sample_size_v12,sig.level,df1_v1,df1_v2,df1_v12,df2,desired_power,pwr.anova.test)





  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)
    colnames(power_sampleSize) = c(paste0(independent_factor_name[1],"_",c("post_hoc_Power", paste0("Sample_Size_at_Power_",desired_power*100,"_percent"))),
                                   paste0(independent_factor_name[2],"_",c("post_hoc_Power", paste0("Sample_Size_at_Power_",desired_power*100,"_percent"))),
                                   paste0("interaction","_",c("post_hoc_Power", paste0("Sample_Size_at_Power_",desired_power*100,"_percent"))))


  return(power_sampleSize)
}
