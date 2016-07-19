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
stat_repeated_ANOVA_power = function(e,p,dta,result_stat, sig.level = 0.05, desired_power = 0.8,factor_name, epsilon = 1, cl){

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




  which_col_Gmean = which(colnames(result_stat)%in%"Global Mean")
  which_col_Gsd = which(colnames(result_stat)%in%"Global Standard Deviation")



  which_col_IDPmean = 2:(2+k-1)
  which_col_IDPsd = (which_col_Gsd+1):(which_col_Gsd+k)

  which_col_rmean = (2+k):(2+k+m-1)
  which_col_rsd = (which_col_Gsd+k+1):(which_col_Gsd+k+1+m-1)

  which_col_intermean = (2+k+m):(2+k+m + k*m - 1)
  which_col_intersd = (which_col_Gsd+k+1+m):ncol(result_stat)


  n_r = as.numeric(table(dta$repeated1))
  n_IDP = as.numeric(table(dta$variable1))
  n_inter = as.numeric(table(dta[,c("variable1","repeated1")]))

  power_sampleSize = parSapply(cl=cl,
                               X = 1:nrow(result_stat),FUN = function(j,e,dta,result_stat,m, N, n_r,n_IDP,n_inter,which_col_Gsd,which_col_Gmean,
                                                                      which_col_IDPmean,which_col_rmean,which_col_intermean,
                                                                      sig.level,df1_r,df2_r,df1_idp,df2_idp,df1_inter,df2_inter,desired_power,epsilon){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 reshape_r =reshape(dta[,c("value","repeated1","id")], idvar = "id", timevar = "repeated1", direction = "wide")
                                 reshape_r = sapply(reshape_r,function(x){
                                   as.numeric(as.character(x))
                                 })
                                 cor = cor(reshape_r)
                                 rho = mean(cor[lower.tri(cor)])
                                 mu = m/(1-rho)

                                 sigma_mu_r = sqrt(sum((result_stat[j,which_col_rmean] - result_stat[j,which_col_Gmean])^2 * n_r/N) )
                                 sigma_r = result_stat[j,which_col_Gsd]
                                 f_r = sigma_mu_r/sigma_r
                                 ncp_r = f_r^2 * mu * N

                                 p.body_r = quote({
                                   ncp_r = f_r^2 * mu * N
                                   pf(qf(sig.level,df1_r,df2_r,lower.tail = F),df1_r,df2_r,ncp_r,lower.tail = F)
                                 })

                                 size_r = tryCatch(uniroot(function(N) eval(p.body_r) - desired_power, c(2 +
                                                                                                       1e-10, 1e+05))$root,error = function(e){return("NA")})




                                 sigma_mu_IDP = sqrt(sum((result_stat[j,which_col_IDPmean] - result_stat[j,which_col_Gmean])^2 * n_IDP/N) )
                                 sigma_IDP = result_stat[j,which_col_Gsd]
                                 f_IDP = sigma_mu_IDP/sigma_IDP
                                 mu_IDP = m/(1+(m-1)*rho)
                                 ncp_IDP = f_IDP^2 * mu_IDP * N * epsilon
                                 p.body_IDP = quote({
                                   ncp_IDP = f_IDP^2 * mu_IDP * N * epsilon
                                   pf(qf(sig.level,df1_idp,df2_idp,lower.tail = F),df1_idp,df2_idp,ncp_IDP,lower.tail = F)
                                 })
                                 size_ind = tryCatch(uniroot(function(N) eval(p.body_IDP) - desired_power, c(2 +
                                                                                                            1e-10, 1e+05))$root,error = function(e){return("NA")})


                                 sigma_mu_inter = sqrt(sum((result_stat[j,which_col_intermean] - result_stat[j,which_col_Gmean])^2 * n_inter/N) )
                                 sigma_inter = result_stat[j,which_col_Gsd]
                                 f_inter = sigma_mu_inter/sigma_inter
                                 ncp_inter = f_inter^2 * mu * N * epsilon
                                 p.body_inter = quote({
                                   ncp_inter = f_inter^2 * mu * N * epsilon
                                   pf(qf(sig.level,df1_inter,df2_inter,lower.tail = F),df1_inter,df2_inter,ncp_inter,lower.tail = F)
                                 })
                                 size_inter = tryCatch(uniroot(function(N) eval(p.body_inter) - desired_power, c(2 +
                                                                                                               1e-10, 1e+05))$root,error = function(e){return("NA")})

                                 # }


                                 return(c(inter = pf(qf(sig.level,df1_inter,df2_inter,lower.tail = F),df1_inter,df2_inter,ncp_inter,lower.tail = F),size_inter,
                                          ind = pf(qf(sig.level,df1_idp,df2_idp,lower.tail = F),df1_idp,df2_idp,ncp_IDP,lower.tail = F),size_ind,
                                          r = pf(qf(sig.level,df1_r,df2_r,lower.tail = F),df1_r,df2_r,ncp_r,lower.tail = F), size_r))
                               },e,dta,result_stat,m, N, n_r,n_IDP,n_inter,which_col_Gsd,which_col_Gmean,
                               which_col_IDPmean,which_col_rmean,which_col_intermean,
                               sig.level,df1_r,df2_r,df1_idp,df2_idp,df1_inter,df2_inter,desired_power,epsilon)


  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)

  colnames(power_sampleSize) = c("Interaction_term_power",paste0("Interaction_term_Size_Required_When_Desired_Power_is",desired_power),
                                 paste0(factor_name[1],"_term_power"),paste0(factor_name[1],"Size_Required_When_Desired_Power_is",desired_power),
                                 paste0(factor_name[2],"_term_power"),paste0(factor_name[2],"Size_Required_When_Desired_Power_is",desired_power))

  return(power_sampleSize)
}
