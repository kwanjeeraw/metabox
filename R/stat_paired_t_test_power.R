#'stat_paired_t_test_power
#'@description stat_paired_t_test_power
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
stat_paired_t_test_power = function(e,f,p,dta, i, result_stat, sig.level = 0.05, desired_power = 0.8, factor_name,cl){

  sample_size = table(dta[,i])
  N = sum(sample_size)
  df =  N-1



  # which_col_Gsd = which(colnames(result_stat)%in%"Global Standard Deviation")
  # which_mu1 = which(colnames(result_stat)%in%"Global Mean")+1
  # which_mu2 = which_mu1 + 1



  power_sampleSize = parSapply(cl=cl,
                               X = 1:nrow(result_stat),FUN = function(j,e,dta,result_stat,sig.level,df,N,desired_power,pwr.t.test){
                                 # for(j in 1:nrow(result_stat)){
                                 dta$value = e[,j]
                                 reshape = tryCatch(reshape(dta, idvar = "id", timevar = "repeated1", direction = "wide"),error=function(err){
                                   reshape(dta, idvar = "id", timevar = "repeated2", direction = "wide")
                                 })
                                 reshape = sapply(reshape,function(x){
                                   as.numeric(as.character(x))
                                 })[,-1]
                                 cor = cor(reshape)
                                 rho = cor[1,2]

                                 mu_diff = abs(mean(reshape[,1] - reshape[,2]))
                                 sigma_x = sd(reshape[,1])
                                 sigma_y = sd(reshape[,2])
                                 sigma = sqrt(sigma_x^2+sigma_y^2-2*rho*sigma_x*sigma_y)
                                 d = mu_diff/sigma
                                 ncp = d*sqrt(N)


                                 size = tryCatch(pwr.t.test(d = d,sig.level=sig.level,power = desired_power,type = "paired", alternative =  "two.sided")$n*2,error = function(e){return("NA")})
                                 # }


                                 return(c(pt(qt(sig.level,df,lower.tail = F),df,ncp,lower.tail = F),size))
                               },e,dta,result_stat,sig.level,df,N,desired_power,pwr.t.test)
  power_sampleSize = t(power_sampleSize)
  power_sampleSize = data.frame(power_sampleSize)
  colnames(power_sampleSize) = paste0(ifelse(colnames(dta)[i]=="repeated1",factor_name[1],factor_name[2]),"_",c("post_hoc_Power", paste0("Total_Sample_Size_Required_per_group_Given_Desired_Power_Equals_",desired_power*100,"_percent")))



  return(power_sampleSize)
}
