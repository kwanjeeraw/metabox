#'stat_t_test_power
#'@description stat_t_test_power
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
stat_t_test_power = function(e,dta, i, sig.level = 0.05, desired_power = 0.8, independent_factor_name,cl){

      sample_size = table(dta[,i])
      n1 = sample_size[1]; n2 = sample_size[2]
      df = n1+n2-2




      power_sampleSize = parSapply(cl=cl,
                X = 1:ncol(e),FUN = function(j,sig.level,df,n1,n2,desired_power,pwr.t.test){
                  # for(j in 1:nrow(result_stat)){
                  dta$value = e[,j]
                  d = diff(by(dta$value, dta[,i], mean, na.rm = T))/sd(dta$value)
                    ncp = d*sqrt(n1*n2/(n1+n2))

                    size = tryCatch(pwr.t.test(d = d,sig.level=sig.level,power = desired_power,type = "two.sample", alternative =  "two.sided")$n*2,error = function(e){return("NA")})
                  # }


                  return(c(pt(qt(sig.level,df,lower.tail = F),df,ncp,lower.tail = F),size))
                },sig.level,df,n1,n2,desired_power,pwr.t.test)
      power_sampleSize = t(power_sampleSize)
      power_sampleSize = data.frame(power_sampleSize)
      colnames(power_sampleSize) = paste0(ifelse(colnames(dta)[i]=="variable1",independent_factor_name[1],independent_factor_name[2]),"_",c("post_hoc_Power", paste0("Total_Sample_Size_at_Power_",desired_power*100,"_percent")))



      return(power_sampleSize)
}
