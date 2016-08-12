#'hypothesis test
#'@description hypothesis test
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examplesz
#'@export
#


stat_Boxplot_generator = function(e,p,f,
                                  e_ori,p_ori,
                                  independent_factor_name = NULL, repeated_factor_name = NULL,
                                  confound = NULL,
                                  equal_variance_anova = F){
  if(length(repeated_factor_name)==0){
    repeated_factor_name = NULL
  }
  if(length(independent_factor_name)==0){
    independent_factor_name = NULL
  }



  factor_name = c(independent_factor_name,repeated_factor_name)
  # e = e[-c(1:6),] #temp!
  # p = p[-c(1:6),] #temp!

  dta = data.frame(value = e[,1], p[factor_name[!factor_name%in%repeated_factor_name]],p[repeated_factor_name])
  if(is.null(repeated_factor_name)){
    colnames(dta) = c("value", paste0("variable",1:sum(!factor_name%in%repeated_factor_name)))
  }else if(sum(!factor_name%in%repeated_factor_name)==0){
    colnames(dta) = c("value", paste0("repeated",1:sum(length(repeated_factor_name))))
  }else{
    colnames(dta) = c("value",paste0("variable",1:sum(!factor_name%in%repeated_factor_name)),paste0("repeated",1:length(repeated_factor_name)))
  }
  dta$id = 1:nrow(dta)#!!!
  # dta$id = p$subjectID

  for(i in 2:ncol(dta)){
    dta[,i] = factor(dta[,i])
  }
  colnames(dta)[2:(2+length(factor_name)-1)] = factor_name
for(i in 1:ncol(e)){
  dta$value = e[,i]
  boxplot(dta$value~dta[,2])
}



}
