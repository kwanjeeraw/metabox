#'stat_check_hypo_test
#'@description stat_check_hypo_test
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

stat_check_hypo_test = function(e,p,f,e_ori,p_ori, # This is for mean and sd.
                                independent_factor_name = NULL, repeated_factor_name = NULL){


  if(length(repeated_factor_name)==0){
    repeated_factor_name = NULL
  }
  if(length(independent_factor_name)==0){
    independent_factor_name = NULL
  }

  factor_name = c(independent_factor_name,repeated_factor_name)
  # check if subjectID OK.
  if(is.null(repeated_factor_name)){# t test, ANOVA, two way ANOVA, subjectID should be identical to each other.
    if(sum(duplicated(p$subjectID))>0){
      return("subjectID should be identical to each other if your study doesn't evolve any repeated measure.")
    }
  }

  if(!is.null(independent_factor_name) & !is.null(repeated_factor_name)){# Mixed factor ANOVA.
    if(sum(as.numeric(by(p$subjectID,p[,repeated_factor_name],function(x){
      sum(duplicated(x))
    })))>0){
      return("subjectID conflicts with your study design.")
    }
  }


  # cannot have any group that has only 1 or 0 samples.
  temp = by(p[,1],p[,factor_name],length)
  if(sum(temp<2)>0){
    return("At least one class has no more than two samples. Please see Sample Size part above for more information. If there is no zeros or ones in Sample Size, you may have deleted
           them in Excluded Samples. Then please use above PCA score plot to visualize if there is any class that has only zero or one class AFTER you exclude the data.")
  }

if(T){
  return("GO")
}


}
