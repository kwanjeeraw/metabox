#'Deal with missing value.
#'@description Estimate or impute the missing values.
#'
#'@usage
#'@param e should be the e, where rows are samples and columns are compounds.
#'@param missingvalueidx cells with which value in data are missing value
#'@param method can be half minimum, minimum, mean, median, Random Forest, kNN, auto-select
#'@param tol_percent above which percent of mising value, we ignore compounds
#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export

deal_with_missing_value = function(e=NULL,f=NULL,p=NULL,
                                   missingvalueidx, method, tol_percent=NULL){# should be the transposrawdata() and factor.idx().
#!!! tol_percent only is the feature direction currently. If sample contains too many missing value, can't do anything.

  result.rownames = rownames(e); result.colnames = colnames(e)

  e[e==missingvalueidx] = NA

  missing_each_feature = sapply(e, function(x) sum(is.na(x)))
  missing_each_sample = apply(e, 1,function(x) sum(is.na(x)))

  if(!is.null(tol_percent)){
    e[,missing_each_feature/ncol(e) <= tol_percent]
  }

  # Here is for imp.
  if(method%in%c("half_minimum","minimum","mean","median")){
    result = sapply(e, function(x, method){
      x[is.na(x)] =
        switch(method,
               mean = mean(x,na.rm = T),
               median = median(x,na.rm = T),
               minnimum = min(x,na.rm = T),
               "half_minimum" = 1/2 * min(x,na.rm = T),
               max  = max(x,na.rm = T))
      return(x)
    }, method = method)
  }

  # Here is for est. Est can only be applied to MCAR and MAR.
  if(method == "Random Forest"){
    result = missForest(e)$ximp
  }else if(method == "kNN"){
    result = t(impute.knn(t(e))$e)
  }else if(method == "auto-select"){ #!!! need to be studied more.
    #scale first. make all of them positive. to normal get boxcox lambda. simulate. get a pseudo. back lambda, back make positive. back scale.
    e.ori = e
    e = e.ori
    #scale
    Medians = Diffs = vector()
    for(i in 1:ncol(e)){
      Medians[i] = median(e[,i+length(factor)],na.rm = T)
      Diffs[i] = diff(quantile(e[,i+length(factor)],probs = c(.25,.75),na.rm = T))
      e[,i+length(factor)] =
        (e[,i+length(factor)] - Medians[i])/Diffs[i]
    }
    #make positive.
    Mins = vector()
    for(i in 1:ncol(e)){
      Mins[i] = min(e[,i+length(factor)], na.rm = T)
      e[,i+length(factor)] =
        e[,i+length(factor)] - Mins[i] + 1
    }
    # get lambda
    lambda=vector()
    for(i in 1:ncol(e)){
      bc = boxcox(e[,i+length(factor)]~1,plotit = F,lambda=seq(-3, 3, length = 10))
      CI = bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.95,1)]
      CI.min = min(CI); CI.max = max(CI)
      if(CI.min > 0){
        lambda[i] = bc$x[which.max(bc$y)]
      }else if(CI.max > 0){
        lambda[i] = 0
      }else{
        lambda[i] = bc$x[which.max(bc$y)]
      }
    }
    e.tran = data.frame(e[,factor],mapply(function(x,y){
      if(y==0){
        log(x)
      }else{
        (x^y-1)/y
      }
    },e,lambda))

    Sigma = make.positive.definite(var(e.tran[,-factor], use = "pairwise.complete.obs"))
    e.simu = data.frame(e[,factor],mvrnorm(n = nrow(e),
                                                 mu = colMeans(e.tran[,-factor], na.rm = T),
                                                 Sigma = Sigma))
    e.simu2 = e.simu; e.simu2[is.na(e)] = NA
    method.list = c("RF", "kNN")
    error = vector()
    error[1] = sum((e.simu[,-factor] - missForest(e.simu2[,-factor])$ximp)^2)
    error[2] = sum((e.simu[,-factor] - t(impute.knn(t(e.simu2[,-factor]))$e))^2)
    missing.value.estimation.method.selected <<- method.list[which.min(error)]
    if(missing.value.estimation.method.selected == "RF"){
      result.simu = missForest(e.simu2[, -factor])$ximp
    }
    if(missing.value.estimation.method.selected == "kNN"){
      result.simu = t(impute.knn(t(e.simu2[,-factor]))$e)
    }
    #back lambda
    result.simu = data.frame(e[,factor],mapply(function(x,y){
      (x*y+1)^(1/y)
    },data.frame(result.simu),lambda))
    #back make positive
    for(i in 1:ncol(result.simu[,-factor])){
      result.simu[,i+length(factor)] = result.simu[,i+length(factor)] + Mins[i] - 1
    }
    #back scale
    for(i in 1:ncol(result.simu[,-factor])){
      result.simu[,i+length(factor)] = result.simu[,i+length(factor)] * Diffs[i] +
        Medians[i]
    }

    result = e.ori[,-factor]
    result[is.na(e)] = result.simu[,-factor][is.na(e)]

  }else{
    "error"#!!!
  }



  result_impted = data.frame(result)
  rownames(result_impted) = result.rownames
  colnames(result_impted) = result.colnames
  result = list(data = result_impted, missing_each_feature=missing_each_feature, missing_each_sample=missing_each_sample,
                deleted_compounds = colnames(e)[missing_each_feature/ncol(e)>tol_percent])
  if(length(result[["deleted_compounds"]])==0){
    result[["deleted_compounds"]] = "none."
  }
  return(result)
}
