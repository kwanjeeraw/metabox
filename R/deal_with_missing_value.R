#'Deal with missing value.
#'@description Estimate or impute the missing values.
#'
#'@usage
#'@param data should be the e, where rows are samples and columns are compounds.
#'@param missingvalueidx cells with which value in data are missing value
#'@param method can be half minimum, minimum, mean, median, Random Forest, kNN, auto-select
#'@param tol.percent above which percent of mising value, we ignore compounds
#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export

deal_with_missing_value = function(data,
                                   missingvalueidx, method, tol.percent){# should be the transposrawdata() and factor.idx().


  result.rownames = rownames(data); result.colnames = colnames(data)

  data[data==as.numeric(missingvalueidx)] = NA
  if(!is.null(tol.percent)){
    too.many.missing = sapply(data, function(x){
      sum(is.na(x))
    })/nrow(data) > tol.percent/100
    data = data[,!too.many.missing] # delete features with a lot of mising values.
    result.colnames = result.colnames[!too.many.missing]
  }

  # Here is for imp.
  if(method%in%c("half minimum","minimum","mean","median")){
    result = sapply(data, function(x, method){
      x[is.na(x)] =
        switch(method,
               mean = mean(x,na.rm = T),
               median = median(x,na.rm = T),
               minnimum = min(x,na.rm = T),
               "half minimum" = 1/2 * min(x,na.rm = T),
               max  = max(x,na.rm = T))
      return(x)
    }, method = method)
  }

  # Here is for est. Est can only be applied to MCAR and MAR.
  if(method == "Random Forest"){
    result = missForest(data[, -factor])$ximp
  }else if(method == "kNN"){
    result = t(impute.knn(t(data))$data)
  }else if(method == "auto-select"){
    #scale first. make all of them positive. to normal get boxcox lambda. simulate. get a pseudo. back lambda, back make positive. back scale.
    data.ori = data
    data = data.ori
    #scale
    Medians = Diffs = vector()
    for(i in 1:ncol(data)){
      Medians[i] = median(data[,i+length(factor)],na.rm = T)
      Diffs[i] = diff(quantile(data[,i+length(factor)],probs = c(.25,.75),na.rm = T))
      data[,i+length(factor)] =
        (data[,i+length(factor)] - Medians[i])/Diffs[i]
    }
    #make positive.
    Mins = vector()
    for(i in 1:ncol(data)){
      Mins[i] = min(data[,i+length(factor)], na.rm = T)
      data[,i+length(factor)] =
        data[,i+length(factor)] - Mins[i] + 1
    }
    # get lambda
    lambda=vector()
    for(i in 1:ncol(data)){
      bc = boxcox(data[,i+length(factor)]~1,plotit = F,lambda=seq(-3, 3, length = 10))
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
    data.tran = data.frame(data[,factor],mapply(function(x,y){
      if(y==0){
        log(x)
      }else{
        (x^y-1)/y
      }
    },data,lambda))

    Sigma = make.positive.definite(var(data.tran[,-factor], use = "pairwise.complete.obs"))
    data.simu = data.frame(data[,factor],mvrnorm(n = nrow(data),
                                                 mu = colMeans(data.tran[,-factor], na.rm = T),
                                                 Sigma = Sigma))
    data.simu2 = data.simu; data.simu2[is.na(data)] = NA
    method.list = c("RF", "kNN")
    error = vector()
    error[1] = sum((data.simu[,-factor] - missForest(data.simu2[,-factor])$ximp)^2)
    error[2] = sum((data.simu[,-factor] - t(impute.knn(t(data.simu2[,-factor]))$data))^2)
    missing.value.estimation.method.selected <<- method.list[which.min(error)]
    if(missing.value.estimation.method.selected == "RF"){
      result.simu = missForest(data.simu2[, -factor])$ximp
    }
    if(missing.value.estimation.method.selected == "kNN"){
      result.simu = t(impute.knn(t(data.simu2[,-factor]))$data)
    }
    #back lambda
    result.simu = data.frame(data[,factor],mapply(function(x,y){
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

    result = data.ori[,-factor]
    result[is.na(data)] = result.simu[,-factor][is.na(data)]

  }else{
    "error"#!!!
  }



  result = data.frame(data[,factor], result)
  rownames(result) = result.rownames
  colnames(result) = result.colnames
  return(result)
}
