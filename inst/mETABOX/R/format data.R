################
# for development.
## load resources.
# setwd("C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\tools\\Abib")
#.blah blah blah

## the example data set should be equaled to d.
################
# getlibrary("xlsx")




# functions.
## load data in any format. If data is not in a ready format, then need to be load manually.
load.data = function(file, ...){
  ### file should be: input$`inputID. function: fileInput(inputID, label, ...)
  ### DISPLAY: input$`inputID$name.
  
  ### distinguish the type of data. Usually the data is xlsx. So currently, only working on xlsx. ???
  if(grepl("xlsx", file$name)){
    d <- xlsx::read.xlsx2(file$datapath, sheetIndex = 1, ...)
    #### currently, data should be at the first sheetIndex. ???
  }
  if(grepl("csv", file$name)){
    d <- read.csv(file$datapath, ...)
  }
  
  ### guess which row(s) is (are) factor.
  d <- sapply(d, as.character)
  d[d==""] <- NA
  factor.index. = apply(d[is.na(d[,1]),], 1, function(x){
    length(unique(x))-2
  })
  # guess which row is the factor. (??? currently only support for one way analysis)
  factor.index = which(factor.index.>1&factor.index.<8)[length(which(factor.index.>1&factor.index.<8))]
  d[factor.index,1] = ""
  row.index.ViewRawData. = which(!is.na(d[,1]))
  row.index.ViewRawData <<- row.index.ViewRawData.[-2]
  col.index.ViewRawData. = which(!is.na(d[1,]))
  col.index.ViewRawData. = as.numeric(col.index.ViewRawData.[-1])
  col.index.ViewRawData <<- c(1, col.index.ViewRawData.)
  result <- d
  return(result)
}

## Use the format of each row to guess which columns are factor.
transpose.raw.data = function(rawdata){ #rawdata is columns and rows selected!
  result = t(rawdata)
  factor.idx = which(colSums(apply(result,2,function(x){
    is.na(as.numeric(x))
  }))>10)
  colnames(result) = as.character(result[1,])
  colnames(result)[factor.idx] = paste0("factor",1:length(factor.idx))### People may change the column name.???
  result = result[-1,]
  result = data.frame(result)
  result[,-factor.idx] = sapply(result[,-factor.idx], function(x){
    as.numeric(as.character(x))
  })
  return(data.frame(result))
}
## Missing value.
deal.with.missing.value = function(data, factor, 
                                   missingvalueidx, method ,tol.percent){# should be the transposrawdata() and factor.idx().
                                                              # missing.idx tells which values in data is missing.from input.
                                                              # method. default should be "none". missingvaluemethod.
  # data = data.frame(data)
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
    result = sapply(data[,-factor], function(x, method){
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
    result = t(impute.knn(t(data[,-factor]))$data)
  }else if(method == "auto-select method"){
#scale first. make all of them positive. to normal get boxcox lambda. simulate. get a pseudo. back lambda, back make positive. back scale. 
    data.ori = data
    data = data.ori
    #scale
    Medians = Diffs = vector()
    for(i in 1:ncol(data[,-factor])){
      Medians[i] = median(data[,i+length(factor)],na.rm = T)
      Diffs[i] = diff(quantile(data[,i+length(factor)],probs = c(.25,.75),na.rm = T))
      data[,i+length(factor)] = 
        (data[,i+length(factor)] - Medians[i])/Diffs[i]
    }
    #make positive.
    Mins = vector()
    for(i in 1:ncol(data[,-factor])){
      Mins[i] = min(data[,i+length(factor)], na.rm = T)
      data[,i+length(factor)] = 
        data[,i+length(factor)] - Mins[i] + 1
    }
    # get lambda
    lambda=vector()
    for(i in 1:ncol(data[,-factor])){
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
    },data[,-factor],lambda))
    
    Sigma = make.positive.definite(var(data.tran[,-factor], use = "pairwise.complete.obs"))
    data.simu = data.frame(data[,factor],mvrnorm(n = nrow(data[,-factor]), 
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
    result[is.na(data[,-factor])] = result.simu[,-factor][is.na(data[,-factor])]
    
  }else{
    "error"#!!!
  }
  
  
  
  result = data.frame(data[,factor], result)
  rownames(result) = result.rownames
  colnames(result) = result.colnames
  return(result)
}