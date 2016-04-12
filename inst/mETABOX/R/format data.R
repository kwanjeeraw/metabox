################
# for development.
## load resources.
# setwd("C:\\Users\\Sili Fan\\Documents\\GitHub\\mETABOX\\data\\example datasets for statistics")
# d <- read.xlsx2("mx 69088 C\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx",sheetIndex = 1,
#                 as.data.frame=T, stringsAsFactors = FALSE)
# d <- read.csv("mx 69088 C\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.csv", stringsAsFactors = FALSE)
# e <- read.xlsx2("mx 69088 C\\e.xlsx", sheetIndex = 1,as.data.frame=T,stringsAsFactors = FALSE)
# f <- read.xlsx2("mx 69088 C\\f.xlsx", sheetIndex = 1,as.data.frame=T,stringsAsFactors = FALSE)
# p <- read.xlsx2("mx 69088 C\\p.xlsx", sheetIndex = 1,as.data.frame=T,stringsAsFactors = FALSE)
## the example data set should be equaled to d.
################





# functions.
## Data Uploading.
### Upload aggregated data.
load.aggregated.data = function(file, ...){ # returns a expression data frame(eData),
                                            # feature information data.frame(fData),
                                            # phenotype of samples data.frame(pData);
  ### file should be: input$`inputID. fileInput(inputID, ...)
  ### DISPLAY: input$`inputID$name.

  ### distinguish the type of data. Usually the data is xlsx. So currently, only working on xlsx but not xls. ???
  if(grepl("xlsx", file$name)){
    #### currently, data should be at the first sheetIndex. ???
    d <- xlsx::read.xlsx2(file$datapath, sheetIndex = 1, stringsAsFactors = FALSE, ...)
  }
  if(grepl("csv", file$name)){
    d <- read.csv(file$datapath, stringsAsFactors = FALSE, ...)
  }
  d[d==""] <- NA
  #### fData
  fData <- d[!is.na(d[,1]),c(which(is.na(d[1,])),sum(is.na(d[1,]))+1)] # The first row and column is critical of formating the data.
  colnames(fData) = fData[1,]; fData = data.frame(fData[-1,],stringsAsFactors = F);rownames(fData) = 1:nrow(fData);
  #### pData
  pData <- d[is.na(d[,1]),!is.na(d[1,])]
  pData <- t(pData); colnames(pData) = pData[1,]; pData = data.frame(pData[-1,],stringsAsFactors = F)
  #### eData
  eData <- d[!is.na(d[,1]),!is.na(d[1,])][-1,-1]
  eData <- sapply(eData, as.numeric)
  colnames(eData) = rownames(pData); rownames(eData) = fData[,1]
  eData <- data.frame(t(eData),stringsAsFactors = F)
  result <- list("expresson" = eData, "feature" = fData, "phenotype" = pData)
  return(result)
}
### Upload expression data.
load.expression.data = function(file,...){
  if(grepl("xlsx", file$name)){
    #### currently, data should be at the first sheetIndex. ???
    e <- xlsx::read.xlsx2(file$datapath, sheetIndex = 1, stringsAsFactors = FALSE, ...)
  }
  if(grepl("csv", file$name)){
    e <- read.csv(file$datapath, stringsAsFactors = FALSE, ...)
  }
  e = t(e)
  colnames = e[1,];e = e[-1,];rownames = rownames(e);e = data.frame(e,stringsAsFactors = F);e = sapply(e, as.numeric);e = data.frame(e,stringsAsFactors = F)
  colnames(e) = colnames;e = data.frame(e);rownames(e)=rownames
  return(e)
}
### Upload feature data.
load.feature.data = function(file,...){
  if(grepl("xlsx", file$name)){
    #### currently, data should be at the first sheetIndex. ???
    f <- xlsx::read.xlsx2(file$datapath, sheetIndex = 1, stringsAsFactors = FALSE, ...)
  }
  if(grepl("csv", file$name)){
    f <- read.csv(file$datapath, stringsAsFactors = FALSE, ...)
  }
  return(f)
}
### Upload phenotype data.
load.phenotype.data = function(file,...){
  if(grepl("xlsx", file$name)){
    #### currently, data should be at the first sheetIndex. ???
    p <- xlsx::read.xlsx2(file$datapath, sheetIndex = 1, stringsAsFactors = FALSE, ...)
  }
  if(grepl("csv", file$name)){
    p <- read.csv(file$datapath, stringsAsFactors = FALSE, ...)
  }
  return(p)
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
