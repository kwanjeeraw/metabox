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

stat_basic_statistics = function(e,f,p,independent_factor_name = NULL, repeated_factor_name = NULL){
  if(length(repeated_factor_name)==0){
    repeated_factor_name = NULL
  }
  if(length(independent_factor_name)==0){
    independent_factor_name = NULL
  }

  factor_name = c(independent_factor_name,repeated_factor_name)

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

  if(ncol(dta)==3){#one way (repeated) ANOVA
    result_stat = matrix(nrow = ncol(e),ncol = (sum((length(unique(dta[,2]))-1):1) + 1 )* 2)#fdr
    for(i in 1:ncol(e)){
      dta$value = e[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+length(unique(dta[,2])))] = by(dta$value, dta[,2],mean,na.rm = T)
      result_stat[i,(1+length(unique(dta[,2])))+1] = sd(dta$value,na.rm = T)
      result_stat[i,((1+length(unique(dta[,2])))+2):ncol(result_stat)] = by(dta$value, dta[,2],sd,na.rm = T)
    }
    result_stat = data.frame(result_stat)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],sd,na.rm = T))))
    return(result_stat)
  }else{

    result_stat = matrix(nrow = ncol(e),ncol = (1+length(unique(dta[,2])) + length(unique(dta[,3])) +
                      length(unique(dta[,2])) *length(unique(dta[,3])))*2)
    for(i in 1:ncol(e)){
      dta$value = e[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(2 + length(unique(dta[,2]))-1)] = by(dta$value, dta[,2], mean, na.rm = T)
      result_stat[i,(2 + length(unique(dta[,2]))):((2 + length(unique(dta[,2]))) + length(unique(dta[,3]))-1)] = by(dta$value, dta[,3], mean, na.rm = T)
      result_stat[i,((2 + length(unique(dta[,2]))) + length(unique(dta[,3]))):(ncol(result_stat)/2)] = by(dta$value, dta[,2:3], mean, na.rm = T)

      result_stat[i,1 + (ncol(result_stat)/2)] = sd(dta$value,na.rm = T)
      result_stat[i,(2+(ncol(result_stat)/2)):(2 + length(unique(dta[,2]))-1+(ncol(result_stat)/2))] = by(dta$value, dta[,2], sd, na.rm = T)
      result_stat[i,(2 + length(unique(dta[,2])) + (ncol(result_stat)/2)):((2 + length(unique(dta[,2]))) + length(unique(dta[,3]))-1 + (ncol(result_stat)/2))] =
        by(dta$value, dta[,3], sd, na.rm = T)
      result_stat[i,((2 + length(unique(dta[,2]))) + length(unique(dta[,3])) + (ncol(result_stat)/2)):(ncol(result_stat)/2 + (ncol(result_stat)/2))] =
        by(dta$value, dta[,2:3], sd, na.rm = T)
    }

    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))), paste("Mean of", names( by(dta$value, dta[,3],mean,na.rm = T))),
                              paste("Mean of", sapply(levels(dta[,3]),function(x){
                                paste(x,levels(dta[,2]), sep = " in ")
                              })),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],mean,na.rm = T))), paste("Standard Deviation of", names( by(dta$value, dta[,3],mean,na.rm = T))),
                              paste("Standard Deviation of", sapply(levels(dta[,3]),function(x){
                                paste(x,levels(dta[,2]), sep = " in ")
                              })))






  }

}
