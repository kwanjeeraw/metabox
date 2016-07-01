#'format bad sequence of DunnTest
#'@description format bad sequence of DunnTest
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

stat_cure_Dunn_format = function(x,sudo_matrix, temp){ # sudo_matrix is not going to be used but use temp.
  if(ncol(temp)>1){
    result = temp
    name_vector = list()
    for(i in 1:nrow(x)){
      name_vector[[i]] = strsplit(rownames(x)[i],split = " - ")[[1]]
    }
    for(i in 1:length(name_vector)){
      result[which(grepl(name_vector[[i]][1],rownames(temp))),which(grepl(name_vector[[i]][2],colnames(temp)))] =
        x[i]
    }


    return(result)
  }else if(ncol(temp)==1){
    result = temp
    for(i in 1:nrow(result)){
      result[i,1] = x[which(rownames(x)%in%rownames(temp)[i]),1]
    }

    return(result)

  }else{
    stop("check stat_cure_Dunn_format")
  }



}
