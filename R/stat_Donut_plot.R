#'Donut_plot
#'@description Donut_plot
#'
#'@usage
#'@param e should be the e, where rows are samples and columns are compounds.

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
stat_Donut_plot = function(e,f,p,p_column,selected_sample){
  #selected_sample is actually the score value of PCA.
  e = as.matrix(e)
    pca = prcomp(e, center = F, scale. = F)
    score = pca$x
    p_temp = tryCatch(p[round(score[,1],4)%in%round(selected_sample,4),],error = function(e){
      stop("No points selected from PCA plot.")
    })

    table = table(p_temp[,p_column])
    names = names(table)
    values = as.numeric(table)
    result = list()
    for(i in 1:length(unique(p_temp[,p_column]))){
      result[[i]] = list(
        label = names[i],
        value = values[i]
      )
    }
    return(list(data = jsonlite::toJSON(result,auto_unbox=T)))
}
