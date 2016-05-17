#'test
#'@description test
#'
#'@usage
#'@param e should be the e, where rows are samples and columns are compounds.
#'
#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
histo = function(p,sample_information_selection,
                 e, #if x is not null, then it knows that it should calculate all the pca score thing.
                 x=NULL){
  for(i in sample_information_selection){
    p[[i]] = as.factor(p[[i]])
  }
  if(length(x)>0){
    e[is.na(e)] = 500
    pca = prcomp(e, center = T, scale. = T)
    score = pca$x
    p = p[round(score[,1],4)%in%round(x,4),]
  }
   temp = sapply(sample_information_selection, function(x){
    o = table(p[[x]])
    Y = as.vector(o)
    col = rep('rgba(204,204,204,1)',length(o))
    col[which.max(Y)] = 'rgba(222,45,38,0.8)'
    list(x = names(o),
         y = Y,
         marker = list(color= col),
         type="bar")
  }, simplify = F)
  result = list()
  for(i in 1:length(temp)){
    result[[i]] = temp[[i]]
    result[[i]]$"xaxis" = paste0("x",i)
    result[[i]]$"yaxis" = paste0("y",i)
  }
  data = jsonlite::toJSON(result,auto_unbox=T)
  layout = list()
  total = length(sample_information_selection)
  split = ceiling(total/2)
  layout = list(
showlegend = FALSE,
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                annotations = list())
  for(i in 1:length(sample_information_selection)){
    layout[[2 * i+4]] = list(
                           domain=c(0.5-(i%%2)*0.5 + as.numeric(!(i%%2))*0.05, 0.45-((i%%2)-1)*0.5 + as.numeric(!(i%%2))*0.05)
                           ,anchor=paste0("y",i))
    layout[[2 * i+5]] =  list(
                           domain = c(((ceiling(i/2) - 1)/split)*1.1, (ceiling(i/2)/split)*0.95)
                           ,anchor=paste0("x",i))
    layout[["annotations"]][[i]] = list(x=1/2*(abs(diff(c(0.5-(i%%2)*0.5 + as.numeric(!(i%%2))*0.05,
                                                     0.45-((i%%2)-1)*0.5 + as.numeric(!(i%%2))*0.05)))) +
                                          0.5-(i%%2)*0.5 + as.numeric(!(i%%2))*0.05,
                                y=(ceiling(i/2)/split)*0.97,
                                showarrow=FALSE,text=sample_information_selection[i],
                                xanchor="center",xref="paper",yanchor="bottom",yref="paper",
                                font = list(
                                  color= 'black'
                                ))
    names(layout)[2 * i+4] = paste0("xaxis",i);names(layout)[2 * i+5] = paste0("yaxis",i)
  }
  layout = jsonlite::toJSON(layout,auto_unbox=T)
  return(list(data = data, layout = layout))
}

