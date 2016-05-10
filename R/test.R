#'test
#'@description test
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

test = function(e, f, p, color, pie_options=c("time","treatment_1")){

  e[is.na(e)] = 500
  pca = prcomp(e, center = T, scale. = T)
  score = pca$x
  score = data.frame(score, p);rownames(score) = rownames(data)
  # plotly::plot_ly(data = score, x = PC1, y = PC2, mode = "markers",
  # color = p[[color]])
result_scatter = paste(by(score, p[[color]], FUN = function(x){
  paste0("{x:[",paste(x[,1],collapse=","),"],y:[",paste(x[,2],collapse=","),"],mode:'markers',type:'scatter',name:'",x[[color]][1],"'}")
}),collapse = ",")
result_scatter = paste0(c("[",result_scatter,"]"), collapse = "")
# now adding the pie chart! using p[,1]
if(is.null(pie_options)){
  result_pie = NULL
}else{
  temp_pie = sapply(p[pie_options], function(x){
    table(x)
  })
  result_pie = paste(apply(temp_pie,2, function(x){
    num = which(apply(temp_pie, 2,function(y){names(y)[1]%in%names(x)[1]}))
    paste0("{values:[",
           paste(round(x/sum(x)*100),collapse=","),"],labels:['",
           paste(names(x),collapse="','"),"'],type:'pie',domain:{x:[",
           paste(c((num-1)*.5, num*.5), collapse = ","),"]}}")
  }),collapse=",")
  result_pie = paste0(c("[",result_pie,"]"), collapse = "")
  result_pie_layout = NULL
}

# [{values:[31,23,23,23],labels:['17hr','1hr','24hr','6hr'],type:'pie',domain:{x:[0,0.5]}}]


# result = paste(c("[",result_scatter,",",result_pie,"]"),collapse = "")
result_hist = NULL
  return(
    list(scatter = result_scatter, pie = result_pie, pie_layout = result_pie_layout,
         hist = result_hist)
    #"[{x:[1, 2, 3, 4, 5],y:[1, 2, 4, 8, 16]}]"
  )

  # plot(score$PC1, score$PC2)

}
