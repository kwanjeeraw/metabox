#'box
#'@description box
#'
#'@usage
#'@param e should be the e, where rows are samples and columns are compounds.
#'
#'@details https://plot.ly/~jackp/969/points-scored-by-the-top-50-scoring-nba-players-in-2012-each-point-is-a-game/#code
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'
#'

boxp = function(e,p,color,selected = NULL){
  name_of_box = rownames(e)
  e = as.matrix(e)
  cols_ind = gg_color_hue(length(unique(p[[color]])))
  col = vector()
  for(i in 1:nrow(e)){
    col[i]=cols_ind[unique(p[[color]])%in%p[[color]][i]]
  }

  TIC = apply(e,1,sum)
  if(length(selected)>0){
    e[is.na(e)] = 500
    pca = prcomp(e, center = T, scale. = T)
    score = pca$x
    col[!round(score[,1],4)%in%round(selected,4)] = "#CDC1C5"
  }

  data = list()
  for(i in 1:nrow(e)){
    data[[i]] = list(
      # x = rownames(e),
      y = e[i,],
      boxpoints="all",fillcolor="rgb(255, 255, 255)",jitter = .4,line = list(width=1),
      marker = list(
        line = list(width = 0),opacity = .9, size = 2,color = col[i]
      ),
      name = name_of_box[i],opacity=.99,type="box",
      showlegend = FALSE
    )
  }
  data[[length(data)+1]] = list(
    x = rownames(e),
    y = TIC,
    type="scatter"
    ,mode= 'lines'
    ,name = "TIC"
    ,marker = list(
      color= "##003566"
    )
  )
  data[[length(data)+2]] = list(
    x = rownames(e),
    y = TIC,
    type="scatter"
    ,mode= 'markers'
    ,name = "TIC"
    ,marker = list(
      color=col,
      size = 5
    ),
    showlegend = FALSE
  )

  data = jsonlite::toJSON(data,auto_unbox=T)
  layout = list(
    title = "boxplot for each subject",
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)',
    xaxis = list(
      tickangle= 45,
      tickfont= list(
        family= 'Old Standard TT, serif',
        size= 10
      )
    ),
    yaxis = list(
      type = "log",autorange = TRUE
    )
    ,shapes = list())
  # for(i in 1:(length(TIC)-1)){
  #   layout$shapes[[i]] = list(
  #     type = "line",xref="x",yref="y",x0=name_of_box[i],y0=TIC[i],x1=name_of_box[i+1],y1=TIC[i+1],
  #     line=list(
  #       color="red",width=2
  #     )
  #   )
  # }

  layout=jsonlite::toJSON(layout,auto_unbox=T)
 return(list(data = data,layout = layout))
}
