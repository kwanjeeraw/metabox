plotlyNetwork <- function(nodeList=NULL, edgeList, nwpage="") {
  mapcolor = function(type){
    switch(type,
         "ANNOTATION" = "#8C8C8C",
         "BIOCHEMICAL_REACTION" = "red",
         "CATALYSIS" = "royalblue",
         "CONTROL" = "darkgreen",
         "CONVERSION" = "magenta",
         "GENETIC_ASSOCIATION" = "deepskyblue",
         "MOLECULAR_BINDING" = "#51ED34",
         "CORRELATION" = "#3f007d",
         "PARTIAL_CORRELATION" = "#f16913",
         "TANIMOTO_SIMILARITY" = "#662506",
         "black")
  }
  
  G = graph_from_data_frame(edgeList, directed=FALSE, vertices=nodeList)
  #G = graph_from_data_frame(mynw$edges, directed=FALSE, vertices=mynw$nodes)
  G = simplify(G, remove.loops = FALSE, edge.attr.comb = "first") #unique edges
  L = layout.auto(G) #get coordinates
  rownames(L) = V(G)$name
  vs = V(G)
  es = as.data.frame(get.edgelist(G), stringsAsFactors = FALSE)
  es = cbind(es, get.edge.attribute(G)$type)
  elcoor = apply(es, MARGIN = 1, function(x) cbind(x0 = L[x[1],1], x1 = L[x[2],1], y0 = L[x[1],2], y1 = L[x[2],2], ecol = mapcolor(x[3]))) #get edge colors
  es = cbind(es, t(elcoor))
  colnames(es) = c("source","target","type","x0","x1","y0","y1","color")
  if("coef" %in% list.edge.attributes(G)){
    es = cbind(es, get.edge.attribute(G)$coef)
    edge_shapes = apply(es, MARGIN = 1, function(x) list(opacity=0.5,type="line",line = list(color = x[8], width = expm1(abs(as.numeric(x[9])))*5.819767),
                                                         x0=as.numeric(x[4]),y0=as.numeric(x[6]),x1=as.numeric(x[5]),y1=as.numeric(x[7]))) #format edges
  }else{
    edge_shapes = apply(es, MARGIN = 1, function(x) list(opacity=0.5,type="line",line = list(color = x[8], width = 3),
                                                         x0=as.numeric(x[4]),y0=as.numeric(x[6]),x1=as.numeric(x[5]),y1=as.numeric(x[7]))) #format edges
  }
  Xn = L[,1]
  Yn = L[,2]
  nodestyle = list(size = 20, line=list(width=1))
  legendstyle = list(font = list(size = 0.2))
  if("nodename" %in% list.vertex.attributes(G)){
    network = plot_ly(type = "scatter", x = Xn, y = Yn, marker = nodestyle, mode = "markers", text = vs$nodename, hoverinfo = "text", color = vs$nodelabel, colors = "RdYlBu")
  }else{
    network = plot_ly(type = "scatter", x = Xn, y = Yn, marker = nodestyle, mode = "markers", text = vs$name, hoverinfo = "text", color = vs$nodelabel, colors = "RdYlBu")
  }
  network = layout(
    network,
    title = nwpage,
    shapes = edge_shapes,
    legend = legendstyle,
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  )
  return(network)
}