enrichmentAnalyzer <- function(nodeList, pval, fc, method) {
  formatEsetId = function(x, y) {
    ind = which(y$id == x)
    x = ifelse(length(ind)>0,y$nodename[ind],x)
  }
  annols = apply(nodeList, 1, function(x) fetchNetwork(to=x["id"], fromtype = "pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
  annonws = combineNetworks(annols) #combine annotation pairs
  era = computeEnrichment(edgelist = annonws$edges[,2:1], pval = pval, fc = fc, method = input$eramethod, size=c(3,500), returnas="dataframe") #compute enrichment
  era = era[order(era$`p adj (non-dir.)`),]
  era$rank = seq(1:nrow(era))
  era$id = lapply(era$id, FUN=formatEsetId, y = annonws$nodes) #format edgelist
  return(list(attb = annonws$nodes, res = era))
}