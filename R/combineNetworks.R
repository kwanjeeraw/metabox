#'Combine networks
#'@description The function can combine networks computed from e.g. \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \code{\link{computeCorrelation}},
#'\code{\link{computeParCorrelation}}, and \code{\link{computeSimilarity}}.
#'@usage combineNetworks(networks, returnas)
#'@param networks list of networks. Each network contains data frame of nodes and data frame of edges.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes and edges. Return empty list if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSimilarity}}
#'@examples
#'#simnw <- computeSimilarity(c(1110,10413,196,51,311,43,764,790)) #compute similarity network for given pubchem compounds
#'#dt <- data.frame(id=seq(32), mtcars, row.names = NULL) #data frame of x
#'#cornw <- computeCorrelation(x = dt, xtype = "compound", coef=0.9, pval = 1e-10) #compute correlation network
#'#result <- combineNetworks(list(simnw, cornw))
#'@export
combineNetworks <- function(networks, returnas="dataframe") UseMethod("combineNetworks")
#'@export
combineNetworks.default <- function(networks, returnas="dataframe"){
  out <- tryCatch(
    {
    cat("Formating network ...\n")
    nodels = unlist(lapply(networks,function(x) list(x$nodes)), recursive = FALSE)
    attb = plyr::rbind.fill(nodels)
    attb = attb[order(attb$nodename,decreasing = TRUE),]
    attb = attb[!duplicated(attb$id),]
    edgels = unlist(lapply(networks,function(x) list(x$edges)), recursive = FALSE)
    pair = plyr::rbind.fill(edgels)
    pair = unique(pair)
    cat("Returning network of size ",nrow(pair)," ...\n")
    ## output
    switch(returnas,
           dataframe = list(nodes=attb, edges=pair),
           list = list(nodes= split(attb, seq(nrow(attb))), edges=split(pair, seq(nrow(pair)))),
           json = list(nodes=jsonlite::toJSON(attb), edges=jsonlite::toJSON(pair)),
           stop("Error: incorrect 'returnas' type"))
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no network ..\n")
      switch(returnas,
             dataframe = list(nodes = data.frame(), edges = data.frame()),
             list = list(nodes = list(), edges = list()),
             json = list(nodes = "", edges = ""))
    })
  return(out)
}
