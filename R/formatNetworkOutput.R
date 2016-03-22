#'Format resulting network
#'@description format graph output from neo4j to data frame of network.
#'@param graph graph output from neo4j to be formatted
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchNetworks}}, \url{http://neo4j.com/docs/stable/rest-api-transactional.html}
#'@return relationship information with the following components:
#'\code{source, target} = node internal id
#'
#'\code{type} = relationship type
#'
#'\code{datasource} = relationship resource
#'
#'\code{properties} = relationship properties
#'
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'#cypher = "MATCH ptw = (from:Protein)-[:CONTROL]->(to:Gene) WHERE from.GID = 'P14859' RETURN DISTINCT ptw LIMIT 2"
#'#nw = curlRequest.TRANSACTION(cypher=qstring)
formatNetworkOutput <- function(graph, returnas) UseMethod("formatNetworkOutput")
formatNetworkOutput.default <- function(graph, returnas){
  pair = data.frame() #list of mapped nodes
  attb = data.frame() #list of node attributes
  if(length(graph)<30000){
    cat("Formating and returning network of approximate size", length(graph),"...\n")
    pair = plyr::rbind.fill(lapply(graph, function(x) formatPath.TRANSACTION(x)))
    attb = plyr::rbind.fill(lapply(graph, function(x) formatNode.TRANSACTION(x$graph$nodes)))
    pair = unique(pair)
    pair = as.data.frame(lapply(pair, unlist), stringsAsFactors = FALSE)#convert df of list
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }else{
    cat("Found ",length(graph)," but returning network of size 50000...\n")
    pair = plyr::rbind.fill(lapply(graph[1:50000], function(x) formatPath.TRANSACTION(x)))
    attb = plyr::rbind.fill(lapply(graph[1:50000], function(x) formatNode.TRANSACTION(x$graph$nodes)))
    pair = unique(pair)
    pair = as.data.frame(lapply(pair, unlist), stringsAsFactors = FALSE)#convert df of list
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }
  
  out = switch(returnas,
               dataframe = list(nodes=attb, edges=pair),
               list = list(nodes = split(attb, seq(nrow(attb))), edges = split(pair, seq(nrow(pair)))),
               json = list(nodes=jsonlite::toJSON(attb), edges=jsonlite::toJSON(pair)),
               stop("incorrect return type"))
  
#   out <- tryCatch(
#     {
#       #format edges
#       pathInfo = data.frame(source=graph$graph$nodes[[1]]$properties$GID, target=graph$graph$nodes[[2]]$properties$GID, type=graph$graph$relationships[[1]]$type, 
#                             sourcename=graph$graph$nodes[[1]]$properties$name, sourcelabel=graph$graph$nodes[[1]]$labels, 
#                             targetname=graph$graph$nodes[[2]]$properties$name, targetlabel=graph$graph$nodes[[2]]$labels, 
#                             datasource=unlist(graph$graph$relationships[[1]]$properties["dataSource"]), stringsAsFactors = FALSE, row.names = NULL)
#       if(is.null(graph$graph$relationships[[1]]$properties["properties"])){
#         pathInfo$properties=list(properties = "")
#       }else{
#         pathInfo$properties=graph$graph$relationships[[1]]$properties["properties"]
#       }
#       pathInfo$sourcexref=list(graph$graph$nodes[[1]]$properties$xref)
#       pathInfo$targetxref=list(graph$graph$nodes[[2]]$properties$xref)
#       pathInfo
#     },
#     error=function(e) {
#       message(e)
#       cat("\nError: RETURN no network ..\n")
#       data.frame() # Choose a return value in case of error
#     })
#   return(out)
}