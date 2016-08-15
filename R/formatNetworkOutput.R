#'Format network
#'@description format a network or graph output from the database to the data frame format.
#'@param graph list of paths containing network information to be formatted
#'@return list of network information with the following components:
#'
#'nodes:
#'
#'\code{id} = node neo4j id
#'
#'\code{gid} = node grinn id
#'
#'\code{nodename} = node name
#'
#'\code{nodelabel} = node type
#'
#'\code{nodexref} = node cross references
#'
#'edges:
#'
#'\code{source, target} = node neo4j id
#'
#'\code{type} = relationship type
#'
#'\code{datasource} = relationship resource
#'
#'\code{properties} = relationship properties
#'
#'Return empty list if error or found nothing.
#'@note maximum no. of paths returned = 30000 paths
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \code{\link{fetchNetworkByGID}}, \code{\link{fetchHetNetworkByGID}}, \url{http://neo4j.com/docs/stable/rest-api-transactional.html}
#'@examples
#'#cypher = "MATCH ptw = (from:Compound)-[:BIOCHEMICAL_REACTION]->(:Compound) WHERE ID(from) = 0 RETURN DISTINCT ptw LIMIT 2"
#'#nw = curlRequest.TRANSACTION(cypher)
#'#result = formatNetworkOutput.default(nw)
formatNetworkOutput <- function(graph) UseMethod("formatNetworkOutput")
formatNetworkOutput.default <- function(graph){
  pair = data.frame() #list of mapped nodes
  attb = data.frame() #list of node attributes
  if(length(graph) <= 30000){
    cat("Formating and returning network of approximate size", length(graph),"...\n")
    pair = plyr::rbind.fill(lapply(graph, function(x) formatPath.TRANSACTION(x)))
    attb = plyr::rbind.fill(lapply(graph, function(x) formatNode.TRANSACTION(x$graph$nodes)))
    pair = unique(pair)
    pair = as.data.frame(lapply(pair, unlist), stringsAsFactors = FALSE)#convert df of list
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }else{
    cat("Found ",length(graph)," but returning network of size 30000...\n")
    pair = plyr::rbind.fill(lapply(graph[1:30000], function(x) formatPath.TRANSACTION(x)))
    attb = plyr::rbind.fill(lapply(graph[1:30000], function(x) formatNode.TRANSACTION(x$graph$nodes)))
    pair = unique(pair)
    pair = as.data.frame(lapply(pair, unlist), stringsAsFactors = FALSE)#convert df of list
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }
  list(nodes=attb, edges=pair)
}
