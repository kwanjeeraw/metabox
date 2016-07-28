#'Format node information
#'@description format and return node information for \code{\link{fetchNode}}.
#'@param node list of nodes containing node information or node attributes to be formatted
#'@return data frame of node information. Return no data frame if error or found nothing.
#'@note maximum no. of nodes returned = 30000 nodes
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{fetchNode}}
#'@examples
#'#querystring="UNWIND [0,1] AS x WITH x MATCH (node) WHERE ID(node) = x RETURN DISTINCT node"
#'#node = curlRequest.TRANSACTION(cypher=querystring)
#'#formatNodeOutput(node)
formatNodeOutput <- function(node, returnas){
  attb = data.frame() #data frame of node attributes
  if(length(node) <= 30000){
    #cat("Formating and returning approximately ", length(node),"nodes ...\n")
    attb = plyr::rbind.fill(lapply(node, function(x) formatNode.TRANSACTION.ALL(x$graph$nodes)))
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }else{
    cat("Found ",length(node)," but returning only 30000 nodes ...\n")
    attb = plyr::rbind.fill(lapply(node[1:30000], function(x) formatNode.TRANSACTION.ALL(x$graph$nodes)))
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }
  out = switch(returnas,
               dataframe = attb,
               list = split(attb, seq(nrow(attb))),
               json = jsonlite::toJSON(attb),
               stop("incorrect return type"))
}
