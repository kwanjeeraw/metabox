#'Format resulting nodes
#'@description format and return node information for \code{\link{fetchNode}}. Retrieve also node relationships as urls.
#'@param node a row of data frame containing node information to be formatted
#'@seealso \code{\link{fetchNode}}
#'@return node information. Return empty data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'#querystring="UNWIND ['G82952','G82949'] AS x WITH x MATCH (node:Pathway) WHERE node.GID = x RETURN DISTINCT node"
#'#node = curlRequest(cypher=querystring)
#'#formatNodeOutput(node[[1]])
formatNodeOutput <- function(node, returnas) UseMethod("formatNodeOutput")
formatNodeOutput.default <- function(node, returnas){
  attb = data.frame() #list of node attributes
  if(length(node)<30000){
    cat("Formating and returning approximately ", length(node),"nodes ...\n")
    attb = plyr::rbind.fill(lapply(node, function(x) formatNode.TRANSACTION.ALL(x$graph$nodes)))
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }else{
    cat("Found ",length(node)," but returning 50000 nodes ...\n")
    attb = plyr::rbind.fill(lapply(node[1:50000], function(x) formatNode.TRANSACTION.ALL(x$graph$nodes)))
    attb = unique(attb)
    attb = as.data.frame(lapply(attb, unlist), stringsAsFactors = FALSE)#convert df of list
  }
  
  out = switch(returnas,
               dataframe = attb,
               list = split(attb, seq(nrow(attb))),
               json = jsonlite::toJSON(attb),
               stop("incorrect return type"))
  #     switch(returnas,
  #            dataframe = nodels,
  #            list = split(nodels, seq(nrow(nodels))),
  #            json = jsonlite::toJSON(nodels),
  #            stop("Error: incorrect 'returnas' type"))
  
#   out <- tryCatch(
#     {
#       nodeInfo = node$data
#       nodeInfo$nodelabel = unlist(node$metadata$labels)
#       nodeInfo$incoming_relationships = node$incoming_relationships
#       nodeInfo$outgoing_relationships = node$outgoing_relationships
#       nodeInfo
#     },
#     error=function(e) {
#       message(e)
#       cat("\nError: RETURN no node ..\n")
#       data.frame() # Choose a return value in case of error
#     }) 
#   return(out)
}