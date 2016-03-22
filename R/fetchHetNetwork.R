#'Query the heterogeneous networks of several relationship types from the graph database
#'@description query the networks for the given source and/or target nodes and relationship pattern
#'see \url{http://grinnhomepage} for the database structure to construct the network query.
#'@usage fetchHetNetwork(from, to, pattern, returnas)
#'@param from a character vector of source nodes used for the query e.g. from = c('id1', 'id2').
#'The value must be neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param to a character vector of target nodes used for the query e.g. to = c('id1', 'id2').
#'The value must be neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param pattern a string specifying the relationship pattern used for the query.
#'
#'Naming a node as \code{from} is to specify the source node used for the query \code{(from:Nodetype)}.
#'Naming a node as \code{to} is to specify the target node used for the query \code{(to:Nodetype)}.
#'None specific nodes are described by a pair of parentheses \code{(:Nodetype)}.
#'
#'To describe a relationship type, you can specify with a pair of square brackets and the arrow as \code{-[:RELATIONSHIP_TYPE]->}
#'
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details The function queries for the heterogeneous network containing the given relationship pattern between the source nodes and/or target nodes based on the database structure. 
#'This function can query for the ralationship of length > 1. Use \code{\link{fetchNetwork}} to query for a network of length = 1.
#'@return 
#'list of nodes with the following components:
#'
#'\code{id} = node internal neo4j id
#'
#'\code{gid} = node grinn id
#'
#'\code{nodename} = node name
#'
#'\code{nodelabel} = node type
#'
#'\code{nodexref} = node cross references
#'
#'list of edges with the following components:
#'
#'\code{source, target} = node internal neo4j id
#'
#'\code{type} = relationship type
#'
#'\code{datasource} = relationship resource
#'
#'\code{properties} = relationship properties
#'
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{convertId}}, \code{\link{fetchNetwork}}, \url{http://grinnhomepage}
#'@examples
#'# Query the network of protein-CONTROL-gene-CONVERSION-protein-CATALYSIS-compound
#'#from = list('30777','30741','28116')
#'#to = c("343","285","255")
#'#pattern = "(from:Protein)-[:CONTROL]->(to:Gene)-[:CONVERSION]->(:Protein)-[:CATALYSIS]->(:Compound)"
#'#result = fetchHetNetwork(from, to, pattern)
#'#library(igraph)
#'#plot(graph.data.frame(result[,c(1,4)], directed=FALSE))
#'#from = c('2','5')
#'#to = '4'
#'#pattern = "(from:Protein)-[:MOLECULAR_BINDING]->(to:Protein)"
#'#result = fetchHetNetwork(from, to, pattern)
#'#from = list('30777','30741','28116')
#'#to = c("343","285","255")
#'#pattern = "(from:Protein)-[:CONTROL]->(to:Gene)"
#'#result = fetchHetNetwork(from, to, pattern)
#'@export
fetchHetNetwork <- function(from=NULL, to=NULL, pattern, returnas="dataframe") UseMethod("fetchHetNetwork")
#'@export
fetchHetNetwork.default <- function(from=NULL, to=NULL, pattern, returnas="dataframe"){ 
  out <- tryCatch(
    {
      if (!is.character(pattern)) stop("argument 'pattern' must be a character vector")
      
      #construct query
      maxkw = 500 #maximum keywords
      doPar = FALSE
      if (!is.null(from) && !is.null(to)) {
        querystring = pathsList["fromto"]
        doPar = TRUE #do loop
        from = unique(stringr::str_trim(unlist(from))) #remove whiteline, duplicate
        from = from[!is.na(suppressWarnings(as.numeric(from)))] #remove string, ID accepts integer only
        to = unique(stringr::str_trim(unlist(to))) #remove whiteline, duplicate
        to = to[!is.na(suppressWarnings(as.numeric(to)))] #remove string, ID accepts integer only
      }else if (!is.null(from) && is.null(to)) {
        querystring = pathsList["from"]
        txtinput = unique(stringr::str_trim(unlist(from))) #remove whiteline, duplicate
        txtinput = txtinput[!is.na(suppressWarnings(as.numeric(txtinput)))] #remove string, ID accepts integer only
        len = length(txtinput)
      }else if (is.null(from) && !is.null(to)) {
        querystring = pathsList["to"]
        txtinput = unique(stringr::str_trim(unlist(to))) #remove whiteline, duplicate
        txtinput = txtinput[!is.na(suppressWarnings(as.numeric(txtinput)))] #remove string, ID accepts integer only
        len = length(txtinput)
      }else{
        stop("Error: No query provided")
      }
      querystring = gsub("relpattern", pattern, querystring)
      
      cat("Querying network ...\n")
      if(!doPar){
        if(len <= maxkw){
          qstring = gsub("keyword", paste0("['",paste0(txtinput, collapse = "','"),"']"), querystring)
        cat(qstring,"\n")      
          paths = curlRequest.TRANSACTION(cypher=qstring)  
          #paths = jsonlite::fromJSON(unlist(curlRequest.json(cypher=qstring), recursive = FALSE))$data
        }else{
          cat("Split queries for more than 500 nodes ...\n")
          subinp = split(txtinput, ceiling(seq_along(txtinput)/maxkw)) #split keywords
          paths = foreach(i=1:length(subinp), .combine=c) %dopar% { 
            qstring = gsub("keyword", paste0("['",paste0(unlist(subinp[i]), collapse = "','"),"']"), querystring)
        cat(qstring,"\n")
            #jsonlite::fromJSON(unlist(curlRequest.json(cypher=qstring), recursive = FALSE))$data
            curlRequest.TRANSACTION(cypher=qstring)
          }
        }
      }else{
        cat("Register parallel computing ...\nWarning: querying a large network will take long time. \n")
        path = foreach(i=1:length(from), .combine=rbind) %dopar% {
          foreach(j=1:length(to)) %dopar% {
            qstring = gsub("keyfrom", from[i], querystring)
            qstring = gsub("keyto", to[j], qstring)
        cat(qstring,"\n")
            #jsonlite::fromJSON(unlist(curlRequest.json(cypher=qstring), recursive = FALSE))$data
            curlRequest.TRANSACTION(cypher=qstring)
          }
        }
        paths = unlist(path, recursive = FALSE)
      }
      formatNetworkOutput(paths,returnas)
#       pathls = unique(unlist(sapply(paths, function(x) x$relationships)))
#       cat("Format and returning network of size ",length(pathls)," ...\n")
#       network = foreach(i=1:length(pathls), .combine=rbind) %dopar% {
#         #fetchRelationship(pathls[i], returnas="dataframe")
#         formatPathOutput(pathls[i])
#       }
#       cat("Format and returning network nodes ...\n")
#       #format nodeList from edgeList
#       scname = sapply(network$sourcename, function(x) ifelse(!is.null(x),x,network$source))
#       sclabel = sapply(network$sourcelabel, function(x) ifelse(!is.null(x),x,""))
#       scxref = sapply(network$sourcexref, function(x) ifelse(!is.null(x),x,list("")))
#       trname = sapply(network$targetname, function(x) ifelse(!is.null(x),x,network$target))
#       trlabel = sapply(network$targetlabel, function(x) ifelse(!is.null(x),x,""))
#       trxref = sapply(network$targetxref, function(x) ifelse(!is.null(x),x,list("")))
#       so = data.frame(id=network$source,name=scname,nodelabel=sclabel, stringsAsFactors = FALSE)
#       so$nodexref = scxref
#       ta = data.frame(id=network$target,name=trname,nodelabel=trlabel, stringsAsFactors = FALSE)
#       ta$nodexref = trxref
#       networknode = unique(rbind(so,ta))
#       
#       ## output
#       switch(returnas,
#              dataframe = list(nodes = networknode, edges = network),
#              list = list(nodes = split(networknode, seq(nrow(networknode))), edges = split(network, seq(nrow(network)))),
#              json = list(nodes = jsonlite::toJSON(networknode), edges = jsonlite::toJSON(network)),
#              stop("Error: incorrect 'returnas' type"))
    },
    error=function(e) {
      message(e)
      cat("\nError: RETURN no network ..\n")
      switch(returnas,
             dataframe = list(nodes = data.frame(), edges = data.frame()),
             list = list(nodes = list(), edges = list()),
             json = list(nodes = "", edges = ""))
    })
  return(out)
}