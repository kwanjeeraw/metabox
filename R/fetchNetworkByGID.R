#'Query the networks of a specific relationship from the graph database
#'@description query the networks of a specific relationship type between the given source and/or target nodes.
#'see \url{http://grinnhomepage} for the database structure to construct the network query.
#'@usage fetchNetworkByGID(from, to, fromtype, totype, reltype, returnas)
#'@param from a character vector of source nodes used for the query e.g. from = c('id1', 'id2').
#'The value must be grinn ids, see \code{\link{convertId}} for how to convert to grinn ids.
#'@param to a character vector of target nodes used for the query e.g. to = c('id1', 'id2').
#'The value must be grinn ids, see \code{\link{convertId}} for how to convert to grinn ids.
#'@param fromtype a string specifying the type of source nodes. It can be one of compound, protein, gene, pathway, rna, dna, phenotype.
#'@param totype a string specifying the type of target nodes. It can be one of compound, protein, gene, pathway, rna, dna, phenotype.
#'@param reltype a string specifying a relationship type used for the query.
#'It can be one of annotation, biochemical_reaction, catalysis, control, conversion, genetic_association, molecular_binding.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details The function queries for the networks containing the given relationship type between the source nodes and/or target nodes based on the database structure.
#'This function is specifically to query for the ralationship of length = 1. Use \code{\link{fetchHetNetwork}} to query for a heterogeneous network of variable length.
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
#'@seealso \code{\link{convertId}}, \code{\link{fetchHetNetwork}}, \url{http://grinnhomepage}
#'@examples
#'# Query the network of gene-CONVERSION-protein
#'#from = list('162608','38432','44911')
#'#result = fetchNetworkByGID(from=from, to=NULL, fromtype="gene", totype="protein", reltype="conversion")
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
#'# Query the network of protein-CONTROL-gene
#'#from = list('30777','30741','28116')
#'#to = c("343","285","255")
#'#result = fetchNetworkByGID(from=from, to=to, fromtype="protein", totype="gene", reltype="control")
#'#from = c('2','5')
#'#to = '4'
#'#result = fetchNetworkByGID(from=from, to=to, fromtype="protein", totype="protein", reltype="molecular_binding")
#'@export
fetchNetworkByGID <- function(from=NULL, to=NULL, fromtype, totype, reltype, returnas="dataframe") UseMethod("fetchNetworkByGID")
#'@export
fetchNetworkByGID.default <- function(from=NULL, to=NULL, fromtype, totype, reltype, returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(fromtype <- match.arg(tolower(fromtype), c("compound","protein","gene","pathway","rna","dna","phenotype"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'fromtype' is not valid, choose one from the list: compound,protein,gene,pathway,rna,dna,phenotype")
      }
      tmparg <- try(totype <- match.arg(tolower(totype), c("compound","protein","gene","pathway","rna","dna","phenotype"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'totype' is not valid, choose one from the list: compound,protein,gene,pathway,rna,dna,phenotype")
      }
      tmparg <- try(reltype <- match.arg(tolower(reltype), c("annotation","biochemical_reaction","catalysis","control","conversion","genetic_association","molecular_binding"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'reltype' is not valid, choose one from the list: annotation,biochemical_reaction,catalysis,control,conversion,genetic_association,molecular_binding")
      }

      #construct query
      maxkw = 500 #maximum keywords
      fromtype = Hmisc::capitalize(fromtype)
      totype = Hmisc::capitalize(totype)
      doPar = FALSE
      if (!is.null(from) && !is.null(to)) {#from and to
        querystring = pathList_GID["fromto"]
        doPar = TRUE #do loop
        from = unique(stringr::str_trim(unlist(from))) #remove whiteline, duplicate
        to = unique(stringr::str_trim(unlist(to))) #remove whiteline, duplicate
      }else if (!is.null(from) && is.null(to)) {#from
        querystring = pathList_GID["from"]
        txtinput = unique(stringr::str_trim(unlist(from))) #remove whiteline, duplicate
        len = length(txtinput)
      }else if (is.null(from) && !is.null(to)) {#to
        querystring = pathList_GID["to"]
        txtinput = unique(stringr::str_trim(unlist(to))) #remove whiteline, duplicate
        len = length(txtinput)
      }else{
        stop("Error: No query provided")
      }
      querystring = gsub("fromtype", fromtype, querystring)
      querystring = gsub("totype", totype, querystring)
      querystring = gsub("reltype", toupper(reltype), querystring)

      cat("Querying network ...\n")
      if(!doPar){
        if(len <= maxkw){
          qstring = gsub("keyword", paste0("['",paste0(txtinput, collapse = "','"),"']"), querystring)
          cat(qstring,"\n")
          #paths = jsonlite::fromJSON(unlist(curlRequest.json(cypher=qstring), recursive = FALSE))$data
          paths = curlRequest.TRANSACTION(cypher=qstring)
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
