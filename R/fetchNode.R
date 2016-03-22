#'Query node(s) from the graph database
#'@description query node(s) from the graph database. The results contain node information including neo4jid, grinnid, nodename, 
#'nodelabel, datasource, description, organism, synonyms and xref, if applicable.
#'@usage fetchNode(txtinput, nodetype, searchby, exactmatch, returnas)
#'@param txtinput a character vector of values used for the query e.g. txtinput = c('name1', 'name2'). 
#'The value can be a kind of node property keys, see \code{searchby}. Default is neo4jid.
#'@param nodetype a string specifying the type of a query node. It can be one of compound, protein, gene, pathway, rna, dna, biofeature.
#'@param searchby a string specifying a node property key used for the query. It can be one of neo4jid, grinnid, name, synonyms, description, properties, xref, datasource. Default is grinnid.
#'@param exactmatch logical. If TRUE, it will query for exact results i.e. a case-sensitive and exact keyword or phrase. Default is TRUE.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@seealso \code{\link{fetchRelation}}
#'@return node information. Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'# Query genes by name
#'#kw <- list('MT-TF','MT-TQ','MT-TI','MT-ND2','MT-ND3')
#'#result <- fetchNode(txtinput=kw, nodetype="gene", searchby="name")
#'# Query genes by KEGG ids
#'#kw <- c('hsa:4514','hsa:45356567','hsa:4535')
#'#result <- fetchNode(txtinput=kw, nodetype="gene", searchby="xref")
#'# Query proteins by description
#'#kw <- '6-phosphogluconolactonase'
#'#result <- fetchNode(txtinput=kw, nodetype="protein", searchby="description", exactmatch=FALSE)
#'@export
fetchNode <- function(txtinput, nodetype, searchby="neo4jid", exactmatch=TRUE, returnas="dataframe") UseMethod("fetchNode")
#'@export
fetchNode.default <- function(txtinput, nodetype, searchby="neo4jid", exactmatch=TRUE, returnas="dataframe"){ 
  out <- tryCatch(
  {
    tmparg <- try(nodetype <- match.arg(tolower(nodetype), c("compound","protein","gene","pathway","rna","dna","biofeature"), several.ok = FALSE), silent = TRUE)
    if (class(tmparg) == "try-error") {
      stop("argument 'nodetype' is not valid, choose one from the list: compound,protein,gene,pathway,rna,dna,biofeature")
    }
    tmparg <- try(searchby <- match.arg(tolower(searchby), c("neo4jid","grinnid","name","synonyms","description","properties","xref","datasource"), several.ok = FALSE), silent = TRUE)
    if (class(tmparg) == "try-error") {
      stop("argument 'searchby' is not valid, choose one from the list: grinnid,name,synonyms,description,properties,xref,datasource")
    }
    
    #construct query
    txtinput = unique(stringr::str_trim(unlist(txtinput))) #remove whiteline, duplicate
    len = length(txtinput)
    maxkw = 500 #maximum keywords
    nodetype = Hmisc::capitalize(nodetype)
    isString =  searchby %in% propertyList$stringVal
    doPar = TRUE
    if (exactmatch == TRUE && isString == TRUE) {
      querystring = nodeList["exactMatch"]
      doPar = FALSE #use UNWIND
    }else if (exactmatch == FALSE && isString == TRUE) {
      querystring = nodeList["regexMatch"] 
    }else if (exactmatch == TRUE && isString == FALSE) {
      querystring = nodeList["exactCollection"] 
    }else{
      querystring = nodeList["regexCollection"]
    }
    if(searchby == 'neo4jid'){
      querystring = gsub("node.property = x", "ID(node) = toInt(x)", querystring)
      txtinput = txtinput[!is.na(suppressWarnings(as.numeric(txtinput)))] #remove string, ID accepts integer only
    }else if(searchby == 'grinnid'){
      querystring = gsub("property", "GID", querystring)
    }else if(searchby == 'datasource'){
      querystring = gsub("property", "dataSource", querystring)
    }else{
      querystring = gsub("property", searchby, querystring)
    }
    querystring = gsub("label", nodetype, querystring)
    querystring = paste(querystring,"RETURN DISTINCT node")
    
    cat("Querying node ...\n")
    if(!doPar){
      if(len <= maxkw){
        qstring = gsub("keyword", paste0("['",paste0(txtinput, collapse = "','"),"']"), querystring)
      cat(qstring,"\n")      
        nodes = curlRequest.TRANSACTION(cypher=qstring)
      }else{
        cat("Split queries for more than 500 nodes ...\n")
        subinp = split(txtinput, ceiling(seq_along(txtinput)/maxkw)) #split keywords
        nodes = foreach(i=1:length(subinp), .combine=c) %dopar% { 
          qstring = gsub("keyword", paste0("['",paste0(unlist(subinp[i]), collapse = "','"),"']"), querystring)
      cat(qstring,"\n")
        curlRequest.TRANSACTION(cypher=qstring)
        }
      }
    }else{
      cat("Register parallel computing ...\nWarning: querying a large number of nodes will take long time. \n")
      nodes = foreach(i=1:length(txtinput), .combine=c) %dopar% {
        qstring = gsub("keyword", txtinput[i], querystring)
      cat(qstring,"\n")
        curlRequest.TRANSACTION(cypher=qstring)
      }
    }
    formatNodeOutput(nodes,returnas)
#     cat("Format and returning node of size ",length(nodes)," ...\n")
#     nodels = foreach(i=1:length(nodes), .combine=rbind) %dopar% {
#       formatNodeOutput(nodes[[i]])
#     }
#     
#     ## output
#     switch(returnas,
#            dataframe = nodels,
#            list = split(nodels, seq(nrow(nodels))),
#            json = jsonlite::toJSON(nodels),
#            stop("Error: incorrect 'returnas' type"))
  },
  error = function(e) {
    message(e)
    cat("\nError: RETURN no node ..\n")
    switch(returnas,
           dataframe = data.frame(),
           list = list(),
           json = list())
  })    
  return(out)
}