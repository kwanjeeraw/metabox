#'Convert id(s)
#'@description convert to internal neo4j id(s) and grinn id(s).
#'@usage convertId(x, nodetype, searchby, exactmatch, returnas)
#'@param x a character vector or dataframe of keywords used for the mapping e.g. x = c('name1', 'name2'), see below for details.
#'The value can be a kind of node property keys, see \code{searchby}. Default is xref
#'@param nodetype a string specifying the type of a query node. It can be one of compound, protein, gene, pathway, rna, dna, biofeature.
#'@param searchby a string specifying a node property key used for the query. It can be one of name, synonyms, description, properties, xref. Default is xref.
#'@param exactmatch logical. If TRUE, it will query for exact results i.e. a case-sensitive and exact keyword or phrase. Default is TRUE.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details If \code{x} is a character vector, the results include the input, the internal neo4j id and grinn id.
#'
#'\code{x} can be a dataframe containing other data e.g. stat values. First column must be the keywords for the mapping.
#'The results will include the input, the internal neo4j id, grinn id and the rest of the dataframe.
#'
#'Unmapped keywords are also returned.
#'@return pair of input and the match internal Neo4j id and grinn id. Return empty list or data frame if error.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'# Convert gene names
#'#kw <- list('MT-TF','MT-TQ','MT-TI','MT-ND2','MT-ND3')
#'#result <- convertId(x=kw, nodetype="gene", searchby="name")
#'# Convert KEGG ids
#'#kw <- c('hsa:4514','hsa:4535')
#'#result <- convertId(x=kw, nodetype="gene", searchby="xref")
#'# Convert description
#'#kw <- '6-phosphogluconolactonase'
#'#result <- convertId(x=kw, nodetype="protein", searchby="description", exactmatch=FALSE)
#'@export
convertId <- function(x, nodetype, searchby="xref", exactmatch=TRUE, returnas="dataframe") UseMethod("convertId")
#'@export
convertId.default <- function(x, nodetype, searchby="xref", exactmatch=TRUE, returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(nodetype <- match.arg(tolower(nodetype), c("compound","protein","gene","pathway","rna","dna","biofeature"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'nodetype' is not valid, choose one from the list: compound,protein,gene,pathway,rna,dna,biofeature")
      }
      tmparg <- try(searchby <- match.arg(tolower(searchby), c("xref","name","synonyms","description","properties"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'searchby' is not valid, choose one from the list: name,synonyms,description,properties,xref")
      }

      if(!is.null(dim(x))){#dataframe input
        txtinput = unique(stringr::str_trim(unlist(x[,1]))) #remove whiteline, duplicate
        isDF = ifelse(ncol(x) > 1, TRUE, FALSE)
      }else{#list input
        txtinput = unique(stringr::str_trim(unlist(x))) #remove whiteline, duplicate
        isDF = FALSE
      }
      #construct query
      nodetype = Hmisc::capitalize(nodetype)
      isString =  searchby %in% propertyList$stringVal
      if (exactmatch == TRUE && isString == TRUE) {
        querystring = idList["exactMatch"]
        querystring = paste(querystring,"RETURN DISTINCT ID(node), node.GID")
      }else if (exactmatch == FALSE && isString == TRUE) {
        querystring = idList["regexMatch"]
        querystring = paste(querystring,"RETURN DISTINCT ID(node), node.GID")
      }else if (exactmatch == TRUE && isString == FALSE) {
        querystring = idList["exactCollection"]
        querystring = paste(querystring,"RETURN DISTINCT ID(node), node.GID")
      }else{
        querystring = idList["regexCollection"]
        querystring = paste(querystring,"RETURN DISTINCT ID(node), node.GID")
      }

      querystring = gsub("property", searchby, querystring)
      querystring = gsub("label", nodetype, querystring)

      cat("Mapping node ...\n")
      cat("Register parallel computing ...\nWarning: querying a large number of nodes will take long time. \n")
      if(isDF){#return all input data
        nodes = foreach(i=1:length(txtinput), .combine=rbind) %dopar% {
          qstring = gsub("keyword", txtinput[i], querystring)
          cat(qstring,"\n")
          res = curlRequest(cypher=qstring)
          if(length(res)>0){
            data.frame(txtinput[i],res, x[i,2:ncol(x)], stringsAsFactors = FALSE)
          }else{
            data.frame(txtinput[i],t(as.matrix(rep("",2))), x[i,2:ncol(x)], stringsAsFactors = FALSE)
          }
        }
        colnames(nodes)[1:3] = c("inputid","neo4jid","grinnid")
      }else{
        nodes = foreach(i=1:length(txtinput), .combine=rbind) %dopar% {
          qstring = gsub("keyword", txtinput[i], querystring)
          cat(qstring,"\n")
          res = curlRequest(cypher=qstring)
          if(length(res)>0){
            data.frame(txtinput[i],res, stringsAsFactors = FALSE)
          }else{
            data.frame(txtinput[i],t(as.matrix(rep("",2))), stringsAsFactors = FALSE)
          }
        }
        colnames(nodes) = c("inputid","neo4jid","grinnid")
      }

      cat("Format and returning output of size ",nrow(nodes)," ...\n")

      ## output
      switch(returnas,
             dataframe = nodes,
             list = split(nodes, seq(nrow(nodes))),
             json = jsonlite::toJSON(nodes),
             stop("Error: incorrect 'returnas' type"))
    },
    error = function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      switch(returnas,
             dataframe = data.frame(),
             list = list(),
             json = list())
    })
  return(out)
}
