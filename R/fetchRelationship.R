#'Query relationships with url
#'@description retrieve relationship information from the database with url. The results include source node, target node, relationship type, dataSource and properties, if applicable.
#'@usage fetchRelationship(url, returnas)
#'@param url a string specifying relationship url.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return relationship information with the following components:
#'
#'\code{source, target} = node grinn id
#'
#'\code{sourcename, targetname} = node name
#'
#'\code{sourcelabel, targetlabel} = node type
#'
#'\code{sourcexref, targetxref} = node cross references
#'
#'\code{type} = relationship type
#'
#'\code{datasource} = relationship resource
#'
#'\code{properties} = relationship properties
#'
#'Return empty list or data frame if error or found nothing.
#'@note curlRequest.TRANSACTION is used in other functions to improve speed.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu
#'@examples
#'# Query genes by name
#'#result <- fetchRelationship("http://localhost:7474/db/data/relationship/0")
#'#result <- fetchRelationship("http://localhost:7474/db/data/node/0/relationships/out")
#'#result <- fetchRelationship("http://localhost:7474/db/data/node/0/relationships/in")
#'
fetchRelationship <- function(url, returnas="dataframe") UseMethod("fetchRelationship")
#'
fetchRelationship.default <- function(url, returnas="dataframe"){
  out <- tryCatch(
  {
    rel = curlRequest.URL(url)
    reldf = data.frame(t(sapply(rel,c)))
    #cat("Format and returning relationship of size ",nrow(reldf)," ...\n")
    pathls = data.frame(stringsAsFactors = FALSE)
    for(i in 1:nrow(reldf)){
      pathls = rbind(pathls,formatPathOutput(reldf[i,]))
    }
    ## output
    switch(returnas,
           dataframe = pathls,
           list = split(pathls, seq(nrow(pathls))),
           json = jsonlite::toJSON(pathls),
           stop("Error: incorrect 'returnas' type"))
  },error=function(e) {
    message(e)
    cat("\nError: RETURN no relationship ..\n")
    switch(returnas,
           dataframe = data.frame(),
           list = list(),
           json = list())
  })
  return(out)
}
