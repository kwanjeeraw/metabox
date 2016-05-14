#'Format relationships
#'@description perform curl request to get relationship information for \code{\link{fetchRelationship}}.
#'@param url a string specifying relationship url to be formatted
#'@seealso \code{\link{curlRequest}}, \code{\link{fetchRelationship}}
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
#'Return empty data frame if error or found nothing.
#'@note curlRequest.TRANSACTION is used in other functions to improve speed.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'#url = "http://localhost:7474/db/data/node/0/relationships/out"
#'#result = curlRequest.URL(url)
#'#df = data.frame(t(sapply(result,c)))
#'#formatPathOutput(df)
formatPathOutput <- function(url){
  out <- tryCatch(
    {
      path = curlRequest.URL(url)
      start = curlRequest.URL.DFrame(path$start)
      end = curlRequest.URL.DFrame(path$end)
      pathInfo = data.frame(source=start$data$GID, sourcename=start$data$name, sourcelabel=start$metadata$labels[[1]],
                            target=end$data$GID, targetname=end$data$name, targetlabel=end$metadata$labels[[1]],
                            type=path$metadata$type, datasource=path$data$dataSource, stringsAsFactors = FALSE)
      if(is.null(path$data$properties)){
        pathInfo$properties=list("")
      }else{
        pathInfo$properties=path$data$properties
      }
      pathInfo$sourcexref=list(start$data$xref)
      pathInfo$targetxref=list(end$data$xref)
      pathInfo
    },
    error=function(e) {
      message(e)
      cat("\nError: RETURN no relationship ..\n")
      data.frame() # Choose a return value in case of error
    })
  return(out)
}
