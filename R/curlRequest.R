#'Execute query
#'@description execute query using the function \code{\link{curlPerform}} from \pkg{RCurl} for sending HTTP request to the database.
#'@return list or data frame. Return no data if found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references The RCurl package \url{http://www.inside-r.org/packages/cran/RCurl}
#'@seealso \code{\link{curlPerform}}, \url{http://neo4j.com/docs/milestone/introduction.html}
#'@examples
#'# Query by neo4j id
#'#querystring <- "MATCH (n) WHERE ID(n) = 0 RETURN n"
#'#result <- curlRequest.TRANSACTION(querystring)
#'# Query by url
#'#url <- "http://localhost:7474/db/data/node/306/relationships/out"
#'#result <- curlRequest.URL(url)
#legacy HTTP endpoint
curlRequest <- function(cypher){
  h = RCurl::basicTextGatherer()
  tryCatch({
    url = paste0(nld,"cypher")
    #url = paste0("http://localhost:7474/db/data/","cypher")
    RCurl::curlPerform(url=url,
                       userpwd = neu,
                       httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
                       postfields=paste('query',RCurl::curlEscape(cypher), sep='='),
                       writefunction = h$update,
                       verbose = FALSE
    )
    result <- jsonlite::fromJSON(h$value())$data
  }, error = function(err) {
    message(err)
    result <- list() #return empty if not found
  })
}
#transaction HTTP endpoint return graph format
#note: execute multiple statements available in neo4j 2.3.3
curlRequest.TRANSACTION <- function(cypher){
  h = RCurl::basicTextGatherer()
  tryCatch({
    url = paste0(nld,"transaction/commit")
    #url = paste0("http://localhost:7474/db/data/","transaction/commit")
    body = paste0("{\"statements\":[{\"statement\":\"",cypher,"\",\"resultDataContents\":[\"graph\"]}]}")
    RCurl::curlPerform(url=url,
                       userpwd = neu,
                       httpheader = c(Authorization = paste("Basic",RCurl::base64(neu)), 'Content-Type' = "application/json"),
                       postfields=body,
                       writefunction = h$update,
                       verbose = FALSE
    )
    result <- jsonlite::fromJSON(h$value(), simplifyDataFrame=FALSE)$results[[1]]$data
  }, error = function(err) {
    message(err)
    result <- list() #return empty if not found
  })
}
#transaction HTTP endpoint return row format
#note: execute multiple statements available in neo4j 2.3.3
curlRequest.TRANSACTION.row <- function(cypher){
  h = RCurl::basicTextGatherer()
  tryCatch({
    url = paste0(nld,"transaction/commit")
    #url = paste0("http://localhost:7474/db/data/","transaction/commit")
    body = paste0("{\"statements\":[{\"statement\":\"",cypher,"\",\"resultDataContents\":[\"row\"]}]}")
    RCurl::curlPerform(url=url,
                       userpwd = neu,
                       httpheader = c(Authorization = paste("Basic",RCurl::base64(neu)), 'Content-Type' = "application/json"),
                       postfields=body,
                       writefunction = h$update,
                       verbose = FALSE
    )
    result <- jsonlite::fromJSON(h$value(), simplifyDataFrame=FALSE)$results[[1]]$data
  }, error = function(err) {
    message(err)
    result <- list() #return empty if not found
  })
}
#url request to list
curlRequest.URL <- function(url){
  h = RCurl::basicTextGatherer()
  tryCatch({
    RCurl::curlPerform(url=url,
                       userpwd = neu,
                       httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
                       writefunction = h$update,
                       verbose = FALSE
    )
    result <- jsonlite::fromJSON(h$value(), simplifyVector = FALSE) #return as list
  }, error = function(err) {
    message(err)
    result <- list() #return empty if not found
  })
}
#url request to data frame
curlRequest.URL.DFrame <- function(url){
  h = RCurl::basicTextGatherer()
  tryCatch({
    RCurl::curlPerform(url=url,
                       userpwd = neu,
                       httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
                       writefunction = h$update,
                       verbose = FALSE
    )
    result <- jsonlite::fromJSON(h$value()) #return as data.frame
  }, error = function(err) {
    message(err)
    result <- data.frame() #return empty if not found
  })
}
