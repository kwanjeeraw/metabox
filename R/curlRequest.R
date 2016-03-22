#'Execute query
#'@description execute query using Cypher or url as an input. The function calls curlPerform from \pkg{RCurl} for sending HTTP request to the graph database.
#'@usage curlRequest(param)
#'@param cypher a string of Cypher (default) or url a string of url
#'@return list or data frame. Return no data if found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references The RCurl package \url{http://www.inside-r.org/packages/cran/RCurl}
#'@seealso \code{\link{curlPerform}}, \url{http://neo4j.com/docs/milestone/introduction.html}
#'@examples
#'# Query metabolites by database id
#'#querystring <- "UNWIND ['MT-TF','MT-ND3'] AS x WITH x MATCH (node:Gene) WHERE node.name = x RETURN DISTINCT node"
#'#result <- curlRequest(querystring)
#'#url <- "http://localhost:7474/db/data/node/306/relationships/out"
#'#result <- curlRequest.URL(url)
#cypher
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
#transaction
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
    #result <- RJSONIO::fromJSON(h$value())$results[[1]]$data
    result <- jsonlite::fromJSON(h$value(), simplifyDataFrame=FALSE)$results[[1]]$data
  }, error = function(err) {
    message(err)
    result <- list() #return empty if not found
  })
}
# #cypher
# curlRequest.json <- function(cypher){
#   h = RCurl::basicTextGatherer()
#   tryCatch({
#     url = paste0(nld,"cypher")
#     RCurl::curlPerform(url=url,
#                        userpwd = neu,
#                        httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
#                        postfields=paste('query',RCurl::curlEscape(cypher), sep='='),
#                        writefunction = h$update,
#                        verbose = FALSE
#     )
#     result <- h$value() #return as json
#   }, error = function(err) {
#     message(err)
#     result <- list() #return empty if not found
#   })
# }
#
# #url
# curlRequest.URL <- function(url){
#   h = RCurl::basicTextGatherer()
#   tryCatch({
#     RCurl::curlPerform(url=url,
#                        userpwd = neu,
#                        httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
#                        writefunction = h$update,
#                        verbose = FALSE
#     )
#     result <- jsonlite::fromJSON(h$value(), simplifyVector = FALSE) #return as list
#   }, error = function(err) {
#     message(err)
#     result <- list() #return empty if not found
#   })
# }
# #url
# curlRequest.URL.DFrame <- function(url){
#   h = RCurl::basicTextGatherer()
#   tryCatch({
#     RCurl::curlPerform(url=url,
#                        userpwd = neu,
#                        httpheader = c(Authorization = paste("Basic",RCurl::base64(neu))),
#                        writefunction = h$update,
#                        verbose = FALSE
#     )
#     result <- jsonlite::fromJSON(h$value()) #return as data.frame
#   }, error = function(err) {
#     message(err)
#     result <- data.frame() #return empty if not found
#   })
# }
#
# curlRequest.TRANSACTION <- function(cypher){
#   h = RCurl::basicTextGatherer()
#   tryCatch({
#     #cypher = "MATCH ptw = (from:Protein)-[:CONTROL]->(to:Gene) WHERE from.GID = 'P14859' RETURN DISTINCT ptw LIMIT 2"
#     #url = paste0(nld,"transaction/commit")
#     url = "http://localhost:7474/db/data/transaction/commit"
#     body = paste0("{\"statements\":[{\"statement\":\"",cypher,"\",\"resultDataContents\":[\"graph\"]}]}")
#     RCurl::curlPerform(url=url,
#                        userpwd = neu,
#                        httpheader = c(Authorization = paste("Basic",RCurl::base64(neu)), 'Content-Type' = "application/json"),
#                        postfields=body,
#                        writefunction = h$update,
#                        verbose = FALSE
#     )
#     result <- RJSONIO::fromJSON(h$value())$results[[1]]$data
#   }, error = function(err) {
#     message(err)
#     result <- list() #return empty if not found
#   })
# }
