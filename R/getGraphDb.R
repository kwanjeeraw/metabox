#'Get database location
#'@description get database location of the current working envornment.
#'By default the local Neo4j database location is http://localhost:7474/db/data/
#'@usage getGraphDb()
#'@return url of the database location.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{setGraphDb}}
#'@export
getGraphDb <- function() UseMethod("getGraphDb")
#'@export
getGraphDb.default <- function(){
  if(foundDb()){
    getDbInfo()
    cat(gsub("http://metaboxdb.fiehnlab.ucdavis.edu/db/data/","metabox is using a default human database.",database.location))
  }else{
    cat("No database found. Using BioNetwork and Pathway-based analyses will require database installation.")
  }
}
