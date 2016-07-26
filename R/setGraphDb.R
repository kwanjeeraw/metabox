#'Set database location
#'@description set database location for the currently working environment.
#'@usage setGraphDb(url)
#'@param url of the database location
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{getGraphDb}}
#'@examples
#'# Set database location to local server
#'setGraphDb("http://localhost:7474/db/data/")
#'@export
setGraphDb <- function(url) UseMethod("setGraphDb")
#'@export
setGraphDb.default <- function(url){
  assign("database.location", url, envir = .GlobalEnv)
}
