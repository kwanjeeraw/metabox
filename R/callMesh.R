#'Retrieve Mesh from PubChem
#'@description retrieve Mesh for a given pubchemID from PubChem
#'@param pcid pubchem ID
#'@return list of nodes and edges. The list contains the data frame of nodes or Mesh and the data frame of edges or annotation pair.
#'Return empty list of data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'# mesh = callMesh(345)
callMesh <- function(pcid) UseMethod("callMesh")
callMesh.default <- function(pcid){
  out <- tryCatch(
    {
      cat("Retrieving Mesh from PubChem ...\n")
      url = paste0('https://pubchem.ncbi.nlm.nih.gov/classification/cgi/classifications.fcgi?format=json&hid=1&search_uid_type=cid&search_uid=',pcid,'&search_type=tree')
      mesh = jsonlite::fromJSON(txt=url, simplifyVector = FALSE)$Hierarchies$Hierarchy[[1]]$Node
      attb = unique(plyr::ldply(lapply(mesh,formatMesh), data.frame))
      pair = unique(cbind(attb$id,pcid))
      colnames(pair) = c("source","target")
      pair = as.data.frame(pair,stringsAsFactors = FALSE)
      list(nodes=attb, edges=pair)
    },error=function(e) {
      #message(e)
      cat("\nUnknown compound: RETURN no result ..\n")
      list(nodes=data.frame(), edges=data.frame())
    })
  return(out)
}
