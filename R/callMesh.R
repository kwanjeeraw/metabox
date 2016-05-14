#'Retrieve Mesh from PubChem
#'@description retrieve Mesh terms for a given PubChem CID from PubChem
#'@param pcid a string of PubChem CID
#'@return list of data frame of nodes and data frame of edges with the following components:
#'
#'nodes:
#'
#'\code{id} = mesh id
#'
#'\code{gid} = mesh id
#'
#'\code{nodename} = mesh name
#'
#'\code{nodelabel} = Mesh
#'
#'#'\code{nodexref} = mesh id
#'
#'edges:
#'
#'\code{source, target} = node
#'
#'\code{type} = ANNOTATION
#'
#'\code{datasource} = PUBCHEM
#'
#'Return empty list of data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'# mesh = callMesh(345)
callMesh <- function(pcid){
  out <- tryCatch(
    {
      cat("Retrieving Mesh from PubChem ...\n")
      url = paste0('https://pubchem.ncbi.nlm.nih.gov/classification/cgi/classifications.fcgi?format=json&hid=1&search_uid_type=cid&search_uid=',pcid,'&search_type=tree')
      mesh = jsonlite::fromJSON(txt=url, simplifyVector = FALSE)$Hierarchies$Hierarchy[[1]]$Node
      attb = unique(plyr::ldply(lapply(mesh,formatMesh), data.frame))
      pair = unique(cbind(attb$id,pcid,"ANNOTATION","PUBCHEM"))
      colnames(pair) = c("source","target","type","datasource")
      pair = as.data.frame(pair,stringsAsFactors = FALSE)
      list(nodes=attb, edges=pair)
    },error=function(e) {
      #message(e)
      cat("Unknown compound: RETURN no result ..\n")
      list(nodes=data.frame(), edges=data.frame())
    })
  return(out)
}
