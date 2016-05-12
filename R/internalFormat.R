#internal function to format node and edge
formatPath.TRANSACTION <- function(graph){
  out <- tryCatch(
    {
      ## Set the name for the class
      data.frame(t(sapply(graph$graph$relationships,
                          function(x) list(source=x$startNode, target=x$endNode, type=x$type, datasource=paste0(x$properties["dataSource"],collapse = "||"), properties=paste0(x$properties["properties"],collapse = "||")))))
      },
    error=function(e) {
      message(e)
      cat("\n..RETURN empty list of relations")
      out = data.frame() # Choose a return value in case of error
    })
  return(out)
}

formatNode.TRANSACTION <- function(node){
  out <- tryCatch(
    {
      data.frame(t(sapply(node,
                          function(x) list(id=x$id, gid=x$properties$GID, nodename=x$properties$name, nodelabel=x$labels, nodexref=paste0(x$properties$xref,collapse = "||")))))
      },
    error=function(e) {
      message(e)
      cat("\n..RETURN empty list of relations")
      out = data.frame() # Choose a return value in case of error
    })
  return(out)
}

formatNode.TRANSACTION.ALL <- function(node){
  out <- tryCatch(
    {
      data.frame(t(sapply(node,
                          function(x) list(id=x$id, gid=x$properties$GID, nodename=x$properties$name, nodelabel=x$labels,
                                           datasource=ifelse(!is.null(x$properties$dataSource),x$properties$dataSource,""),
                                           description=ifelse(!is.null(x$properties$description),x$properties$description,""),
                                           organism=ifelse(!is.null(x$properties$organism),x$properties$organism,""),
                                           synonyms=paste0(x$properties$synonyms,collapse = "||"), nodexref=paste0(x$properties$xref,collapse = "||")))))
    },
    error=function(e) {
      message(e)
      cat("\n..RETURN empty list of relations")
      out = data.frame() # Choose a return value in case of error
    })
  return(out)
}

formatId = function(x, y) {
  ind = which(y$gid == x)
  x = ifelse(length(ind)>0,y$id[ind],x)
}

formatNode.LIST = function(x,y,z){
  nout = fetchNode(txtinput=x, nodetype=y, searchby=z)
  if(nrow(nout)==0){#not found will return original input
    nout = data.frame(id=x, gid=x, nodename=x, nodelabel=Hmisc::capitalize(y), nodexref='', stringsAsFactors = FALSE)
  }else{
    nout = nout[,c(1:4,9)]
  }
}

#MESH contains Chemicals and Drugs Category tree (D) starting from level 3
formatMesh = function(x){
  #don't want root, itself, mesh = Supplementary Records
  if(unlist(x$ParentID) != "root" && x$Information$Name != "Supplementary Records" && !is.null(x$Information$ChildID)){
    mid = paste0('D',stringr::str_sub(x$Information$URL,-6)) #MeshTree = D
    mout = merge(data.frame(id=mid, stringsAsFactors = FALSE),MESH,by.x='id',by.y='MeshId') #merge with MESH table for annotation information
    if(nrow(mout)>0){
      data.frame(id=mout$id, gid=mout$id, nodename=mout$MeshName, nodelabel="Mesh", nodexref=mout$id, stringsAsFactors = FALSE)
    }
  }
}

foundDb = function(){#check if db exists
  tryCatch({#query nodeList from nodedata, database required
    node = curlRequest.TRANSACTION("MATCH (n) WHERE ID(n) = 0 RETURN n")
    TRUE
    #FALSE
  }, error = function(err) {#catch error if there is no db
    FALSE
  })
}
