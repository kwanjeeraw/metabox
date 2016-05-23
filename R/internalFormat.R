#format edge results from curlRequest.TRANSACTION(cypher)
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
#format node results from curlRequest.TRANSACTION(cypher)
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
#format node results from curlRequest.TRANSACTION(cypher), return all node properties
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
#get and format node properties
formatNode.LIST = function(x,y,z){
  nout = fetchNode(txtinput=x, nodetype=y, searchby=z)
  if(nrow(nout)==0){#not found node will return original input
    nout = data.frame(id=x, gid=x, nodename=x, nodelabel=Hmisc::capitalize(y), nodexref='', stringsAsFactors = FALSE)
  }else{
    nout = nout[,c(1:4,9)]
  }
}
#format Mesh results from https://pubchem.ncbi.nlm.nih.gov/classification/cgi/classifications.fcgi
#MESH object contains Chemicals and Drugs Category tree (D) starting from level 2 e.g. Amino Acids (D12.125)
formatMesh = function(x){
  #don't want root, mesh = Supplementary Records, itself
  if(unlist(x$ParentID) != "root" && x$Information$Name != "Supplementary Records" && !is.null(x$Information$ChildID)){
    mid = paste0('D',stringr::str_sub(x$Information$URL,-6)) #MeshTree = D
    mout = merge(data.frame(id=mid, stringsAsFactors = FALSE),MESH,by.x='id',by.y='MeshId') #merge with MESH table for annotation information
    if(nrow(mout)>0){
      data.frame(id=mout$id, gid=mout$id, nodename=mout$MeshName, nodelabel="Mesh", nodexref=mout$id, annotation_size=x$Information$Counts[[2]]$Count, stringsAsFactors = FALSE)
    }
  }
}
#check if the db exists
foundDb = function(){
  tryCatch({#query nodeList from nodedata, database required
    node = curlRequest.TRANSACTION("MATCH (n) WHERE ID(n) = 0 RETURN n")
    TRUE
  }, error = function(err) {#catch error and return FALSE if there is no db
    FALSE
  })
}

#convert MESH data frame to mesh tree
#ex submesh = MESH[1:50,-4]
#result = formatMeshTree(submesh)
#jsonlite::toJSON(ToListExplicit(result, unname = TRUE), pretty = TRUE)
formatMeshTree = function(mes){
  require('data.tree')
  mes = mes[order(mes$Tree),]
  meshtree = Node$new("Chemicals and Drugs Category 2nd level")
  for(i in 1:nrow(mes)){
    if(nchar(mes$Tree[i])==7){#always create level 1
      meshtree$AddChild(mes$Tree[i], MeshId=mes$MeshId[i], MeshName=mes$MeshName[i])
    }else{
      paindex = substr(mes$Tree[i], 1, nchar(mes$Tree[i])-4) #get parent index
      panode = meshtree$FindNode(paindex) #get parent node
      if(is.null(panode)){#if no parent node
        meshtree$AddChild(mes$Tree[i], MeshId=mes$MeshId[i], MeshName=mes$MeshName[i])
      }else{#if has parent node
        meshtree$FindNode(paindex)$AddChild(mes$Tree[i], MeshId=mes$MeshId[i], MeshName=mes$MeshName[i])
      }
    }
  }
  meshtree
}
