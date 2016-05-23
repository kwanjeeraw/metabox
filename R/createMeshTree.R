#'Create Mesh tree
#'@description format mesh results into \code{\pkg{data.tree}} structure. Mesh results are from enrichment, overrepresentation and worldcloud analysis.
#'@usage createMeshTree(meshtable,fntype)
#'@param meshtable data frame of mesh results or list of data frame of nodes, edges, mesh results.
#'The mesh results contain with the following columns: id, Tree, MeshName, member, p_adj (for enrichment and overrepresentation)or freq (for wordcloud).
#'@param fntype a string specifying a method calculating mesh results. It can be one of wordcloud, overrep, enrichment.
#'@param toprank a numeric number specifying the top rank of mesh results. Default is 50.
#'@details \code{\pkg{data.tree}} is used to convert mesh results to mesh tree structure. The tree can be plotted by d3js on GUI.
#'@return list of mesh tree from \code{\pkg{data.tree}}
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\pkg{data.tree}}, \url{https://d3js.org/}
#'@examples
#'#txtinput <- c(1110,10413,196,51,311,43,764,790) #compute overrepresented terms for given pubchem compounds
#'#nodeoverr <- computeNodeOverrep(txtinput=txtinput, nodetype="compound", annotation="mesh", internalid=FALSE)
#'nodeoverr = merge(nodeoverr$overrepresentation,MESH,by.x='id',by.y='MeshId')
#'#result = createMeshTree(nodeoverr, fntype="overrep")
#'#jsonlite::toJSON(result)
#'@export
createMeshTree <- function(meshtable,fntype,toprank=50) UseMethod("createMeshTree")
#'@export
createMeshTree.default <- function(meshtable,fntype,toprank=50){
  require('data.tree')
  if (class(meshtable) != "data.frame") {#get data frame of mesh results if input is list
    meshtable = meshtable[[3]] #fix index from the analysis results
  }
  cat("Creating mesh tree ...")
  ind = ifelse(nrow(meshtable)>=toprank,toprank,nrow(meshtable))
  meshtable = meshtable[1:ind,]#get top rank
  meshtable = merge(meshtable,MESH,by.x='id',by.y='MeshId')#get Tree info
  meshtable = meshtable[order(meshtable$Tree),]
  meshtree = Node$new("Chemicals and Drugs Category 2nd level", MeshId="D", MeshName="Chemicals and Drugs Category", member="PubChem CIDs")
  for(i in 1:nrow(meshtable)){#build mesh tree
    siz = switch(fntype,
                 wordcloud = meshtable$freq[i],
                 overrepresentation = -log(meshtable$p_adj[i]),
                 enrichment = -log(meshtable$p_adj[i])) #scale size
    if(nchar(meshtable$Tree[i])==7){#always create level 1
      meshtree$AddChild(meshtable$Tree[i], MeshId=meshtable$id[i], MeshName=meshtable$MeshName[i], member=meshtable$member[i], size=siz)
    }else{#other levels
      lev = stringr::str_count(meshtable$Tree[i], "\\.") #get level
      paindex = substr(meshtable$Tree[i], 1, (lev*4)-1) #get parent index
      panode = meshtree$FindNode(paindex) #get parent node
      if(is.null(panode)){#if no parent node
        meshtree$AddChild(meshtable$Tree[i], MeshId=meshtable$id[i], MeshName=meshtable$MeshName[i], member=meshtable$member[i], size=siz)
      }else{#if has parent node
        meshtree$FindNode(paindex)$AddChild(meshtable$Tree[i], MeshId=meshtable$id[i], MeshName=meshtable$MeshName[i], member=meshtable$member[i], size=siz)
      }
    }
  }
  cat("Returning meshtree ...")
  ToListExplicit(meshtree, unname = TRUE)
}
