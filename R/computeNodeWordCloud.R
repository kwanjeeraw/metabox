#'Compute WordCloud the input entities
#'@description compute WordCloud for the list of entities (e.g. compound, protein, gene).
#'The function wraps around \pkg{\link{tm}}.
#'@usage computeNodeWordCloud(txtinput, nodetype, annotation, internalid, returnas)
#'@param txtinput a character vector of entities used for the query e.g. txtinput = c('id1', 'id2').
#'The value can be neo4j ids or grinn ids, see \code{\link{convertId}} for how to convert ids.
#'@param nodetype a string specifying the type of source nodes. It can be one of compound, protein, gene, rna, dna.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh. Mesh annotation is available for PubChem compounds only.
#'@param internalid boolean value, whether txtinput are neo4j ids, see \code{\link{convertId}} for how to convert ids.
#'\code{internalid} has no effect on Mesh enrichment.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges and WordCloud result. The list contains the data frame of nodes, the data frame of edges and
#'the data frame of wordCloud result with the following components:
#'
#'\code{id} = internal neo4j id
#'
#'\code{gid} = grinn id
#'
#'\code{nodename} = name of entity set
#'
#'\code{nodelabel} = annotation type
#'
#'\code{nodexref} = cross references
#'
#'\code{freq} = frequency of words
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#'@seealso \pkg{\link{tm}}, \pkg{\link{wordcloud}}
#'@examples
#'#kw <- c('G15729','G17561','G16015','G18145','G16708')
#'#grinnNW <- fetchGrinnNetwork(txtInput=kw, from="metabolite", to="protein")
#'#library(grinn)
#'#data(dummyStat)
#'#result <- fetchSubnetwork(statInput = dummyStat, nwInput = grinnNW)
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
#'@export
computeNodeWordCloud <- function(txtinput, nodetype="compound", annotation="pathway", internalid = TRUE, returnas="dataframe") UseMethod("computeNodeWordCloud")
#'@export
computeNodeWordCloud.default <- function (txtinput, nodetype="compound", annotation="pathway", internalid = TRUE, returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(nodetype <- match.arg(tolower(nodetype), c("compound","protein","gene","rna","dna"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'nodetype' is not valid, choose one from the list: compound,protein,gene,rna,dna")
      }
      tmparg <- try(annotation <- match.arg(tolower(annotation), c("pathway","mesh"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'annotation' is not valid, choose one from the list: pathway,mesh")
      }
      if(tolower(annotation) == 'pathway' && foundDb()){#pathway wordcloud
        cat("Querying database ...\n")
        if(internalid){
          annols = lapply(txtinput, function(x) fetchNetwork(to=x, fromtype="pathway", totype = nodetype, reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = lapply(txtinput, function(x) fetchNetworkByGID(to=x, fromtype="pathway", totype = nodetype, reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          wc = callWordCloud(edgelist = annonws$edges, nodelist = annonws$nodes) #compute wordcloud
          networknode = annonws$nodes[annonws$nodes$nodelabel != "Pathway", ] #not return pathway nodes
          list(nodes=networknode, edges=annonws$edges, wordcloud=wc) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame()) #output
        }
      }else if(tolower(annotation) == 'mesh'){#mesh wordcloud
        if(internalid || tolower(nodetype) != "compound"){
          cat("Error: Accept only PubChem compounds, returning no data ...\n")
          annols = NULL
        }else{
          cat("Connecting PubChem ...\n")
          annols = lapply(txtinput, function(x) callMesh(pcid=x)) #query pubchem annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          if(foundDb()){#have db
            nodels = lapply(txtinput, formatNode.LIST, y="compound", z="grinnid") #query nodes by gid
            networknode = plyr::ldply(nodels, data.frame)
            networknode$id = networknode$gid
          }else{#no db
            cat("No database installed, returning original input ...\n")
            networknode = data.frame(id=txtinput, gid=txtinput, nodename=txtinput, nodelabel="Compound", nodexref='', stringsAsFactors = FALSE)
          }
          wc = callWordCloud(edgelist = annonws$edges, nodelist = annonws$nodes) #compute wordcloud
          list(nodes=networknode, edges=annonws$edges, wordcloud=wc) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame()) #output
        }
      }else{
        cat('Error: No database installed, returning no data ..\n')
        list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame()) #output
      }
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame()) #output
    })
  return(out)
}
