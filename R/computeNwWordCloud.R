#'Compute word cloud on the input network
#'@description compute word cloud on the input network.
#'The input network is generated from any function such as \code{\link{computeSimilarity}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSubnetwork}},
#'\code{\link{fetchHetNetwork}} and \code{\link{fetchNetwork}}. The function calls \code{\link{callWordCloud}}.
#'@usage computeNwWordCloud(edgelist, nodelist, annotation, internalid, returnas)
#'@param edgelist a data frame of edges contains at least source and target pairs
#'@param nodelist a data frame of nodes contains node information e.g. node name, node xref.
#'The first column is id. See \code{\link{fetchNetwork}} for the data frame structure.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh. Mesh annotation is available for PubChem compounds only.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'\code{internalid} has no effect on Mesh enrichment.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges, WordCloud result and annotation pairs. The list contains the data frame of nodes, the data frame of edges and
#'the data frame of wordCloud result with the following components:
#'
#'\code{rank} = rank sort by freq
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
#'
#'\code{member} = list of members of the entity set
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \pkg{\link{tm}}, \pkg{\link{wordcloud}}
#'@examples
#'# pc = read.csv("~/Desktop/testGrinn/pubchemls.txt")
#'# nw = computeSimilarity(pc$GrinnID)
#'# wd = computeNwWordCloud(nw$edges,nw$nodes)
#'# wordcloud::wordcloud(words = gsub(" - Mus musculus \\(mouse\\)","",wd$wordcloud$nodename), freq = wd$wordcloud$freq, scale=c(2,.1),min.freq = 1,max.words=50, random.order=FALSE, rot.per=0.5, colors=brewer.pal(8, "Dark2"))
#'# barplot(wd$wordcloud$freq[1:10], las = 2, names.arg = gsub(" - Mus musculus \\(mouse\\)","",wd$wordcloud$nodename[1:10]), col ="lightblue", main ="Most frequent words", ylab = "Word frequencies")
#'@export
computeNwWordCloud <- function(edgelist, nodelist, annotation="pathway", internalid = TRUE, returnas="dataframe") UseMethod("computeNwWordCloud")
#'@export
computeNwWordCloud.default <- function (edgelist, nodelist, annotation="pathway", internalid = TRUE, returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(annotation <- match.arg(tolower(annotation), c("pathway","mesh"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'annotation' is not valid, choose one from the list: pathway,mesh")
      }
      if(tolower(annotation) == 'pathway' && foundDb()){#pathway wordcloud
        cat("Querying database ...\n")
        if(internalid){
          annols = apply(nodelist, 1, function(x) fetchNetwork(to=x["id"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = apply(nodelist, 1, function(x) fetchNetworkByGID(to=x["gid"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          wc = callWordCloud(edgelist = annonws$edges, nodelist = annonws$nodes) #compute wordcloud
          wc = wc[order(wc$freq, decreasing = TRUE),]
          wc$rank = seq(1:nrow(wc))
          wc = wc[,c(ncol(wc),1:(ncol(wc)-1))] #rearrange columns
          list(nodes=nodelist, edges=edgelist, wordcloud=wc, pairs=annonws$edges) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame(), pairs=data.frame()) #output
        }
      }else if(tolower(annotation) == 'mesh'){#mesh wordcloud
        cat("Connecting PubChem ...\n")
        annols = apply(nodelist, 1, function(x) callMesh(pcid=x["gid"])) #query annotation pairs
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          #format edge, change gid to id, fix edge row order
          annopair = dplyr::right_join(nodelist[,1:2],annonws$edges[,1:2],by=c('gid' = 'target'))[,c(3,1)]
          colnames(annopair) = c('source','target')
          wc = callWordCloud(edgelist = annopair, nodelist = annonws$nodes) #compute wordcloud
          wc = wc[order(wc$freq, decreasing = TRUE),]
          wc$rank = seq(1:nrow(wc))
          wc = wc[,c(ncol(wc),1:(ncol(wc)-1))] #rearrange columns
          list(nodes=nodelist, edges=edgelist, wordcloud=wc, pairs=annonws$edges, pairs=annopair) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame(), pairs=data.frame()) #output
        }
      }else{
        cat('Error: No database installed, returning no data ..\n')
        list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame(), pairs=data.frame()) #output
      }
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame(), pairs=data.frame()) #output
    })
  return(out)
}
