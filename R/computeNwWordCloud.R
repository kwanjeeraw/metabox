#'Compute word cloud on the input network
#'@description compute word cloud on the input network.
#'The input network is generated from any function such as \code{\link{computeSimilarity}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSubnetwork}},
#'\code{\link{fetchHetNetwork}} and \code{\link{fetchNetwork}}. The function calls \code{\link{callWordCloud}}.
#'@usage computeNwWordCloud(edgelist, nodelist, annotation, internalid, returnas)
#'@param edgelist a data frame of edges contains at least source and target pairs
#'@param nodelist a data frame of nodes contains node information e.g. node name, node xref.
#'The first column is id. See \code{\link{fetchNetwork}} for the data frame structure.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges and wordCloud result. The list contains the data frame of nodes, the data frame of edges and
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
      if(tolower(annotation) == 'pathway'){#pathway enrichment
        cat("Querying database ...\n")
        if(internalid){
          annols = apply(nodelist, 1, function(x) fetchNetwork(to=x["id"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = apply(nodelist, 1, function(x) fetchNetworkByGID(to=x["gid"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){
          annonws = combineNetworks(annols) #combine annotation pairs
          wc = callWordCloud(edgelist = annonws$edges, nodelist = annonws$nodes) #compute wordcloud
        }
        else{
          nodelist = data.frame()
          edgelist = data.frame()
          wc = data.frame()
        }
      }else if(tolower(method) == 'mesh'){#mesh enrichment
        stop('Under development')
      }else{
        stop('Unknown annotation')
      }
      list(nodes=nodelist, edges=edgelist, wordcloud=wc)
    },
    error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), wordcloud=data.frame())
    })
  return(out)
}
