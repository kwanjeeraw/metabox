#'Compute wordCloud
#'@description compute wordCloud of given annotation pairs retrieved from e.g. \code{\link{fetchNetwork}}.
#'@usage callWordCloud(edgelist, nodelist)
#'@param edgelist a data frame of annotation pairs. A source column (1st column) contains annotation terms and a target column (2nd column) contains annotated entities.
#'@param nodelist a data frame of node attributes e.g. node neo4j id, node grinn id, node name, node xref.
#'@seealso \pkg{\link{tm}}, \pkg{\link{wordcloud}}
#'@return data frame of wordcloud contains the following components:
#'
#'\code{rank} = rank sort by freq
#'
#'\code{id} = annotation id or annotation neo4j id
#'
#'\code{gid} = annotation id or annotation grinn id
#'
#'\code{nodename} = annotation name
#'
#'\code{nodelabel} = annotation type
#'
#'\code{nodexref} = cross references
#'
#'\code{freq} = frequency of the annotation term
#'
#'\code{member} = list of members of the annotation term
#'
#'Return empty data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#'@seealso \pkg{\link{tm}}
#'@examples
#'#simnw <- computeSimilarity(c(1110,10413,196,51,311,43,764,790)) #compute similarity network for given pubchem compounds
#'#result <- computeNwWordCloud(simnw$edges, simnw$nodes, annotation="mesh", internalid=FALSE)
#'#wordcloud::wordcloud(words = result$wordcloud$nodename, freq = result$wordcloud$freq, scale=c(2,.1),min.freq = 1,max.words=50, random.order=FALSE, rot.per=0.5, colors=RColorBrewer::brewer.pal(8, "Dark2"))
#'#barplot(result$wordcloud$freq[1:10], las = 2, names.arg = result$wordcloud$nodename[1:10], col ="lightblue", main ="Most frequent words", ylab = "Word frequencies")
callWordCloud <- function(edgelist, nodelist){
  out <- tryCatch(
  {
    cat("Computing wordCloud ...\n")
    docs = tm::Corpus(tm::VectorSource(edgelist$source))
    docs = tm::tm_map(docs, tm::content_transformer(tolower))
    dtm = tm::TermDocumentMatrix(docs)
    mt = as.matrix(dtm)
    v = sort(rowSums(mt), decreasing=TRUE)
    df = data.frame(word = toupper(names(v)), freq = v)
    wt = merge(nodelist, df, by.x='id', by.y='word')
    wt$member = plyr::ddply(edgelist,c('source'),plyr::summarise,member=list(target))$member
    wt[order(wt$freq, decreasing = TRUE),]
  },
  error=function(e) {
    message(e)
    cat("\nError: RETURN no result ..\n")
    data.frame()
  })
  return(out)
}
