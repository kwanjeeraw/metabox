#'Compute wordcloud for a given annotation
#'@description compute wordcloud for a given annotation retrieved from e.g. \code{\link{fetchNetwork}}.
#'@param edgelist a data frame of annotation pairs (e.g. source target = pathway compound)
#'@param nodelist a data frame of nodes containin node information e.g. node id, node gid, node name, node xref.
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \pkg{\link{tm}}, \pkg{\link{wordcloud}}
#'@return data frame of word frequency. Return empty data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@examples
#'# pc = read.csv("~/Desktop/testGrinn/pubchemls.txt")
#'# nw = fetchNetworkByGID(to=pc$GrinnID,fromtype = "pathway",totype = "compound",reltype = "annotation")
#'# wd = callWordCloud(nw$edges,nw$nodes)
#'# wordcloud::wordcloud(words = gsub(" - Mus musculus \\(mouse\\)","",wd$nodename), freq = wd$freq, scale=c(2,.1),min.freq = 1,max.words=50, random.order=FALSE, rot.per=0.5, colors=brewer.pal(8, "Dark2"))
#'# barplot(wd$freq[1:10], las = 2, names.arg = gsub(" - Mus musculus \\(mouse\\)","",wd$nodename[1:10]), col ="lightblue", main ="Most frequent words", ylab = "Word frequencies")
callWordCloud <- function(edgelist, nodelist) UseMethod("callWordCloud")
callWordCloud.default <- function(edgelist, nodelist){
  out <- tryCatch(
  {
    cat("Computing wordCloud ...\n")
    docs = tm::Corpus(tm::VectorSource(edgelist$source))
    docs = tm::tm_map(docs, tm::content_transformer(tolower))
    dtm = tm::TermDocumentMatrix(docs)
    mt = as.matrix(dtm)
    v = sort(rowSums(mt), decreasing=TRUE)
    df = data.frame(word = names(v), freq = v)
    wt = merge(nodelist, df, by.x='id', by.y='word')
    wt[order(wt$freq, decreasing = TRUE),]
  },
  error=function(e) {
    message(e)
    cat("\nError: RETURN no result ..\n")
    data.frame()
  })
  return(out)
}
