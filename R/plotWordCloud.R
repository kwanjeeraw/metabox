#'plot WordCloud
#'@description plot WordCloudread overriding wordcloud for GUI
#'@usage plotWordCloud(wordtable)
#'@param wordtable data frame of word frequency
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{wordcloud}}
#'@examples
#'# plotWordCloud("wordtable")
#'@export
plotWordCloud <- function(wordtable) UseMethod("plotWordCloud")
#'@export
plotWordCloud.default <- function(wordtable){
  wordcloud::wordcloud(words = wordtable$nodename, freq = wordtable$freq, scale=c(2,.1),min.freq = 1,max.words=50, random.order=FALSE, rot.per=0.5, colors=RColorBrewer::brewer.pal(8, "Dark2"))
}
