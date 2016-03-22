#'Combine networks
#'@description The function can combine networks computed from \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \code{\link{computeCorrelation}}, 
#'\code{\link{computeParCorrelation}}, and \code{\link{computeSimilarity}}.
#'@usage combineNetworks(networks, returnas)
#'@param networks list of networks.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return 
#'list of nodes and 
#'list of edges with the following components:
#'
#'\code{source, target} = node internal neo4j id
#'
#'\code{type} = relationship type
#'
#'\code{datasource} = relationship resource
#'
#'\code{properties} = relationship properties
#'
#'and other information such as correlation coefficient, pvalue, etc.
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@seealso \code{\link{fetchNetwork}}, \code{\link{fetchHetNetwork}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSimilarity}}
#'@examples
#'# Create metabolite-protein network from the list of metabolites using grinn ids and combine the grinn network to a correlation network
#'#kw <- c('G160','G300','G371')
#'#grinnnw <- fetchGrinnNetwork(txtInput=kw, from="metabolite", to="protein")
#'#dummy <- rbind(nodetype=rep("metabolite"),t(mtcars))
#'#colnames(dummy) <- c('G160','G300','G371','G367',paste0('G',sample(400:22000, 28)))
#'#corrnw <- fetchCorrNetwork(datNormX=dummy, datNormY=NULL, corrCoef=0.5, pval= 1e-12, method="pearson", returnAs="tab")
#'#result <- combineNetwork(grinnnw,corrnw)
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
#'# Create metabolite-protein network from the list of metabolites using grinn ids and combine the grinn network to a provided network
#'#txtInput <- list('G371','G783','G1.1')
#'#grinnnw <- fetchGrinnNetwork(txtInput, from="metabolite", to="protein")
#'#othernw = list()
#'#othernw$nodes = data.frame(id=c("G371","G783","XXX","YYY"),nodename=c("G371","G783","XXX","YYY"),nodetype=c("metabolite","metabolite","protein","protein"),stringsAsFactors = FALSE)
#'#othernw$edges = data.frame(source=c("G371","G783"),target=c("XXX","YYY"),corr_coef=c(-0.368,0.385),pval=c(0.000927,0.000497),reltype=c("Metabolite_Protein","Metabolite_Protein"),relname=c("CORRELATION","CORRELATION"),stringsAsFactors = FALSE)
#'#result <- combineNetwork(grinnnw,othernw)
#'@export
combineNetworks <- function(networks, returnas="dataframe") UseMethod("combineNetworks")
#'@export
combineNetworks.default <- function(networks, returnas="dataframe"){
  out <- tryCatch(
    {
    cat("Formating network ...\n")
    nodels = unlist(lapply(networks,function(x) list(x$nodes)), recursive = FALSE)
    attb = plyr::rbind.fill(nodels)
    attb = attb[order(attb$nodename,decreasing = TRUE),]
    attb = attb[!duplicated(attb$id),]
    edgels = unlist(lapply(networks,function(x) list(x$edges)), recursive = FALSE)
    pair = plyr::rbind.fill(edgels)
    pair = unique(pair)
    
    cat("Returning network of size ",nrow(pair)," ...\n")
    ## output
    switch(returnas,
           dataframe = list(nodes=attb, edges=pair),
           list = list(nodes= split(attb, seq(nrow(attb))), edges=split(pair, seq(nrow(pair)))),
           json = list(nodes=jsonlite::toJSON(attb), edges=jsonlite::toJSON(pair)),
           stop("Error: incorrect 'returnas' type"))
    },
    error=function(e) {
      message(e)
      cat("\nError: RETURN no network ..\n")
      switch(returnas,
             dataframe = list(nodes = data.frame(), edges = data.frame()),
             list = list(nodes = list(), edges = list()),
             json = list(nodes = "", edges = ""))
    })
  return(out)
}