#'Compute a maximum scoring subnetwork
#'@description compute a maximum scoring subnetwork or module by integrating omic data and the biological network.
#'The function wraps around the main functions of \pkg{\link{BioNet}} including \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}.
#'@usage callBionet(edgelist, nodelist, pval, fdr)
#'@param edgelist a data frame of edges
#'@param nodelist a data frame of nodes containin node information e.g. node name, node xref. Default is NULL.
#'@param pval a numeric vector of p-values with name attributes
#'@param fdr a numeric value specifying false discovery rate. Default is 0.05.
#'@details
#'The method \code{bionet} encapsulates the main functions from \pkg{\link{BioNet}} to compute a functional module or significanly changed subnetworks. 
#'Those functions include: \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}.
#'@return list of nodes and edges. The list contains the data frame of nodes and the data frame of edges.
#'Return empty list of data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Beisser D., Klau GW., Dandekar T., Müller T. and Dittrich MT. (2010) BioNet: an R-Package for the functional analysis of biological networks. Bioinformatics, 26(8):1129-30 
#'@references Dittrich MT., Klau GW., Rosenwald A., Dandekar T., Müller T. (2008) Identifying functional modules in protein-protein interaction networks: an integrated exact approach. Bioinformatics, 24(13):i223-31
#'@seealso \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}
#'@examples
#'#kw <- c('G15729','G17561','G16015','G18145','G16708')
#'#grinnNW <- fetchGrinnNetwork(txtInput=kw, from="metabolite", to="protein")
#'#library(grinn)
#'#data(dummyStat)
#'#result <- fetchSubnetwork(statInput = dummyStat, nwInput = grinnNW)
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
callBionet <- function (edgelist, nodelist, pval, fdr){
  out <- tryCatch(
  {
    cat("Computing subnetwork ...\n")
    fbModel = BioNet::fitBumModel(pval, plot = FALSE)
    grph = igraph::graph_from_data_frame(edgelist, directed=FALSE, vertices=nodelist)
    scoredNw = BioNet::scoreNodes(network = grph, fb = fbModel, fdr = fdr)
    mdule = NULL #set default
    mdule = BioNet::runFastHeinz(network = grph, scores = scoredNw)
    
    if(!is.null(mdule)){
      cat("Formatting subnetwork ...\n")
      ns = lapply(igraph::list.vertex.attributes(mdule),function(x) igraph::get.vertex.attribute(mdule,x))
      attb = data.frame((sapply(ns,c)), stringsAsFactors = FALSE)
      colnames(attb) = igraph::list.vertex.attributes(mdule)
      colnames(attb)[1] = "id" #set colname
      #attb$nodelabel = ""
      es = lapply(igraph::list.edge.attributes(mdule),function(x) igraph::get.edge.attribute(mdule,x))
      pair = data.frame((sapply(es,c)))
      pair = cbind(igraph::ends(mdule, igraph::E(mdule), names=TRUE), pair, stringsAsFactors=FALSE)
      colnames(pair) = c("source", "target", igraph::list.edge.attributes(mdule))
      cat("Returning subnetwork of ",nrow(attb)," nodes and ",nrow(pair)," edges...\n")
      list(nodes=attb, edges=pair)
    }else{# if no network found
      cat("Returning no network ...\n")
      cat("Found 0 node and 0 edge...\n")
      list(nodes=data.frame(), edges=data.frame())
    }
  },
  error=function(e) {
    message(e)
    cat("\nError: RETURN no network ..\n")
    list(nodes=data.frame(), edges=data.frame()) # Choose a return value in case of error
  })
  return(out)
}