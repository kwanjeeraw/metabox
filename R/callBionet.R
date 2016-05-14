#'Compute a maximum scoring subnetwork
#'@description compute a maximum scoring subnetwork from p-values of the nodes of a network.
#'The function wraps around the main steps of \pkg{\link{BioNet}} including \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}
#'to compute a subnetwork.
#'@usage callBionet(edgelist, nodelist, pval, fdr)
#'@param edgelist a data frame of edges contains at least a source column (1st column) and a target column (2nd column).
#'@param nodelist a data frame of nodes contains node attributes e.g. node id, node name, node xref. Default is NULL.
#'@param pval a numeric vector of p-values with name attributes identical to the names of network nodes.
#'@param fdr a numeric value specifying false discovery rate. Default is 0.05.
#'@return list of data frame of nodes and data frame of edges with the following components:
#'
#'nodes:
#'
#'\code{nodeAttributes} = node attributes provided
#'
#'\code{score} = node score: positive values = signal content and negative values = background noise
#'
#'edges:
#'
#'\code{source, target}
#'
#'\code{edgeAttributes} = edge attributes provided
#'
#'Return list of empty data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Beisser D., Klau GW., Dandekar T., Müller T. and Dittrich MT. (2010) BioNet: an R-Package for the functional analysis of biological networks. Bioinformatics, 26(8):1129-30
#'@references Dittrich MT., Klau GW., Rosenwald A., Dandekar T., Müller T. (2008) Identifying functional modules in protein-protein interaction networks: an integrated exact approach. Bioinformatics, 24(13):i223-31
#'@seealso \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}
#'@examples
#'#simnw <- computeSimilarity(c(1110,10413,196,51,311,43,764,790)) #compute similarity network for given pubchem compounds
#'#pval <- data.frame(pubchem=c(1110,10413,196,51,311,43,764,790),stat=runif(8, 0, 0.06)) #statistical values of pubchem compounds
#'#result <- computeSubnetwork(sim$edges,sim$nodes,pval=pval,internalid = F)
callBionet <- function (edgelist, nodelist, pval, fdr){
  out <- tryCatch(
  {
    cat("Computing subnetwork ...\n")
    fbModel = BioNet::fitBumModel(pval, plot = FALSE)
    grph = igraph::graph_from_data_frame(edgelist, directed=FALSE, vertices=nodelist)
    scoredNw = BioNet::scoreNodes(network = grph, fb = fbModel, fdr = fdr)
    mdule = NULL #set default
    mdule = BioNet::runFastHeinz(network = grph, scores = scoredNw)
    if(length(igraph::E(mdule))>0){
      cat("Formatting subnetwork ...\n")
      ns = lapply(igraph::list.vertex.attributes(mdule),function(x) igraph::get.vertex.attribute(mdule,x))
      attb = data.frame((sapply(ns,c)), stringsAsFactors = FALSE)
      colnames(attb) = igraph::list.vertex.attributes(mdule)
      colnames(attb)[1] = "id" #set colname
      #attb$nodelabel = ""
      es = lapply(igraph::list.edge.attributes(mdule),function(x) igraph::get.edge.attribute(mdule,x))
      pair = as.data.frame(es)
      pair = cbind(igraph::ends(mdule, igraph::E(mdule), names=TRUE), pair, stringsAsFactors=FALSE)
      colnames(pair) = c("source", "target", igraph::list.edge.attributes(mdule))
      cat("Returning subnetwork of ",nrow(attb)," nodes and ",nrow(pair)," edges...\n")
      list(nodes=attb, edges=pair)
    }else{# if no network found
      cat("Returning no network ...\n")
      cat("Found 0 node and 0 edge...\n")
      list(nodes=data.frame(), edges=data.frame())
    }
  },error=function(e) {
    message(e)
    cat("\nError: RETURN no network ..\n")
    list(nodes=data.frame(), edges=data.frame()) # Choose a return value in case of error
  })
  return(out)
}
