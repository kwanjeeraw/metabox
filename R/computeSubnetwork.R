#'Compute a subnetwork
#'@description compute a subnetwork or module by integrating omic data and the biological network.
#'The function wraps around the main functions of \pkg{\link{BioNet}} including \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}.
#'@usage computeSubnetwork(edgelist, nodelist, pval, fc, fdr, internalid, method, returnas)
#'@param edgelist a data frame of edges contains at least source and target pairs
#'@param nodelist a data frame of nodes contains node information e.g. node name, node xref. Default is NULL.
#'@param pval a numeric vector of p-values with name attributes corresponding to network nodes
#'@param fc a numeric vector of fold changes with name attributes corresponding to network nodes
#'@param fdr a numeric value specifying false discovery rate. Default is 0.05.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param method a string specifying method used to compute the subnetwork. It can be one of bionet, frank. Default is bionet. See \code{details}
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details
#'The method \code{bionet} encapsulates the main functions from \pkg{\link{BioNet}} to compute a functional module or significanly changed subnetworks.
#'Those functions include: \code{\link{fitBumModel}}, \code{\link{scoreNodes}}, \code{\link{runFastHeinz}}.
#'@return list of nodes and edges. The list contains the data frame of nodes and the data frame of edges. colnames of nodes = name (igraph default), ..., score
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
#'@export
computeSubnetwork <- function(edgelist, nodelist=NULL, pval, fc=NULL, fdr=0.05, internalid=TRUE, method="bionet", returnas="dataframe") UseMethod("computeSubnetwork")
#'@export
computeSubnetwork.default <- function (edgelist, nodelist=NULL, pval, fc=NULL, fdr=0.05, internalid=TRUE, method="bionet", returnas="dataframe"){
  out <- tryCatch(
    {
    tmparg <- try(method <- match.arg(tolower(method), c("bionet","frank"), several.ok = FALSE), silent = TRUE)
    if (class(tmparg) == "try-error") {
      stop("argument 'method' is not valid, choose one from the list: bionet,frank")
    }
    if (class(pval) == "data.frame") {#format pval dataframe input: 1st id, 2nd pval
      if(internalid){#format attribute names
        pv = pval[,2]
        names(pv) = pval[,1]
        pval = pv
      }else{
        merged = merge(nodelist, pval, by.x = colnames(nodelist)[2], by.y = colnames(pval)[1], all.y = TRUE)
        pv = merged[,ncol(nodelist)+1]
        names(pv) = merged$id
        pval = pv
      }
    }
    if(tolower(method) == 'bionet'){
      outnw = callBionet(edgelist, nodelist, pval, fdr)
    }else if(tolower(method) == 'frank'){
      stop('Under development')
    }else{
      stop('Unknown method')
    }
    ## output
    switch(returnas,
      dataframe = outnw,
      list = list(nodes= split(outnw$nodes, seq(nrow(outnw$nodes))), edges=split(outnw$edges, seq(nrow(outnw$edges)))),
      json = list(nodes=jsonlite::toJSON(outnw$nodes), edges=jsonlite::toJSON(outnw$edges)),
      stop("Error: incorrect 'returnas' type"))
  },
  error=function(e) {
    message(e)
    cat("\nError: RETURN no network ..\n")
    switch(returnas,
           dataframe = list(nodes=data.frame(), edges=data.frame()),
           list = list(nodes=list(), edges=list()),
           json = list(nodes=list(), edges=list()))
  })
  return(out)
}
