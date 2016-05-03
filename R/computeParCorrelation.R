#'Compute a partial correlation network
#'@description compute the partial correlation network for omic data.
#'The function wraps around the functions of \pkg{\link{qpgraph}}.
#'Partial correlation coefficients, p-values and correlation directions are calculated.
#'The partial correlation coefficients are continuous values between -1 (negative correlation) and 1 (positive correlation), with numbers close to 1 or -1, meaning very closely correlated.
#'@usage computeParCorrelation(x, xtype, internalid, coef, pval, alpha, epsilon, matrix.completion, returnas)
#'@param x a data frame of quantified omic data e.g. gene expression data, metabolite intensities.
#'Columns are samples and rows are molecular entities e.g. genes, proteins or compounds.
#'@param xtype a string specifying the type of nodes (default = NULL). It can be one of compound, protein, gene, rna, dna.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param coef a numeric value specifying the minimum absolute partial correlation coefficient to be included in the output (from 0 to 1, default is 0.7).
#'@param pval a numeric value specifying the maximum p-value to be included in the output (default is 0.05).
#'@param alpha a numeric value specifying significance level of each test used in \code{\link{qpAvgNrr}}.
#'@param epsilon a numeric value specifying the maximum cutoff value of the non-rejection rate met by the edges that are included in the qp-graph, see \code{\link{qpGraph}}.
#'@param matrix.completion a string specifying algorithm to employ in the matrix completion operations used in \code{\link{qpPAC}}
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return
#'list of nodes with the following components:
#'
#'\code{id} = node id
#'
#'\code{gid} = node gid
#'
#'\code{nodename} = node name
#'
#'edgelist with the following components:
#'
#'\code{source, target} = node
#'
#'\code{coef} = partial correlation coefficient
#'
#'\code{pval} = p-value
#'
#'\code{direction} = correlation direction
#'
#'\code{type} = relationship type
#'
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Castelo R. and Roverato A. (2006) A robust procedure for Gaussian graphical model search from microarray data with p larger than n. J. Mach. Learn. Res., 7:2621-2650.
#'@references Castelo R. and Roverato A. (2009) Reverse engineering molecular regulatory networks from microarray data with qp-graphs. J Comput Biol, 16(2), pp. 213-27.
#'@seealso \pkg{\link{qpgraph}}, \code{\link{qpAvgNrr}}, \code{\link{qpGraph}}, \code{\link{qpPAC}}
#'#library(qpgraph)
#'#library(snow)
#'#library(rlecuyer)
#'#datNorm = read.csv("~/Documents/grinn_sample/lung_miyamoto/metAdj.txt",sep="\t",header=TRUE)
#'#datNorm = datNorm[-1,] #exclude nodetype
#'#dt = sapply(datNorm, function(x) as.numeric(as.character(x)))
#'#nw = fetchPtCorrNetwork(datNorm=dt)
#'@export
computeParCorrelation <- function(x, xtype=NULL, internalid = TRUE, coef=0.7, pval=0.05, alpha=0.05, epsilon=0.5, matrix.completion="IPF", returnas="dataframe") UseMethod("computeParCorrelation")
#'@export
computeParCorrelation.default <- function (x, xtype=NULL, internalid = TRUE, coef=0.7, pval=0.05, alpha=0.05, epsilon=0.5, matrix.completion="IPF", returnas="dataframe")
{
  out <- tryCatch(
    {
      tmparg <- try(matrix.completion <- match.arg(toupper(matrix.completion), c("IPF","HTF"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'matrix.completion' is not valid, choose one from the list: IPF,HTF")
      }
    cat("Formating row.names of input data frame ...\n")
    tmp = x[,2:ncol(x)]
    row.names(tmp) = x[,1]
    x = tmp
    cat("Computing partial correlation ...\n")
    nrr.estimates = qpgraph::qpAvgNrr(x, alpha=alpha)
    g = qpgraph::qpGraph(nrr.estimates, epsilon=epsilon)
    pac.estimates = qpgraph::qpPAC(x, g=g@g, matrix.completion=matrix.completion)

    #format output
    nRow = nrow(pac.estimates$R)
    nNames = dimnames(pac.estimates$R)[[1]]
    rowMat = matrix(c(1:nRow), nRow, nRow, byrow = TRUE)
    colMat = matrix(c(1:nRow), nRow, nRow)
    dstRows = as.dist(rowMat)
    dstCols = as.dist(colMat)
    network = data.frame(source = as.character(nNames[dstRows]), target = as.character(nNames[dstCols]), coef = pac.estimates$R[lower.tri(pac.estimates$R)],
                         pval = pac.estimates$P[lower.tri(pac.estimates$P)], direction = sign(pac.estimates$R[lower.tri(pac.estimates$R)]), stringsAsFactors = FALSE)
    network = network[!is.na(network$pval),]
    network = network[abs(network$coef) > coef, ]
    network = network[network$pval < pval, ]
    cat("Format and returning network of size ",nrow(network)," ...\n")
    if(nrow(network)>0){
      network$type = "PARTIAL_CORRELATION"
      cat("Format and returning network nodes ...\n")
      if(!is.null(xtype)){#given xtype, search DB for nodes
        nodelist = unique(c(network$source, network$target))
        if(internalid){
          nodels = lapply(nodelist, formatNode.LIST, y=xtype, z="neo4jid") #query nodes
          networknode = plyr::ldply(nodels, data.frame)
        }else{
          nodels = lapply(nodelist, formatNode.LIST, y=xtype, z="grinnid") #query nodes by gid
          networknode = plyr::ldply(nodels, data.frame)
          #format edge
          edgedf = merge(networknode[,1:2],network,by.x='gid',by.y='target')[,-1]
          colnames(edgedf)[1] = "target"
          edgedf = merge(networknode[,1:2],edgedf,by.x='gid',by.y='source')[,-1]
          colnames(edgedf)[1] = "source"
          network = edgedf
        }
      }else{#no xtype specified, return input
        #format nodeList from edgeList
        so = data.frame(id=network$source, gid=network$source, nodename=network$source, stringsAsFactors = FALSE)
        ta = data.frame(id=network$target, gid=network$target, nodename=network$target, stringsAsFactors = FALSE)
        networknode = unique(rbind(so,ta))
      }
      ## output
      switch(returnas,
             dataframe = list(nodes = networknode, edges = network),
             list = list(nodes = split(networknode, seq(nrow(networknode))), edges = split(network, seq(nrow(network)))),
             json = list(nodes = jsonlite::toJSON(networknode), edges = jsonlite::toJSON(network)),
             stop("Error: incorrect 'returnas' type"))
    }else{
      switch(returnas,
             dataframe = list(nodes = data.frame(), edges = data.frame()),
             list = list(nodes = list(), edges = list()),
             json = list(nodes = "", edges = ""))
    }
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
