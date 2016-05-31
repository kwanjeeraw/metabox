#'Compute partial correlation network
#'@description compute the partial correlation network of entities from raw or quantified data, see details.
#'@usage computeParCorrelation(x, xtype, internalid, coef, pval, alpha, epsilon, matrix.completion, returnas)
#'@param x a data frame of raw or quantified data e.g. gene expression data, metabolite intensities.
#'Columns are samples and rows are entities e.g. genes, proteins or compounds.
#'@param xtype a string specifying a node type (default = NULL). If provided and the database is installed, node attributes will be automatically retrieved from the database.
#'For database query, the value can be one of compound, protein, gene, rna, dna.
#'@param internalid a logical value indicating whether the network nodes are neo4j ids, if TRUE (default). See \code{\link{convertId}} for how to convert ids.
#'It has no effect if \code{xtype} = NULL or there is no database installed.
#'@param coef a numeric value specifying the minimum absolute partial correlation coefficient to be included in the output (from 0 to 1, default is 0.7).
#'@param pval a numeric value specifying the maximum p-value to be included in the output (default is 0.05).
#'@param alpha a numeric value specifying significance level of each test used in \code{\link{qpAvgNrr}}.
#'@param epsilon a numeric value specifying the maximum cutoff value of the non-rejection rate met by the edges that are included in the qp-graph, see \code{\link{qpGraph}}.
#'@param matrix.completion a string specifying algorithm to employ in the matrix completion operations used in \code{\link{qpPAC}}
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details The function wraps around the functions of \pkg{\link{qpgraph}}.
#'Partial correlation coefficients, p-values and correlation directions are calculated.
#'The partial correlation coefficients are continuous values between -1 (negative correlation) and 1 (positive correlation), with numbers close to 1 or -1, meaning very closely correlated.
#'@return list of network information with the following components:
#'
#'nodes:
#'
#'\code{id} = node id or node neo4j id
#'
#'\code{gid} = node id or node grinn id
#'
#'\code{nodename} =  node id or node name
#'
#'\code{nodelabel} = node type if provided
#'
#'edges:
#'
#'\code{source, target} = node id or node neo4j id
#'
#'\code{coef} = partial correlation coefficient
#'
#'\code{pval} = p-value
#'
#'\code{direction} = correlation direction
#'
#'\code{type} = relationship type
#'
#'Return empty list if error or found nothing.
#'@note If the database is installed, node attributes will be automatically retrieved from the database. Otherwise node attributes will be the original input.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Castelo R. and Roverato A. (2006) A robust procedure for Gaussian graphical model search from microarray data with p larger than n. J. Mach. Learn. Res., 7:2621-2650.
#'@references Castelo R. and Roverato A. (2009) Reverse engineering molecular regulatory networks from microarray data with qp-graphs. J Comput Biol, 16(2), pp. 213-27.
#'@seealso \pkg{\link{qpgraph}}, \code{\link{qpAvgNrr}}, \code{\link{qpGraph}}, \code{\link{qpPAC}}
#'@examples
#'#dt <- data.frame(id=row.names(mtcars), mtcars, row.names = NULL) #data frame of x
#'#result <- computeParCorrelation(x = dt)
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
      gc()
      cat("Formating row.names of input data frame ...\n")
      #tmp = x[,2:ncol(x)]
      tmp = as.data.frame(lapply(x[,-1], as.numeric))
      row.names(tmp) = x[,1]
      x = tmp
      cat("Computing partial correlation ...\n")
      nrr.estimates = qpgraph::qpAvgNrr(x, alpha=alpha)
      g = qpgraph::qpGraph(nrr.estimates, epsilon=epsilon)
      pac.estimates = qpgraph::qpPAC(x, g=g@g, matrix.completion=matrix.completion, tol=1)
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
      if(nrow(network)>0){#pass cutoff
        network$type = "PARTIAL_CORRELATION"
        cat("Format and returning network nodes ...\n")
        if(!is.null(xtype) && foundDb()){#given xtype and have db, search db for node attributes
          nodelist = unique(c(network$source, network$target))
          if(internalid){
            nodels = lapply(nodelist, formatNode.LIST, y=xtype, z="neo4jid") #query nodes
            networknode = plyr::ldply(nodels, data.frame)
          }else{
            nodels = lapply(nodelist, formatNode.LIST, y=xtype, z="grinnid") #query nodes by gid
            networknode = plyr::ldply(nodels, data.frame)
            #format edge, change gid to id, fix edge row order
            s = dplyr::right_join(networknode[,1:2],network[,1:2],by=c('gid' = 'source'))
            t = dplyr::right_join(networknode[,1:2],network[,1:2],by=c('gid' = 'target'))
            network$source = s$id
            network$target = t$id
          }
        }else{#no xtype specified or no db installed, return original input
          #format nodeList from edgeList
          so = data.frame(id=network$source, gid=network$source, nodename=network$source, stringsAsFactors = FALSE)
          so$nodelabel = if(!is.null(xtype)) Hmisc::capitalize(xtype)
          ta = data.frame(id=network$target, gid=network$target, nodename=network$target, stringsAsFactors = FALSE)
          ta$nodelabel = if(!is.null(xtype)) Hmisc::capitalize(xtype)
          networknode = unique(rbind(so,ta))
        }
        ## output
        switch(returnas,
               dataframe = list(nodes = networknode, edges = network),
               list = list(nodes = split(networknode, seq(nrow(networknode))), edges = split(network, seq(nrow(network)))),
               json = list(nodes = jsonlite::toJSON(networknode), edges = jsonlite::toJSON(network)),
               stop("Error: incorrect 'returnas' type"))
      }else{#not pass cutoff
        switch(returnas,
               dataframe = list(nodes = data.frame(), edges = data.frame()),
               list = list(nodes = list(), edges = list()),
               json = list(nodes = "", edges = ""))
      }
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no network ..\n")
      switch(returnas,
             dataframe = list(nodes = data.frame(), edges = data.frame()),
             list = list(nodes = list(), edges = list()),
             json = list(nodes = "", edges = ""))
    })
  return(out)
}
