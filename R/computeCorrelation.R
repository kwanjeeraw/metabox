#'Compute weighted correlation network
#'@description compute the correlation network of entities from one or two quantified data sets, see details.
#'@usage computeCorrelation(x, y, xtype, ytype, internalid, coef, pval, method, returnas)
#'@param x a data frame of raw or quantified data e.g. gene expression data, metabolite intensities.
#'Columns are samples and rows are molecular entities e.g. genes, proteins or compounds.
#'@param y a data frame of raw or quantified data e.g. gene expression data, metabolite intensities.
#'Columns are samples and rows are molecular entities e.g. genes, proteins or compounds. See details.
#'@param xtype a string specifying a node type (default = NULL). If provided and the database is installed, node attributes will be automatically retrieved from the database.
#'For database query, the value can be one of compound, protein, gene, rna, dna.
#'@param ytype a string specifying a node type (default = NULL).
#'@param internalid a logical value indicating whether the network nodes are neo4j ids, if TRUE (default). See \code{\link{convertId}} for how to convert ids.
#'It has no effect if \code{xtype} = NULL or there is no database installed.
#'@param coef a numeric value specifying the minimum absolute correlation coefficient to be included in the output (from 0 to 1, default is 0.7).
#'@param pval a numeric value specifying the maximum p-value to be included in the output (default is 0.05).
#'@param method a string specifying method for computing correlation. It can be one of pearson, kendall, spearman (default). See \code{\link{cor}} for details.
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details The function wraps around \code{\link{corAndPvalue}} function of \pkg{\link{WGCNA}}. Correlation coefficients, p-values and correlation directions are calculated.
#'The correlation coefficients are continuous values between -1 (negative correlation) and 1 (positive correlation), with numbers close to 1 or -1, meaning very closely correlated.
#'
#'x and y are data frame in which columns are samples and rows are entities.
#'If y is given, then the correlations between the x entities and y entities are computed. Otherwise the correlations between x entities are computed.
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
#'\code{coef} = correlation coefficient
#'
#'\code{pval} = p-value
#'
#'\code{direction} = correlation direction
#'
#'\code{type} = relationship type
#'
#'Return empty list if error or found nothing.
#'@note If only one data set \code{x} is provided and the database is installed, node attributes will be automatically retrieved from the database. Otherwise node attributes will be the original input.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Langfelder P. and Horvath S. (2008) WGCNA: an R package for weighted correlation network analysis. BMC Bioinformatics, 9:559
#'@references Dudoit S., Yang YH., Callow MJ. and Speed TP. (2002) Statistical methods for identifying differentially expressed genes in replicated cDNA microarray experiments, STATISTICA SINICA, 12:111
#'@references Langfelder P. and Horvath S. Tutorials for the WGCNA package \url{http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/index.html}
#'@seealso \code{\link{cor}}, \code{\link{corAndPvalue}}
#'@examples
#'# Compute a correlation network from compound data
#'#dt <- data.frame(id=seq(32), mtcars, row.names = NULL) #data frame of x
#'#result <- computeCorrelation(x = dt, xtype = "compound", coef=0.9, pval = 1e-10)
#'# Compute a correlation network from two data sets
#'#dtX <- data.frame(id=seq(16), mtcars[1:16,], row.names = NULL) #data frame of x
#'#dtY <- data.frame(id=seq(17,32), mtcars[17:32,], row.names = NULL) #data frame of x
#'#result <- computeCorrelation(x=dtX, y=dtY, coef=0.9, pval = 1e-7)
#'@export
computeCorrelation <- function(x, y=NULL, xtype=NULL, ytype=NULL, internalid = TRUE, coef=0.7, pval=0.05, method="spearman", returnas="dataframe") UseMethod("computeCorrelation")
#'@export
computeCorrelation.default <- function(x, y=NULL, xtype=NULL, ytype=NULL, internalid = TRUE, coef=0.7, pval=0.05, method="spearman", returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(method <- match.arg(tolower(method), c("pearson","kendall","spearman"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'method' is not valid, choose one from the list: pearson,kendall,spearman")
      }
      if(is.null(y)) {#have one input, assign values to a square matrix
        cat("Formating row.names of input data frame ...\n")
        tmp = x[,2:ncol(x)]
        row.names(tmp) = x[,1]
        x = tmp
        cat("Computing correlation ...\n")
        rec = WGCNA::corAndPvalue(t(x), method=method)
        #format output
        nRow = nrow(rec$cor)
        nNames = dimnames(rec$cor)[[1]]
        rowMat = matrix(c(1:nRow), nRow, nRow, byrow = TRUE)
        colMat = matrix(c(1:nRow), nRow, nRow)
        dstRows = as.dist(rowMat)
        dstCols = as.dist(colMat)
        network = data.frame(source = as.character(nNames[dstRows]), target = as.character(nNames[dstCols]), coef = rec$cor[lower.tri(rec$cor)],
                              pval = rec$p[lower.tri(rec$p)], direction = sign(rec$cor[lower.tri(rec$cor)]), stringsAsFactors = FALSE)
        network = network[abs(network$coef) > coef, ]
        network = network[network$pval < pval, ]
        cat("Format and returning network of size ",nrow(network)," ...\n")
        if(nrow(network)>0){#pass cutoff
          network$type = "CORRELATION"
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
      }else if(!is.null(y)) {#have another data
        cat("Formating row.names of x input data frame ...\n")
        tmp = x[,2:ncol(x)]
        row.names(tmp) = x[,1]
        x = tmp
        cat("Formating row.names of y input data frame ...\n")
        tmp = y[,2:ncol(y)]
        row.names(tmp) = y[,1]
        y = tmp
        cat("Computing correlation ...\n")
        rec = WGCNA::corAndPvalue(t(x), t(y), method=method)
        #format output
        rec_df = reshape2::melt(rec$cor) #matrix to data frame
        colnames(rec_df) = c("source","target","coef")
        #corAdj_df = p.adjust(p=recp_df$pval,method="none") #p-adjusted, no adjustment in this version
        network = data.frame(source = as.character(rec_df$source), target = as.character(rec_df$target), coef = rec_df$coef, pval = reshape2::melt(rec$p)[,3], direction = sign(rec_df$coef), stringsAsFactors = FALSE)
        network = network[abs(network$coef) > coef, ]
        network = network[network$pval < pval, ]
        cat("Format and returning network of size ",nrow(network)," ...\n")
        if(nrow(network)>0){#pass cutoff
          network$type = "CORRELATION"
          cat("Format and returning network nodes ...\n")
          #format nodeList from edgeList
          so = data.frame(id=network$source, gid=network$source, nodename=network$source, stringsAsFactors = FALSE)
          so$nodelabel = if(!is.null(xtype)) Hmisc::capitalize(xtype)
          ta = data.frame(id=network$target, gid=network$target, nodename=network$target, stringsAsFactors = FALSE)
          ta$nodelabel = if(!is.null(ytype)) Hmisc::capitalize(ytype)
          networknode = unique(rbind(so,ta))

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
