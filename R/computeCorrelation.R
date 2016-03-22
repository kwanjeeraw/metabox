#'Compute a weighted correlation network
#'@description compute the correlation network for one or two omic data sets. 
#'The function wraps around \code{\link{corAndPvalue}} function of \pkg{\link{WGCNA}}. 
#'Correlation coefficients, p-values and correlation directions are calculated.
#'The correlation coefficients are continuous values between -1 (negative correlation) and 1 (positive correlation), with numbers close to 1 or -1, meaning very closely correlated.
#'@usage computeCorrelation(x, y, xtype, ytype, coef, pval, method, returnas)
#'@param x a data frame of quantified omic data e.g. gene expression data, metabolite intensities. 
#'Columns are samples and rows are molecular entities e.g. genes, proteins or compounds. 
#'@param y a data frame of quantified omic data e.g. gene expression data, metabolite intensities. 
#'Columns are samples and rows are molecular entities e.g. genes, proteins or compounds.
#'@param xtype a string specifying the type of nodes (default = NULL). It can be one of compound, protein, gene, rna, dna.
#'@param ytype a string specifying the type of nodes (default = NULL). It can be one of compound, protein, gene, rna, dna.
#'@param coef a numeric value specifying the minimum absolute correlation coefficient to be included in the output (from 0 to 1, default is 0.7).
#'@param pval a numeric value specifying the maximum p-value to be included in the output (default is 0.05).
#'@param method a string specifying method to compute correlation. It can be one of pearson, kendall, spearman (default) see \code{\link{cor}}.  
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@details x and y are data frame in which columns are samples and rows are molecular entities e.g. genes, proteins or compounds. 
#'If y is given, then the correlations between the molecular entities of x and of y are computed. Otherwise the correlations between molecular entities of x are computed. 
#'If grinn functions will be used further, we recommend using the grinn ids for entities, see \code{\link{convertId}}.
#'@return 
#'list of nodes with the following components:
#'
#'\code{id} = node id
#'
#'\code{nodename} = node name
#'
#'edgelist with the following components:
#'
#'\code{source, target} = node id
#'
#'\code{coef} = correlation coefficient
#'
#'\code{pval} = p-value
#'
#'\code{direction} = correlation direction
#'
#'\code{type} = relationship type
#'
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Langfelder P. and Horvath S. (2008) WGCNA: an R package for weighted correlation network analysis. BMC Bioinformatics, 9:559 
#'@references Dudoit S., Yang YH., Callow MJ. and Speed TP. (2002) Statistical methods for identifying differentially expressed genes in replicated cDNA microarray experiments, STATISTICA SINICA, 12:111
#'@references Langfelder P. and Horvath S. Tutorials for the WGCNA package \url{http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/index.html}
#'@seealso \code{\link{cor}}, \code{\link{corAndPvalue}}
#'@examples
#'# Compute a correlation network of metabolites
#'#dummy <- rbind(nodetype=rep("metabolite"),t(mtcars))
#'#colnames(dummy) <- c('G1.1','G27967','G371','G4.1',paste0('G',sample(400:22000, 28)))
#'#result <- fetchCorrNetwork(datNormX=dummy, datNormY=NULL, corrCoef=0.7, pval=1e-12, method="spearman", returnAs="tab")
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
#'# Compute a correlation network of metabolites and proteins
#'#dummyX <- rbind(nodetype=rep("metabolite"),t(mtcars)[,1:16])
#'#colnames(dummyX) <- c('G1.1','G27967','G371','G4.1',paste0('G',sample(400:22000, 12)))
#'#dummyY <- rbind(nodetype=rep("protein"),t(mtcars)[,17:32])
#'#colnames(dummyY) <- c('P28845','P08235','Q08AG9','P80365',paste0('P',sample(10000:80000, 12)))
#'dummyX <- mtcars[1:16,]
#'dummyY <- mtcars[17:32,]
#'#result <- computeCorrelation(x=dummyX, y=dummyY, coef=0.7, pval=1e-4, method="spearman", returnas="dataframe")
#'@export
computeCorrelation <- function(x, y=NULL, xtype=NULL, ytype=NULL, coef=0.7, pval=0.05, method="spearman", returnas="dataframe") UseMethod("computeCorrelation")
#'@export
computeCorrelation.default <- function(x, y=NULL, xtype=NULL, ytype=NULL, coef=0.7, pval=0.05, method="spearman", returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(method <- match.arg(tolower(method), c("pearson","kendall","spearman"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'method' is not valid, choose one from the list: pearson,kendall,spearman")
      }
      if(is.null(y)) {#Assign values to a square matrix
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
        if(nrow(network)>0){
          network$type = "CORRELATION"
          cat("Format and returning network nodes ...\n")
          #format nodeList from edgeList
          so = data.frame(id=network$source, nodename=network$source, stringsAsFactors = FALSE)
          so$nodelabel = if(!is.null(xtype)) Hmisc::capitalize(xtype)
          ta = data.frame(id=network$target, nodename=network$target, stringsAsFactors = FALSE)
          ta$nodelabel= if(!is.null(xtype)) Hmisc::capitalize(xtype)
          networknode = unique(rbind(so,ta))
          
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
      }else if(!is.null(y)) {
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
        if(nrow(network)>0){
          network$type = "CORRELATION"
          cat("Format and returning network nodes ...\n")
          #format nodeList from edgeList
          so = data.frame(id=network$source, nodename=network$source, stringsAsFactors = FALSE)
          so$nodelabel = if(!is.null(xtype)) Hmisc::capitalize(xtype)
          ta = data.frame(id=network$target, nodename=network$target, stringsAsFactors = FALSE)
          ta$nodelabel = if(!is.null(ytype)) Hmisc::capitalize(ytype)
          networknode = unique(rbind(so,ta))
          
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
      }
      
      #network = network[abs(network$coef) > coef, ]
      #network = network[network$pval < pval, ]
#       cat("Format and returning network of size ",nrow(network)," ...\n")
#       if(nrow(network)>0){
#         network$type = "CORRELATION"
#         cat("Format and returning network nodes ...\n")
#         #format nodeList from edgeList
#         so = data.frame(id=network$source, stringsAsFactors = FALSE)
#         ta = data.frame(id=network$target, stringsAsFactors = FALSE)
#         networknode = unique(rbind(so,ta))
#         
#         ## output
#         switch(returnas,
#                dataframe = list(nodes = networknode, edges = network),
#                list = list(nodes = split(networknode, seq(nrow(networknode))), edges = split(network, seq(nrow(network)))),
#                json = list(nodes = jsonlite::toJSON(networknode), edges = jsonlite::toJSON(network)),
#                stop("Error: incorrect 'returnas' type"))
#       }else{
#         switch(returnas,
#                dataframe = list(nodes = data.frame(), edges = data.frame()),
#                list = list(nodes = list(), edges = list()),
#                json = list(nodes = "", edges = ""))
#       }
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