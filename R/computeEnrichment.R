#'Perform enrichment analysis
#'@description perform enrichment analysis by integrating the statistical values of molecular entities and the compound, protein or gene set collections.
#'The function wraps around the main functions of \pkg{\link{piano}}.
#'@usage computeEnrichment(edgelist, pval, fc, method, size, returnas)
#'@param edgelist a two-column data frame of molecular entities and entity set collections. 
#'The first column contains molecular entities and the second column contains entity set collections. See \code{\link{loadGSC}} for details.
#'@param pval a numeric vector containing the p-values of molecular entities, computed from statistical analysis, 
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param fc a numeric vector containing fold changes or sign information (positive or negative) of molecular entities 
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param method a string specifying the enrichment analysis method. It can be one of reporter (default), fisher, median, mean, stouffer. See \code{\link{runGSA}}
#'@param size a numeric vector specifying the minimum and maximum number of members in each entity set collection to be used in the analysis. Default is c(3,500).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return entity sets with the following components:
#'
#'\code{id} = id of entity set
#'
#'\code{amount} = number of members in each entity set
#'
#'\code{Stat} = entity set statistics
#'
#'\code{p} = entity set p-values
#'
#'\code{p adj} = adjusted entity set p-values
#'
#'\code{member} = list of entities
#'See \code{\link{runGSA}} for more details about directional classes. Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Fisher R. (1932) Statistical methods for research workers. Oliver and Boyd, Edinburgh.
#'@references Stouffer S., Suchman E., Devinney L., Star S., and Williams R. (1949) The American soldier: adjustment during army life. Princeton University Press, Oxford, England.
#'@references Patil K. and Nielsen J. (2005) Uncovering transcriptional regulation of metabolism by using metabolic network topology. Proceedings of the National Academy of Sciences of the United States of America 102(8), 2685.
#'@references Oliveira A., Patil K., and Nielsen J. (2008) Architecture of transcriptional regulatory circuits is knitted over the topology of bio-molecular interaction networks. BMC Systems Biology 2(1), 17.
#'@references Väremo L., Nielsen J., and Nookaew I. (2013) Enriching the gene set analysis of genome-wide data by incorporating directionality of gene expression and combining statistical hypotheses and methods. Nucleic Acids Research, 41(8), pp. 4378-4391.
#'@seealso \code{\link{loadGSC}}, \code{\link{runGSA}}, \code{\link{GSAsummaryTable}}
#'@examples
#'#kw <- c('G15729','G17561','G16015','G18145','G16708')
#'#grinnNW <- fetchGrinnNetwork(txtInput=kw, from="metabolite", to="protein")
#'#library(grinn)
#'#data(dummyStat)
#'#result <- fetchSubnetwork(statInput = dummyStat, nwInput = grinnNW)
#'#library(igraph)
#'#plot(graph.data.frame(result$edges[,1:2], directed=FALSE))
#'@export
computeEnrichment <- function(edgelist, pval, fc=NULL, method="reporter", size=c(3,500), returnas="dataframe") UseMethod("computeEnrichment")
#'@export
computeEnrichment.default <- function (edgelist, pval, fc=NULL, method="reporter", size=c(3,500), returnas="dataframe"){
  out <- tryCatch(
    {
    tmparg <- try(method <- match.arg(tolower(method), c("reporter","fisher","median","mean","stouffer"), several.ok = FALSE), silent = TRUE)
    if (class(tmparg) == "try-error") {
      stop("argument 'method' is not valid, choose one from the list: reporter,fisher,median,mean,stouffer")
    }
    
    gs = piano::loadGSC(edgelist, type="data.frame")
    gsaRes = piano::runGSA(geneLevelStats=pval, directions=fc, gsc=gs, geneSetStat=method, gsSizeLim=size)
    resTab = piano::GSAsummaryTable(gsaRes)

    cat("Formatting output ...\n")
    memls = sapply(resTab$Name, function(x) names(piano::geneSetSummary(gsaRes, geneSet=x)$geneLevelStats), simplify=TRUE, USE.NAMES=TRUE)
    resTab$member = memls
    colnames(resTab) = gsub("Genes","amount",colnames(resTab))
    colnames(resTab) = gsub("Name","id",colnames(resTab))

    cat("Returning output of size ",nrow(resTab)," ...\n")
    if(nrow(resTab)>0){
      ## output
      switch(returnas,
             dataframe = resTab,
             list = split(resTab, seq(nrow(resTab))),
             json = jsonlite::toJSON(resTab),
             stop("Error: incorrect 'returnas' type"))
    }else{
      switch(returnas,
             dataframe = data.frame(),
             list = list(),
             json = list())
    }
  },
  error=function(e) {
    message(e)
    cat("\nError: RETURN no data ..\n")
    switch(returnas,
           dataframe = data.frame(),
           list = list(),
           json = list())
  })
  return(out)
}