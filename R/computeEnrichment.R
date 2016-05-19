#'Perform enrichment analysis
#'@description perform enrichment analysis from p-values of entities. The function wraps around the main functions of \pkg{\link{piano}}.
#'@usage computeEnrichment(edgelist, pval, fc, method, size, returnas)
#'@param edgelist a two-column data frame of annotation pairs. The 1st column contains annotated entities and the 2nd column contains annotation terms. See \code{\link{loadGSC}} for details.
#'@param pval a numeric vector of statistical values e.g. p-values. The name attributes must be identical to the names of entities. See \code{\link{runGSA}} for details.
#'@param fc a numeric vector of fold changes or sign information (positive or negative) with name attributes identical to the names of entities. See \code{\link{runGSA}} for details. Default is NULL.
#'@param method a string specifying the enrichment analysis method. It can be one of reporter (default), fisher, median, mean, stouffer. See \code{\link{runGSA}}
#'@param size a numeric vector specifying the minimum and maximum number of members in each annotation term to be used in the analysis. Default is c(3,500).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return enrichment analysis result with the following components:
#'
#'\code{id} = annotation id
#'
#'\code{no_of_entities} = number of members in each annotation term
#'
#'\code{p} = raw p-values
#'
#'\code{p_adj} = adjusted p-values
#'
#'\code{member} = list of entity members of the annotation term
#'
#'Return empty list or data frame if error or found nothing.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Fisher R. (1932) Statistical methods for research workers. Oliver and Boyd, Edinburgh.
#'@references Stouffer S., Suchman E., Devinney L., Star S., and Williams R. (1949) The American soldier: adjustment during army life. Princeton University Press, Oxford, England.
#'@references Patil K. and Nielsen J. (2005) Uncovering transcriptional regulation of metabolism by using metabolic network topology. Proceedings of the National Academy of Sciences of the United States of America 102(8), 2685.
#'@references Oliveira A., Patil K., and Nielsen J. (2008) Architecture of transcriptional regulatory circuits is knitted over the topology of bio-molecular interaction networks. BMC Systems Biology 2(1), 17.
#'@references Väremo L., Nielsen J., and Nookaew I. (2013) Enriching the gene set analysis of genome-wide data by incorporating directionality of gene expression and combining statistical hypotheses and methods. Nucleic Acids Research, 41(8), pp. 4378-4391.
#'@seealso \code{\link{loadGSC}}, \code{\link{runGSA}}, \code{\link{GSAsummaryTable}}
#'@examples
#'#simnw <- computeSimilarity(c(1110,10413,196,51,311,43,764,790)) #compute similarity network for given pubchem compounds
#'#pval <- data.frame(pubchem=c(1110,10413,196,51,311,43,764,790), stat=runif(8, 0, 0.06)) #statistical values of pubchem compounds
#'#result <- computeNwEnrichment(simnw$edges, simnw$nodes, annotation="mesh", pval, internalid = FALSE)
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
    cat("Computing enrichment ...\n")
    gs = piano::loadGSC(edgelist, type="data.frame")
    gsaRes = piano::runGSA(geneLevelStats=pval, directions=fc, gsc=gs, geneSetStat=method, gsSizeLim=size)
    resTab = piano::GSAsummaryTable(gsaRes)
    cat("Formatting output ...\n")
    resTab$member = lapply(resTab$Name, function(x) names(piano::geneSetSummary(gsaRes, geneSet=x)$geneLevelStats)) #get members
    #format colnames
    colnames(resTab) = gsub("Genes","amount",colnames(resTab))
    colnames(resTab) = gsub("Name","id",colnames(resTab))
    colnames(resTab) = gsub("\\.","_",colnames(resTab))
    colnames(resTab) = gsub("amount \\(tot\\)","no_of_entities",colnames(resTab))
    drops = c("Stat (non-dir_)","Stat (dist_dir_up)","Stat (dist_dir_dn)","Stat (mix_dir_up)","Stat (mix_dir_dn)","amount (down)","amount (up)")
    resTab = resTab[ , !(colnames(resTab) %in% drops)] #hide stat columns
    colnames(resTab) = gsub(" \\(non-dir_\\)","",colnames(resTab))
    colnames(resTab) = gsub("p adj","p_adj",colnames(resTab))
    #resTab = resTab[,c(1,ncol(resTab),2:(ncol(resTab)-1))] #rearrange columns
    cat("Returning enrichment of size ",nrow(resTab)," ...\n")
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
