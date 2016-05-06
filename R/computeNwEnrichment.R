#'Perform enrichment analysis on the input network
#'@description perform enrichment analysis on the input network by integrating the statistical values of molecular entities e.g. compound, protein or gene.
#'The input network is generated from any function such as \code{\link{computeSimilarity}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSubnetwork}},
#'\code{\link{fetchHetNetwork}} and \code{\link{fetchNetwork}}. The function wraps around \code{\link{computeEnrichment}} for enrichment analysis.
#'@usage computeNwEnrichment(edgelist, nodelist, annotation, pval, fc, internalid, method, size, returnas)
#'@param edgelist a data frame of edges contains at least source and target pairs
#'@param nodelist a data frame of nodes contains node information e.g. node name, node xref.
#'The first column is id. See \code{\link{fetchNetwork}} for the data frame structure.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh.
#'@param pval a numeric vector containing the p-values of molecular entities, computed from statistical analysis,
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param fc a numeric vector containing fold changes or sign information (positive or negative) of molecular entities
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'@param method a string specifying the enrichment analysis method. It can be one of reporter (default), fisher, median, mean, stouffer. See \code{\link{runGSA}}
#'@param size a numeric vector specifying the minimum and maximum number of members in each entity set collection to be used in the analysis. Default is c(3,500).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges and enrichment analysis result. The list contains the data frame of nodes, the data frame of edges and
#'the data frame of enrichment analysis result with the following components:
#'
#'\code{id} = internal neo4j id
#'
#'\code{gid} = grinn id
#'
#'\code{nodename} = name of entity set
#'
#'\code{nodelabel} = annotation type
#'
#'\code{nodexref} = cross references
#'
#'\code{amount} = number of members in each entity set
#'
#'\code{p} = entity set p-values
#'
#'\code{p adj} = adjusted entity set p-values
#'
#'\code{member} = list of entities
#'See \code{\link{runGSA}} for more details about directional classes. Return empty list if error or found nothing.
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
computeNwEnrichment <- function(edgelist, nodelist, annotation="pathway", pval, fc=NULL, internalid = TRUE, method="reporter", size=c(3,500), returnas="dataframe") UseMethod("computeNwEnrichment")
#'@export
computeNwEnrichment.default <- function (edgelist, nodelist, annotation="pathway", pval, fc=NULL, internalid = TRUE, method="reporter", size=c(3,500), returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(annotation <- match.arg(tolower(annotation), c("pathway","mesh"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'annotation' is not valid, choose one from the list: pathway,mesh")
      }
      tmparg <- try(method <- match.arg(tolower(method), c("reporter","fisher","median","mean","stouffer"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'method' is not valid, choose one from the list: reporter,fisher,median,mean,stouffer")
      }
      if (class(pval) == "data.frame") {#format pval dataframe input: 1st id, 2nd pval, 3rd fc (can be NULL)
        if(internalid){#use internal ids as attribute names
          #pval
          pv = pval[,2]
          names(pv) = pval[,1]
#           if(ncol(pval)>2){#format fc
#             fcv = pval[,3]
#             names(fcv) = pval[,1]
#             fc = fcv
#           }
          pval = pv
        }else{#format attribute names from gid to id
          merged = merge(nodelist, pval, by.x = colnames(nodelist)[2], by.y = colnames(pval)[1], all.y = TRUE)
          pv = merged[,ncol(nodelist)+1]
          names(pv) = merged$id
#           if(ncol(pval)>2){#format fc
#             fcv = merged[,ncol(nodelist)+2]
#             names(fcv) = merged$id
#             fc = fcv
#           }
          pval = pv
        }
      }
      if(tolower(annotation) == 'pathway'){#pathway enrichment
        cat("Querying database ...\n")
        if(internalid){
          annols = apply(nodelist, 1, function(x) fetchNetwork(to=x["id"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = apply(nodelist, 1, function(x) fetchNetworkByGID(to=x["gid"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){
          annonws = combineNetworks(annols) #combine annotation pairs
          era = computeEnrichment(edgelist = annonws$edges[,2:1], pval = pval, fc = fc, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-1))] #rearrange columns
        }
        else{
          nodelist = data.frame()
          edgelist = data.frame()
          era = data.frame()
        }
      }else if(tolower(annotation) == 'mesh'){#mesh enrichment
        cat("Connecting PubChem ...\n")
        annols = apply(nodelist, 1, function(x) callMesh(pcid=x["gid"])) #query pubchem annotation pairs
        if(!is.null(unlist(annols))){
          annonws = combineNetworks(annols) #combine annotation pairs
          annopair = merge(annonws$edges,nodelist, by.x='target', by.y='gid')[,2:3] #change gid to id
          colnames(annopair) = c('source','target')
          era = computeEnrichment(edgelist = annopair[,2:1], pval = pval, fc = fc, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-1))] #rearrange columns
        }
        else{
          nodelist = data.frame()
          edgelist = data.frame()
          era = data.frame()
        }
      }else{
        stop('Unknown annotation')
      }
      list(nodes=nodelist, edges=edgelist, enrichment=era)
    },
    error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame())
    })
  return(out)
}
