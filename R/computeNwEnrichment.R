#'Perform enrichment analysis on the input network
#'@description perform enrichment analysis on the input network by integrating the statistical values of molecular entities e.g. compound, protein or gene.
#'The input network is generated from any function such as \code{\link{computeSimilarity}}, \code{\link{computeCorrelation}}, \code{\link{computeParCorrelation}}, \code{\link{computeSubnetwork}},
#'\code{\link{fetchHetNetwork}} and \code{\link{fetchNetwork}}. The function wraps around \code{\link{computeEnrichment}} for enrichment analysis.
#'@usage computeNwEnrichment(edgelist, nodelist, annotation, pval, fc, internalid, method, size, returnas)
#'@param edgelist a data frame of edges contains at least source and target pairs
#'@param nodelist a data frame of nodes contains node information e.g. node name, node xref.
#'The first column is id. See \code{\link{fetchNetwork}} for the data frame structure.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh. Mesh annotation is available for PubChem compounds only.
#'@param pval a numeric vector containing the p-values of molecular entities, computed from statistical analysis,
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param fc a numeric vector containing fold changes or sign information (positive or negative) of molecular entities
#'with name attributes corresponding to the entities. See \code{\link{runGSA}} for details.
#'@param internalid boolean value, whether name attributes of pval are neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'\code{internalid} has no effect on Mesh enrichment.
#'@param method a string specifying the enrichment analysis method. It can be one of reporter (default), fisher, median, mean, stouffer. See \code{\link{runGSA}}
#'@param size a numeric vector specifying the minimum and maximum number of members in each entity set collection to be used in the analysis. Default is c(3,500).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges, enrichment analysis result and annotation pairs. The list contains the data frame of nodes, the data frame of edges and
#'the data frame of enrichment analysis result with the following components:
#'
#'\code{rank} = rank sort by p adj
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
#'\code{p} = entity set p-values
#'
#'\code{p adj} = adjusted entity set p-values
#'
#'\code{no_of_entities} = number of input entities in each entity set
#'
#'\code{total} = total number of entities in each entity set from the database, not available for Mesh
#'
#'\code{member} = list of entity members of the entity set
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
      require('dplyr')
      if (class(pval) == "data.frame") {#format pval dataframe input: 1st id, 2nd pval, 3rd fc (can be NULL)
        if(internalid){#format pval: 1st id, 2nd pval, ... ,use only 1st and 2nd column
          pv = pval[,2]
          names(pv) = pval[,1]
          pval = pv
        }else{#format pval: 1st gid, 2nd pval, ... ,use only 1st and 2nd column, change gid to id
          merged = merge(nodelist, pval, by.x = colnames(nodelist)[2], by.y = colnames(pval)[1], all.y = TRUE)
          pv = merged[,ncol(nodelist)+1]
          names(pv) = merged$id
          pval = pv
        }
      }
      if(tolower(annotation) == 'pathway' && foundDb()){#pathway enrichment
        cat("Querying database ...\n")
        if(internalid){
          annols = apply(nodelist, 1, function(x) fetchNetwork(to=x["id"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = apply(nodelist, 1, function(x) fetchNetworkByGID(to=x["gid"], fromtype="pathway", totype = x["nodelabel"], reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          era = computeEnrichment(edgelist = annonws$edges[,2:1], pval = pval, fc = fc, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era = era[ , !(colnames(era) == 'no_of_entities')] #hide sum columns
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-1))] #rearrange columns
          #count subtotal entities
          tatt = dplyr::left_join(annonws$edges[,1:2],annonws$nodes,by=c('target'='id')) #get target attributes
          subtotls = tatt %>% dplyr::group_by(source,nodelabel) %>% dplyr::tally() #count entities
          subtot = plyr::ddply(subtotls,c('source'),plyr::summarise,no_of_entities=list(paste0(nodelabel,' (',n,')')))
          era = dplyr::left_join(era, subtot, by=c('id'='source'))
          #count total entities
          ptwls = unique(era$id) #list of pathways
          ntypels = unique(annonws$nodes$nodelabel)
          ntypels = ntypels[ntypels!="Pathway"] #list of nodetype
          cat("Querying pathway statistics ...\n")
          ptwstat = lapply(ptwls, function(x) {
            lapply(ntypels, function(y) {
              qstring = paste0('MATCH (from:Pathway)-[r:ANNOTATION]->(to:',y,') where ID(from) = ',x,' RETURN toString(ID(from)), labels(to), count(to)')
              tmp = as.data.frame(curlRequest(qstring), stringsAsFactors = FALSE)
              colnames(tmp) = c('id','nodelabel','count')
              tmp
            })
          })
          ptwstat = plyr::ldply (unlist(ptwstat, recursive = F), data.frame) #get total number of annotated nodes of a pathway
          ptwstat = ptwstat[order(ptwstat$nodelabel),]
          tot = plyr::ddply(ptwstat,c('id'),plyr::summarise,total=list(paste0(nodelabel,' (',count,')')))
          era = dplyr::left_join(era,tot,by=c('id'='id'))
          era = era[c(1:(ncol(era)-3),(ncol(era)-1),ncol(era),(ncol(era)-2))] #rearrange columns
          list(nodes=nodelist, edges=edgelist, enrichment=era, pairs=annonws$edges) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame(), pairs=data.frame()) #output
        }
      }else if(tolower(annotation) == 'mesh'){#mesh enrichment
        cat("Connecting PubChem ...\n")
        annols = apply(nodelist, 1, function(x) callMesh(pcid=x["gid"])) #query pubchem annotation pairs
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          #format edge, change gid to id, fix edge row order
          annopair = dplyr::right_join(nodelist[,1:2],annonws$edges[,1:2],by=c('gid' = 'target'))[,c(3,1)]
          colnames(annopair) = c('source','target')
          era = computeEnrichment(edgelist = annopair[,2:1], pval = pval, fc = fc, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-5),(ncol(era)-3),(ncol(era)-2),(ncol(era)-4),(ncol(era)-1))] #rearrange columns
          list(nodes=nodelist, edges=edgelist, enrichment=era, pairs=annopair) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame(), pairs=data.frame()) #output
        }
      }else{
        cat('Error: No database installed, returning no data ..\n')
        list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame(), pairs=data.frame()) #output
      }
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame(), pairs=data.frame()) #output
    })
  return(out)
}
