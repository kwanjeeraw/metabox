#'Perform enrichment analysis on the input entities
#'@description perform enrichment analysis on the list of entities (e.g. compound, protein, gene) by integrating with the statistical values.
#'The function wraps around \code{\link{computeEnrichment}} for enrichment analysis.
#'@usage computeNodeEnrichment(nodedata, nodetype, annotation, internalid, method, size, returnas)
#'@param nodedata a data frame of entities and the statistical values. #'The first column is id and the second is e.g. p-values.
#'@param nodetype a string specifying the type of source nodes. It can be one of compound, protein, gene, rna, dna.
#'@param annotation a string specifying the type of annotations e.g. pathway and mesh. Mesh annotation is available for PubChem compounds only.
#'@param internalid boolean value, whether nodedata use neo4j ids, see \code{\link{convertId}} for how to convert to neo4j ids.
#'\code{internalid} has no effect on Mesh enrichment.
#'@param method a string specifying the enrichment analysis method. It can be one of reporter (default), fisher, median, mean, stouffer. See \code{\link{runGSA}}
#'@param size a numeric vector specifying the minimum and maximum number of members in each entity set collection to be used in the analysis. Default is c(3,500).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of nodes, edges and enrichment analysis result. The list contains the data frame of nodes, the data frame of annotation pairs and
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
#'\code{amount} = number of members in each entity set
#'
#'\code{p} = entity set p-values
#'
#'\code{p adj} = adjusted entity set p-values
#'
#'\code{member} = list of members of the entity set
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
computeNodeEnrichment <- function(nodedata, nodetype="compound", annotation="pathway", internalid = TRUE, method="reporter", size=c(3,500), returnas="dataframe") UseMethod("computeNodeEnrichment")
#'@export
computeNodeEnrichment.default <- function (nodedata, nodetype="compound", annotation="pathway", internalid = TRUE, method="reporter", size=c(3,500), returnas="dataframe"){
  out <- tryCatch(
    {
      tmparg <- try(nodetype <- match.arg(tolower(nodetype), c("compound","protein","gene","rna","dna"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'nodetype' is not valid, choose one from the list: compound,protein,gene,rna,dna")
      }
      tmparg <- try(annotation <- match.arg(tolower(annotation), c("pathway","mesh"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'annotation' is not valid, choose one from the list: pathway,mesh")
      }
      tmparg <- try(method <- match.arg(tolower(method), c("reporter","fisher","median","mean","stouffer"), several.ok = FALSE), silent = TRUE)
      if (class(tmparg) == "try-error") {
        stop("argument 'method' is not valid, choose one from the list: reporter,fisher,median,mean,stouffer")
      }
      if (class(nodedata) != "data.frame"){
        stop("argument 'nodedata' is not valid, data frame is required.")
      }
      if(tolower(annotation) == 'pathway' && foundDb()){#pathway enrichment
        cat("Querying database ...\n")
        if(internalid){
          annols = apply(nodedata, 1, function(x) fetchNetwork(to=x[1], fromtype="pathway", totype = nodetype, reltype = "ANNOTATION")) #query annotation pairs
        }else{
          annols = apply(nodedata, 1, function(x) fetchNetworkByGID(to=x[1], fromtype="pathway", totype = nodetype, reltype = "ANNOTATION")) #query annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          if(internalid){
            #format pval: 1st id, 2nd pval, ... ,use only 1st and 2nd column
            pv = nodedata[,2]
            names(pv) = nodedata[,1]
            pval = pv
          }else{
            #format pval: 1st gid, 2nd pval, ... ,use only 1st and 2nd column, change gid to id
            merged = merge(annonws$nodes, nodedata, by.x = colnames(annonws$nodes)[2], by.y = colnames(nodedata)[1], all.y = TRUE)
            pv = merged[,ncol(annonws$nodes)+1]
            names(pv) = merged$id
            pval = pv
          }
          era = computeEnrichment(edgelist = annonws$edges[,2:1], pval = pval, fc = NULL, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-1))] #rearrange columns
          networknode = annonws$nodes[annonws$nodes$nodelabel != "Pathway", ] #not return pathway nodes
          list(nodes=networknode, edges=annonws$edges, enrichment=era) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame()) #output
        }
      }else if(tolower(annotation) == 'mesh'){#mesh enrichment
        if(internalid || tolower(nodetype) != "compound"){
          cat("Error: Accept only PubChem compounds, returning no data ...\n")
          annols = NULL
        }else{
          cat("Connecting PubChem ...\n")
          annols = apply(nodedata, 1, function(x) callMesh(pcid=x[1])) #query pubchem annotation pairs
        }
        if(!is.null(unlist(annols))){#found annotation
          annonws = combineNetworks(annols) #combine annotation pairs
          if(foundDb()){#have db
            nodels = lapply(nodedata[,1], formatNode.LIST, y="compound", z="grinnid") #query nodes by gid
            networknode = plyr::ldply(nodels, data.frame)
            networknode$id = networknode$gid
          }else{#no db
            cat("No database installed, returning original input ...\n")
            networknode = data.frame(id=nodedata[,1], gid=nodedata[,1], nodename=nodedata[,1], nodelabel="Compound", nodexref='', stringsAsFactors = FALSE)
          }
          #format pval: 1st gid, 2nd pval, ... ,use only 1st and 2nd column
          pv = nodedata[,2]
          names(pv) = nodedata[,1]
          pval = pv
          era = computeEnrichment(edgelist = annonws$edges[,2:1], pval = pval, fc = NULL, method = method, size=size, returnas="dataframe") #compute enrichment
          era = era[order(era$`p adj (non-dir_)`),]
          era$rank = seq(1:nrow(era))
          era = merge(annonws$nodes, era, by='id') #merge annotation attributes and enrichemt results
          era = era[,c(ncol(era),1:(ncol(era)-1))] #rearrange columns
          list(nodes=networknode, edges=annonws$edges, enrichment=era) #output
        }
        else{#no annotation found
          list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame()) #output
        }
      }else{
        cat('Error: No database installed, returning no data ..\n')
        list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame()) #output
      }
    },error=function(e) {
      message(e)
      cat("\nError: RETURN no data ..\n")
      list(nodes=data.frame(), edges=data.frame(), enrichment=data.frame()) #output
    })
  return(out)
}
