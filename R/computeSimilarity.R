#'Compute Tanimoto similarity network
#'@description compute a network for compounds based on chemical structure similarity.
#'The function computes Tanimoto distances between the given compounds using PubChem fingerprints.
#'@usage computeSimilarity(txtinput, coef, returnas)
#'@param txtinput a character vector of PubChem CIDs.
#'@param coef a numeric value specifying the minimum Tanimoto similarity correlation coefficient to be included in the output (from 0 to 1, default is 0.7).
#'@param returnas a string specifying output type. It can be one of dataframe, list, json. Default is dataframe.
#'@return list of network information with the following components:
#'
#'nodes:
#'
#'\code{id} = pubchem CID or node neo4j id
#'
#'\code{gid} = pubchem CID
#'
#'\code{nodename} = pubchem CID or node name
#'
#'\code{nodelabel} = node type
#'
#'edges:
#'
#'\code{source, target} = pubchem CID or node neo4j id
#'
#'\code{coef} = Tanimoto similarity coefficient
#'
#'\code{type} = relationship type
#'
#'Return empty list if error or found nothing.
#'@note If the database is installed, node attributes will be automatically retrieved from the database. Otherwise node attributes will be the original input.
#'@author Kwanjeera W \email{kwanich@@ucdavis.edu}
#'@references Willett P., Barnard JM. and Downs GM. (1998) Chemical similarity searching. J. Chem. Inf. Comput. Sci., 38, 983–996.
#'@references Barupal KD., et al. (2012) MetaMapp: mapping and visualizing metabolomic data by integrating information from biochemical pathways and chemical and mass spectral similarity. BMC Bioinformatics. 13:99.
#'@references Grapov D., Wanichthanarak K. and Fiehn O. (2015) MetaMapR: pathway independent metabolomic network analysis incorporating unknowns. Bioinformatics. 31(16):2757-60.
#'@references Cao Y., Charisi A., Cheng L., Jiang T. and Girke T. (2008) ChemmineR: a compound mining framework for R. Bioinformatics, 24(15), pp. 1733–1734.
#'@references \url{ftp://ftp.ncbi.nih.gov/pubchem/specifications/pubchem_fingerprints.txt}
#'@seealso \pkg{\link{metabomapr}}, \code{\link{CID_tanimoto}}, \code{\link{fpSim}}, \code{\link{sdfid}}
#'@examples
#'#simnw <- computeSimilarity(c(1110,10413,196,51,311,43,764,790)) #compute similarity network for given pubchem compounds
#'@export
computeSimilarity <- function(txtinput, coef=0.7, returnas="dataframe") UseMethod("computeSimilarity")
#'@export
computeSimilarity.default <- function (txtinput, coef=0.7, returnas="dataframe")
{
  out <- tryCatch(
    {
      txtinput = unique(stringr::str_trim(unlist(txtinput))) #remove whiteline, duplicate
      cat("Computing Tanimoto similarity ...\n")
      network = getChemSimNet(txtinput, cutoff = coef)
      cat("Format and returning network of size ",nrow(network)," ...\n")
      if(nrow(network)>0){#pass cutoff
        network = data.frame(source = as.character(network[,1]), target = as.character(network[,2]), coef = as.numeric(network[,3]), stringsAsFactors = FALSE)
        network$type = "TANIMOTO_SIMILARITY"
        cat("Format and returning network nodes ...\n")
        #format nodeList from edgeList
        so = data.frame(id=network$source, gid=network$source, nodename=network$source, nodelabel="Compound", stringsAsFactors = FALSE)
        ta = data.frame(id=network$target, gid=network$target, nodename=network$target, nodelabel="Compound", stringsAsFactors = FALSE)
        sota = unique(rbind(so,ta))
        if(foundDb()){#have db
          nodels = lapply(sota$id, formatNode.LIST, y="compound", z="grinnid") #query nodes by gid
          networknode = plyr::ldply(nodels, data.frame)
          #format edge, change gid to id, fix edge row order
          s = dplyr::right_join(networknode[,1:2],network[,1:2],by=c('gid' = 'source'))
          t = dplyr::right_join(networknode[,1:2],network[,1:2],by=c('gid' = 'target'))
          network$source = s$id
          network$target = t$id
        }else{#no db
          cat("No database installed, returning original input ...\n")
          networknode = sota
        }
        ## output
        switch(returnas,
               dataframe = list(nodes = networknode, edges = network),
               list = list(nodes = split(networknode, seq(nrow(networknode))), edges = split(network, seq(nrow(network)))),
               json = list(nodes = jsonlite::toJSON(networknode), edges = jsonlite::toJSON(network)),
               stop("Error: incorrect 'returnas' type"))
      }else{#not pass cutoff
        cat("Coef is too high, no network returned ...\n")
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
