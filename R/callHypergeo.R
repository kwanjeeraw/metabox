#overrepresentation analysis of pathways, one nodetype
callHypergeo.pathway <- function(edgelist, nodelist, member, nodetype, inputsize){
  qstring = paste0('MATCH (:Pathway)-[r:ANNOTATION]->(n:',nodetype,') RETURN count(DISTINCT n)')#get db stat
  totEnt = as.data.frame(curlRequest.TRANSACTION.row(qstring), stringsAsFactors = FALSE)$row #total no. of entities in db
  resDF = foreach(i=1:nrow(edgelist), .combine=rbind) %dopar% {#overrepresentation analysis
    qstring = paste0('MATCH (from:Pathway)-[r:ANNOTATION]->(to:',nodetype,') where ID(from) = ',edgelist$id[i],' RETURN toString(ID(from)), labels(to), count(to)')
    annosize = as.data.frame(curlRequest.TRANSACTION.row(qstring), stringsAsFactors = FALSE) #get annotation info from db
    colnames(annosize) = c('id','nodelabel','count')
    blackAnno = totEnt - annosize$count #no. of entities not in the annotation term
    pval = phyper(edgelist$count[i]-1, annosize$count, blackAnno, inputsize, lower.tail = F) #hypergeometric test
    hyp = data.frame(id=as.character(edgelist$id[i]), p=pval, no_of_entities=edgelist$count[i],
                     annotation_size=annosize$count, background_size=totEnt, stringsAsFactors = FALSE)
  }
  resDF$p_adj = p.adjust(resDF$p, method = "BH") #adjust raw p-values
  resDF = resDF[order(resDF$p_adj),]
  resDF = resDF[,c(1:2,ncol(resDF),3:(ncol(resDF)-1))] #rearrange columns
  resDF$rank = seq(1:nrow(resDF))
  resDF = merge(nodelist, resDF, by='id') #merge annotation attributes and results
  resDF = resDF[,c(ncol(resDF),1:(ncol(resDF)-1))] #rearrange columns
  resDF = dplyr::left_join(resDF, member, by=c('id'='source'))
  resDF
}
#overrepresentation analysis of mesh terms
callHypergeo.mesh <- function(edgelist, nodelist, size, inputsize){
  annomem = plyr::ddply(edgelist,c('source'),plyr::summarise,member=list(target)) #get members
  subanno = edgelist %>% dplyr::group_by(source) %>% dplyr::tally() #count entities
  colnames(subanno) = c('id','count')
  subanno = subanno[subanno$count >= size,] #filter by annotation size
  subanno = dplyr::left_join(subanno, nodelist, by=c('id'='id')) #combine node attributes
  cat("Performing overrepresentation analysis ...\n")
  if(nrow(subanno) > 0){
    totEnt = nodelist$background_size[1] #total no. of entities
    resDF = foreach(i=1:nrow(subanno), .combine=rbind) %dopar% {#overrepresentation analysis
      blackAnno = totEnt - subanno$annotation_size[i] #no. of entities not in the annotation term
      pval = phyper(subanno$count[i]-1, subanno$annotation_size[i], blackAnno, inputsize, lower.tail = F) #hypergeometric test
      hyp = data.frame(id=as.character(subanno$id[i]), p=pval, no_of_entities=subanno$count[i],
                       annotation_size=subanno$annotation_size[i], background_size=totEnt, stringsAsFactors = FALSE)
    }
    resDF$p_adj = p.adjust(resDF$p, method = "BH") #adjust raw p-values
    resDF = resDF[order(resDF$p_adj),]
    resDF = resDF[,c(1:2,ncol(resDF),3:(ncol(resDF)-1))] #rearrange columns
    resDF$rank = seq(1:nrow(resDF))
    resDF = merge(nodelist[,1:5], resDF, by='id') #merge annotation attributes and results, exclude some mesh info
    resDF = resDF[,c(ncol(resDF),1:(ncol(resDF)-1))] #rearrange columns
    resDF = dplyr::left_join(resDF, annomem, by=c('id'='source'))
    resDF
  }else{#less than minSize
    cat("No. of members is too small, returning no data ...\n")
    data.frame()
  }
}
