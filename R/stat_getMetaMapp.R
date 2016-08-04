#'getMetaMapp
#'@description getMetaMapp
#'
#'@usage
#'@param norm.

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'





  getChemSimNet <- function (cids, cutoff=0.7) {
    library(RCurl)
    #cids <- c(1:5)
    #cutoff <- 0.7
    if (length(cids)>200) {
      hex <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")
      bin <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
      cidlist <- split(cids, ceiling(seq_along(cids)/200))
      subkeys <- do.call(rbind,lapply(cidlist,function(x) { read.csv(paste(c('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',paste(x,collapse=","),'/property/Fingerprint2D/csv'),collapse=""))}))
      m <- t(sapply(subkeys[,2],function(x) { as.integer(strsplit(paste(sapply(strsplit(paste(base64Decode(x,"raw")[5:115],collapse=""),"")[[1]],function(x) {bin[which(hex==x)]}),collapse=""),"")[[1]][1:881] ) }))
      mat <- m%*%t(m)
      len <- length(m[,1])
      s <- mat.or.vec(len,len)
      for (i in 1:len) {
        for (j in 1:len){
          s[i,j] <- mat[i,j]/(mat[i,i]+mat[j,j]-mat[i,j])
        }
      }
      diag(s) <- 0
      dfmax <- cbind(cids,"tmsim",cids[sapply(1:length(cids), function (k) {which.max(s[k,]) })])
      s[lower.tri(s)]<-0
      chemsimdf <- do.call(rbind,sapply(1:length(cids), function (k) { if(length(which(s[k,]>cutoff))>0) {cbind(cids[k],"tmsim",cids[which(s[k,]>cutoff)])}} ))
      chemsimdf <- rbind(chemsimdf, cbind(cids,"tmsim",""), dfmax )
      write.table(chemsimdf,file=paste(c("chemsim_",gsub("[.]","",as.character(cutoff)),".sif"),collapse=""), quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)  ## To write the cytoscape network file as an output
      return(chemsimdf)
    } else{

      hex <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")
      bin <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
      subkeys <- read.csv(paste(c('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',paste(cids,collapse=","),'/property/Fingerprint2D/csv'),collapse=""))
      m <- t(sapply(subkeys[,2],function(x) { as.integer(strsplit(paste(sapply(strsplit(paste(base64Decode(x,"raw")[5:115],collapse=""),"")[[1]],function(x) {bin[which(hex==x)]}),collapse=""),"")[[1]][1:881] ) }))
      mat <- m%*%t(m)
      len <- length(m[,1])
      s <- mat.or.vec(len,len)
      for (i in 1:len) {
        for (j in 1:len){
          s[i,j] <- mat[i,j]/(mat[i,i]+mat[j,j]-mat[i,j])
        }
      }
      diag(s) <- 0
      dfmax <- cbind(cids,"tmsim",cids[sapply(1:length(cids), function (k) {which.max(s[k,]) })])
      s[lower.tri(s)]<-0
      chemsimdf <- do.call(rbind,sapply(1:length(cids), function (k) { if(length(which(s[k,]>cutoff))>0) {cbind(cids[k],"tmsim",cids[which(s[k,]>cutoff)])}} ))
      chemsimdf <- rbind(chemsimdf, cbind(cids,"tmsim",""), dfmax )
      write.table(chemsimdf,file=paste(c("chemsim_",gsub("[.]","",as.character(cutoff)),".sif"),collapse=""), quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)  ## To write the cytoscape network file as an output
      return(chemsimdf)
    }
  }
  ## Tanimoto score calculation was adopted from http://data2quest.blogspot.com/2013/10/fast-tanimoto-similarity-calculation.html

  # krp <- read.table("KRPlinks.txt", sep="\t")

  getKEGGRpairs <- function (cids, keggids, cutoff=0.7) {
    krp.1 <- match(krp[,1],keggids)
    krp.2 <- match(krp[,2],keggids)
    krp.cbind <- cbind (krp.1,krp.2)
    krp.net <- subset(krp.cbind, krp.1!="NA" & krp.2!="NA")
    cid.krp.2 <- cids[krp.net[,2]]
    cid.krp.1 <- cids[krp.net[,1]]
    krp.cid.net <- cbind(cid.krp.1,"krp",cid.krp.2)
    chemsim <- getChemSimNet(cids,cutoff)
    krp.cid.net <- rbind(krp.cid.net,chemsim)
    write.table(krp.cid.net,file=paste(c("chemsim_krp_",gsub("[.]","",as.character(cutoff)),".sif"),collapse=""), quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)  ## To write the cytoscape network file as an output
  }



  ##http://128.120.143.234:1234/ocpu/tmp/x068bb10d58/
  getMetaMapp <- function (rsess="http://128.120.143.234:1234/ocpu/tmp/x068bb10d58/", pvalind=1, cutoff=0.7) {
    library(RCurl)
    library(RJSONIO)
    statres <- paste(rsess,"R/.val/csv",sep="")
    df <- read.csv(statres)
    getKEGGRpairs(df$PubChem[is.na(df$PubChem)==FALSE], as.character(df$KEGG[is.na(df$PubChem)==FALSE]), cutoff)
    fcdf <- df[which(is.na(df$PubChem)==FALSE),grep("^Mean",colnames(df))]
    fcdf$meanratio <- fcdf$Mean.of.t1...None / fcdf$Mean.of.t2...Test.Compound
    pvaldf <- df[which(is.na(df$PubChem)==FALSE),grep("p_value",colnames(df))]
    exportdf <- data.frame(Pubchem=df$PubChem[is.na(df$PubChem)==FALSE])
    exportdf$KEGG <- as.character(df$KEGG[is.na(df$PubChem)==FALSE])
    exportdf$fold.change <- rep(1,length(exportdf[,1]))
    exportdf$pvaldirection <- rep("No Change",length(exportdf[,1]))
    exportdf <- cbind(exportdf,df[which(is.na(df$PubChem)==FALSE),grep("p_value",colnames(df))])
    exportdf$CpdName <- as.character(df$BinBase_name[is.na(df$PubChem)==FALSE])
    sigind <- which(exportdf[,grep("p_value",colnames(exportdf))[1]]<0.05)
    for( x in sigind)  {
      if(fcdf$meanratio[x]<1) {
        exportdf$fold.change[x] <- round(1/fcdf$meanratio[x],1)
        exportdf$pvaldirection[x] <- "Down"
      } else {
        exportdf$fold.change[x] <- round(fcdf$meanratio[x],1)
        exportdf$pvaldirection[x] <- "Up"
      }
    }
    #exportdf$meshanno <- sapply(exportdf$Pubchem, function (x) { paste(fromJSON( paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pccompound&db=mesh&id=",x,"&retmode=json",sep="") )$linksets[[1]]$linksetdbs[[1]]$links,collapse=",")  }   )
    write.table( exportdf, file=paste("node_attributes_chemsim_krp_",gsub("[.]","",as.character(cutoff)) ,".tsv", sep="" ), col.names = T, row.names = F, quote = F, sep = "\t" )
  }




