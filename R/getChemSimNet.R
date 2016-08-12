#compute similarity network, D. Barupal
getChemSimNet <- function (cids, cutoff=0.7) {
#   #cids <- c(1:5)
#   #cutoff <- 0.7
#   library(RCurl)
#   hex <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")
#   bin <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
#   subkeys <- read.csv(paste(c('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/',paste(cids,collapse=","),'/property/Fingerprint2D/csv'),collapse=""))
#   m <- t(sapply(subkeys[,2],function(x) { as.integer(strsplit(paste(sapply(strsplit(paste(base64Decode(x,"raw")[5:115],collapse=""),"")[[1]],function(x) {bin[which(hex==x)]}),collapse=""),"")[[1]][1:881] ) }))
#   mat <- m%*%t(m)
#   len <- length(m[,1])
#   s <- mat.or.vec(len,len)
#   for (i in 1:len) {
#     for (j in 1:len){
#       s[i,j] <- mat[i,j]/(mat[i,i]+mat[j,j]-mat[i,j])
#     }
#   }
#   diag(s) <- 0
#   s[lower.tri(s)]<-0
#   return(do.call(rbind,sapply(1:length(cids), function (k) { if(length(which(s[k,]>cutoff))>0) {cbind(cids[k],cids[which(s[k,]>cutoff)],s[k,][which(s[k,]>cutoff)])}} )))
return (data.frame(source = c('1','2','3','4','5'), target = c('11','12','13','14','15'), coef = c(0.1,0.2,0.4,0.5,0.9), stringsAsFactors = FALSE))
}
