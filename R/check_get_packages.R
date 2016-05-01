#'Check libraries needed.
#'@description Check and download libraries needed.
#' Check if library already downloaded then either load it or download it and then load it.
#'@usage
#'@param pkg a vector containing the names(string) of packages needed.
#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
check_get_packages = function(pkg){
  options(warn=-1)

  res<-character()

  need<-as.matrix(sapply(1:length(pkg),function(i)
  {

    if(require(pkg[i],character.only = TRUE)==FALSE)
    {
      res<-c(res,pkg[i])
    }
  }))

  need<-as.character(unlist(need[!is.null(need)]))
  if(length(need)>0)
  {

    x<-sapply(need,install.packages,dependencies = TRUE)
    lib.fun<-function(need){
      sapply(1:length(need), function(i){
        out<-tryCatch(library(need[i], character.only= TRUE), error=function(e){need[i]})
        if(all(out==need[i])){need[i]}
      })
    }

    out<-as.character(unlist(lib.fun(need)))

    #try bioconductor
    if(length(out)>0){
      cat(paste("Package not found, trying Bioconductor..."),"\n")
      source("http://bioconductor.org/biocLite.R")
      lib.fun.bioc<-function(need){
        sapply(1:length(need), function(i){
          tryCatch( biocLite(need[i],ask=FAlSE),
                    error=function(e){need[i]})
        })
      }

      tmp<-lib.fun.bioc(out)
      final<-lib.fun(tmp)
      if(length(final)>0){cat(paste("could not find package: ",paste(as.character(unlist(final)),collapse=", "),sep=""),"\n")}
    }

  }

}
