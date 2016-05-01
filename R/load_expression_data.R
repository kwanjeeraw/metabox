#'Upload expression data.
#'@description fdas
#' dasfs
#'@usage
#'@param file an object with a child named datapath, which is the direction of the file input
#'@param
#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export

load_expression_data = function(file,...){
  if(grepl("xlsx", file$name)){
    #### currently, data should be at the first sheetIndex. ???
    e <- tryCatch(xlsx::read.xlsx2(file$datapath, sheetIndex = 1, stringsAsFactors = FALSE, ...),
                  error = function(e){
                    openxlsx::read.xlsx(file$datapath, sheet = 1)
                  })
  }
  if(grepl("csv", file$name)){
    e <- read.csv(file$datapath, stringsAsFactors = FALSE, ...)
  }
  e = t(e)
  colnames = e[1,];e = e[-1,];rownames = rownames(e);e = data.frame(e,stringsAsFactors = F);e = sapply(e, as.numeric);e = data.frame(e,stringsAsFactors = F)
  colnames(e) = colnames;e = data.frame(e);rownames(e)=rownames
  return(e)
}
