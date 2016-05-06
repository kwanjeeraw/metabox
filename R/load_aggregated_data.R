#'Upload aggregated data.
#'@description Upload aggregated data.
#' Load xlsx or csv file and return a list of expression, feature and phenotype dataset. The standard of xlsx or csv file can be found \link{here}.!!!
#'@usage load_aggregated_data(file, type, ...)
#'@param file the file in read.csv or read.xlsx2.
#'@param type a string of file name ended either with .xlsx or .csv.
#'@param ... Additional arguments for xlsx::read.xlsx2 or read.csv.
#'@details
#'
#'@return a list of three data frames: "expression"(sample in row), "feature"(compoud in row) and "phenotype"(sample in row).
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso \code{\link{load_expression_data}}, \code{\link{load_expression_data}}, \code{\link{load_expression_data}}
#'@examples
#'load_aggregated_data(input$inputID,startRow=2)
#'@export

load_aggregated_data = function(file, type,...){ # returns a expression data frame(eData),
  # feature information data.frame(fData),
  # phenotype of samples data.frame(pData);
  ### file should be: input$inputID. fileInput(inputID, ...)
  ### DISPLAY: input$inputID$name.

  ### distinguish the type of data. Usually the data is xlsx. So currently, only working on xlsx but not xls. ???
  if(type){
    #### currently, data should be at the first sheetIndex. ???
    d <- tryCatch(xlsx::read.xlsx2(file, sheetIndex = 1, stringsAsFactors = FALSE, ...),
                  error = function(e){
                    openxlsx::read.xlsx(file, sheet = 1)
                  })
  }else{
    d <- read.csv(file, stringsAsFactors = FALSE, ...)
  }
  d[d==""] <- NA
  #### fData
  fData <- d[!is.na(d[,1]),c(which(is.na(d[1,])),sum(is.na(d[1,]))+1)] # The first row and column is critical of formating the data.
  colnames(fData) = fData[1,]; fData = data.frame(fData[-1,],stringsAsFactors = F);rownames(fData) = 1:nrow(fData);
  #### pData
  pData <- d[is.na(d[,1]),!is.na(d[1,])]
  pData <- t(pData); colnames(pData) = pData[1,]; pData = data.frame(pData[-1,],stringsAsFactors = F)
  #### eData
  eData <- d[!is.na(d[,1]),!is.na(d[1,])][-1,-1]
  eData <- sapply(eData, as.numeric)
  colnames(eData) = rownames(pData); rownames(eData) = fData[,1]
  eData <- data.frame(t(eData),stringsAsFactors = F)

  # remove any unwanted character in columns of eData, fData and pData to _.
  colnames(eData) = gsub("([_])|[[:punct:]]", "_", colnames(eData))
  colnames(fData) = gsub("([_])|[[:punct:]]", "_", colnames(fData))
  colnames(pData) = gsub("([_])|[[:punct:]]", "_", colnames(pData))

  result <- list(expression = eData, feature = fData, phenotype = pData)

  writeLines("sucess!","messages.txt")
  return(result)
}
