#'Upload Data
#'@description Upload Data
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

stat_load_data = function(file){ # returns a expression data frame(eData),
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # t test
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\\\two independent group\\mx_274941_Francisco Portell_human cells_06-2016_submit.xlsx"
  # ANOVA
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\one way ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # two way ANOVA 2*2
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\two way ANOVA\\mx 255530 Jan Schilling_Project 1_ mouse serum_04-2016_submit_4.29.2016.xlsx"
  # two way ANOVA 3*4
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\two way ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # paired t test
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\two paired group\\mx_274941_Francisco Portell_human cells_06-2016_submit.xlsx"
  # one way repeated ANOVA
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\one way repeated ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"


  d <- tryCatch(xlsx::read.xlsx2(file, sheetIndex = 1, stringsAsFactors = FALSE,header=F, ...),
                  error = function(e){
                    openxlsx::read.xlsx(file, sheet = 1,colNames = F)
                  })
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
  # remove all the NA. And replace NA with "NA" Otherwise DataTables will give error.datatables warning requested unknown parameter
  eData[is.na(eData)]="NA"
  fData[is.na(fData)]="NA"
  pData[is.na(pData)]="NA"

  for(i in 1:nrow(pData)){
    for(j in 1:ncol(pData)){
      pData[i,j] = gsub("\\+|~|-", " ", pData[i,j])
    }
  }

  result <- list(expression = eData, feature = fData, phenotype = pData)

  # e = e_ori = eData; p = p_ori = pData; f = fData;



  writeLines("sucess!","messages.txt")
  return(result)
}
