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

stat_load_data = function(file,sheetIndex = NULL, from_example=NULL){ # returns a expression data frame(eData),
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # t test
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\two independent group\\mx_274941_Francisco Portell_human cells_06-2016_submit.xlsx"
  # ANOVA
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\one way ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # two way ANOVA 2*2
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\\\data\\two way ANOVA\\mx 255530 Jan Schilling_Project 1_ mouse serum_04-2016_submit_4.29.2016.xlsx"
  # two way ANOVA 3*4
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\two way ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # paired t test
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\two paired group\\mx_274941_Francisco Portell_human cells_06-2016_submit.xlsx"
  # one way repeated ANOVA
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\one way repeated ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # two way repeated ANOVA 2*2
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\two way repeated ANOVA\\mx 255530 Jan Schilling_Project 1_ mouse serum_04-2016_submit_4.29.2016.xlsx"
  # two way repeated ANOVA 3*4
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\two way repeated ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"

  # mixed ANOVA 2*2
  # file = "C:\\Users\\fansi\\Desktop\\MetaBoxDiv2\\data\\mixed ANOVA\\mx 255530 Jan Schilling_Project 1_ mouse serum_04-2016_submit_4.29.2016.xlsx"
  # mixed ANOVA 3*4
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\mixed ANOVA\\mx 69088_HepG2 cells_Hirahatake & Meissen_high fructose_summer course_08-2015_submit.xlsx"



  # metabolomics data for manuscript
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\manuscript\\mx 107155 _study 112912 TRDRP LC7 NYU Lung Tissue Miyamoto 10113.xlsx"
  # gene data for manuscript
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\manuscript\\GeneExpression_GSE32863.xlsx"


  # loess norm
  # file = "C:\\Users\\Sili Fan\\Documents\\GitHub\\MetaBoxDiv2\\data\\loessnorm.xlsx"

  # temp
  # file = "C:\\Users\\Sili Fan\\Desktop\\test.xlsx"

  if(is.null(from_example)){
    if(length(sheetIndex)==0 | sheetIndex==""){
      sheetIndex = 1
    }else{
      sheetIndex=as.numeric(sheetIndex)
    }

    if(grepl("xlsx", file)){
      d <- tryCatch(xlsx::read.xlsx2(file, sheetIndex = sheetIndex, stringsAsFactors = FALSE,header=F, ...),
                    error = function(e){
                      openxlsx::read.xlsx(file, sheet = sheetIndex,colNames = F)
                    })
    }else if(grepl("csv", file)){
      # file = "C:\\Users\\fansi\\Downloads\\val (18).csv"
      d <- read.csv(file,header = T,stringsAsFactors = FALSE)
    }


    d[d==""] <- NA
    #### fData
    fData <- d[!is.na(d[,1]),c(which(is.na(d[1,])),sum(is.na(d[1,]))+1)] # The first row and column is critical of formating the data.
    colnames(fData) = as.character(fData[1,]); fData = data.frame(fData[-1,],stringsAsFactors = F);rownames(fData) = 1:nrow(fData);
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

    if(sum(!c("phenotype_index","sampleID","feature_index","KnownorUnknown")%in%c(colnames(pData),colnames(fData)))>0){
      message = paste0("The data uploaded doesn't have ",
                       paste(c("phenotype_index","sampleID","feature_index","KnownorUnknown")[!
                                                                                                c("phenotype_index","sampleID","feature_index","KnownorUnknown")%in%c(colnames(pData),colnames(fData))],collapse = ", ")," and they are added automatically. You can examine them by ")

    }else{
      message = NULL
    }

    if(!"phenotype_index"%in%colnames(pData)){
      pData$phenotype_index = 1:nrow(pData)
    }
    if(!"sampleID"%in%colnames(pData)){
      pData$sampleID = 1:nrow(pData)
    }
    if(!"feature_index"%in%colnames(fData)){
      fData$feature_index = 1:nrow(fData)
    }
    if(!"KnownorUnknown"%in%colnames(fData)){
      fData$KnownorUnknown = rep(T,nrow(fData))
    }

    # check format.
    #1. cannot have missing value.
    num_of_missing = vector()
    for(i in 1:ncol(eData)){
      num_of_missing[i] = sum(is.na(eData[,i]))
    }
    if(sum(num_of_missing>0)){
      message = paste(message,  fData$feature_index[num_of_missing>0], "th feature contains missing value and they have been removed!")
    }
    eData = eData[, num_of_missing == 0]
    fData = fData[num_of_missing == 0, ]

    #2. cannot have constant value
    constant_feature = sapply(eData, sd) == 0
    if(sum(constant_feature)>0){
      message  = paste(message,  fData$feature_index[constant_feature], "th feature is constant and they have been removed!")
    }
    eData = eData[, !constant_feature]
    fData = fData[!constant_feature, ]

    if(is.null(message)){
      message = "Success!"
      writeLines(message,"messages.txt")
    }else{
      writeLines(message,"messages.txt")
    }
    if(sum(duplicated(pData$sampleID[pData$sampleID>0]))){
      duplicatedID = T
    }else{
      duplicatedID = F
    }
    result <- list(expression = eData, feature = fData, phenotype = pData, duplicatedID=duplicatedID )
    # e = e_ori = eData; p = p_ori = pData; f = fData;



    return(result)
  }else{
    if(from_example=="Two Independent Group Data"){
      writeLines("Success!","messages.txt", sep = "")
    return(t_test_example)
    }else if(from_example=="Multi-Independent Group Data"){
      writeLines("Success!","messages.txt", sep = "")
    return(ANOVA_example)
    }else if(from_example=="3*4-Independent Group Data"){
      writeLines("Success!","messages.txt", sep = "")
    return(two_way_ANOVA_3_4_example)
    }else if(from_example=="2*2-Independent Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(two_way_ANOVA_2_2_example)
    }else if(from_example=="Two Paired Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(paired_t_test_example)
    }else if(from_example=="Multi-Paired Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(paired_ANOVA_example)
    }else if(from_example=="3*4-Paired Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(two_way_repeated_ANOVA_3_4_example)
    }else if(from_example=="2*2-Paired Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(two_way_repeated_ANOVA_2_2_example)
    }else if(from_example=="3*4-Mixed Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(mixed_two_way_ANOVA_3_4_example)
    }else if(from_example=="2*2-Mixed Group Data"){
      writeLines("Success!","messages.txt", sep = "")
      return(mixed_two_way_ANOVA_2_2_example)
    }
  }

}
