#'Study_Design
#'@description Study_Design
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

stat_Study_Design = function(DATA, between_factor = NULL, within_factor = NULL){
  eData = DATA$expression
  fData = DATA$feature
  pData = DATA$phenotype


  if(length(within_factor) == 0){
    within_factor = NULL
  }else if(within_factor==""){
    within_factor=NULL
  }

  if(length(between_factor) == 0){
    between_factor=NULL
  }else if(between_factor==""){
    between_factor=NULL
  }

  Sample_Size = tryCatch(table(pData[,c(between_factor,within_factor)]),
                         error = function(err){
                           "Waiting user to select factor."
                         })


  if(length(between_factor)==0&length(within_factor)==0){
    stat_method = "No Method Selected"
  }else{




    factor_name = c(between_factor,within_factor)
    factor_name = factor_name[!factor_name == ""]
    dta = data.frame(value = eData[,1], pData[factor_name])
    if(length(factor_name[!factor_name%in%within_factor])==1 & length(within_factor)==0 & (length(unique(dta[,2]))>2)){# one way ANOVA
      stat_method = "one way ANOVA"
    }else if(length(factor_name[!factor_name%in%within_factor])==1 & length(within_factor)==0 & (length(unique(dta[,2]))==2)){# t test
      stat_method = "independent t test"
    }else if(length(factor_name[!factor_name%in%within_factor])==2 & length(within_factor)==0){# two way ANOVA

      if(length(unique(dta[,2]))>2 & length(unique(dta[,3]))>2){
        stat_method = "two way ANOVA33"
      }
      if(length(unique(dta[,2]))==2 & length(unique(dta[,3]))>2){
        stat_method = "two way ANOVA23"
      }
      if(length(unique(dta[,2]))>2 & length(unique(dta[,3]))==2){
        stat_method = "two way ANOVA32"
      }
      if(length(unique(dta[,2]))==2 & length(unique(dta[,3]))==2){
        stat_method = "two way ANOVA22"
      }


    }else if(length(factor_name[!factor_name%in%within_factor])==0 & length(within_factor)==1 & (length(unique(dta[,2]))>2)){# one way repeated ANOVA
      stat_method = "one way repeated ANOVA"
    }else if(length(factor_name[!factor_name%in%within_factor])==0 & length(within_factor)==1 & (length(unique(dta[,2]))==2)){# paired t test
      stat_method = "paired t test"
    }else if(length(factor_name[!factor_name%in%within_factor])==0 & length(within_factor)==2){# two way repeated anova.
      stat_method = "two way repeated anova"
    }else if(length(factor_name[!factor_name%in%within_factor])==1 & length(within_factor)==1){# mixed two way anova
      stat_method = "mixed two way anova"
    }else{
      stat_method = "Your Design is so complicated that we couldn't analysis. If you have any question, please contact Sili: slfan@ucdavis.edu"
    }
  }








  writeLines(stat_method,"stat_method.txt")

  return(Sample_Size)
}

