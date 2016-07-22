#'stat_guess_factor
#'@description stat_guess_factor
#'
#'@usage
#'@param stat_guess_factor

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'
stat_guess_factor = function(DATA){
  library(stringr)
  pData = DATA$phenotype

  table_p = table(pData$sampleID)

  if(max(table_p)==1){ # there is no repeated measure here.

    independent_factor_name = colnames(pData)[!(sapply(sapply(pData,table),length)==1 | sapply(sapply(pData,table),length)==nrow(pData))]

    independent_factor_name = independent_factor_name[!independent_factor_name%in%c('phenotype_index','sampleID','batch','Sample_specific_weight','QC','tim_of_injection','rank')]



    repeated_factor_name = ""

    }else{ # It means that there are repeated factors.
    temp = sapply(pData,function(x){
      sapply(unique(pData$sampleID),function(id){
        sum(by(pData$sampleID,x,function(y){id%in%y}))
      })
    })

    temp.df = data.frame(Var1 = rownames(temp), temp)
    table_p.df = data.frame(table_p)
    merged = merge(temp.df,table_p.df,by="Var1")

    repeated_factor_name = intersect(colnames(merged)[-length(merged)][apply(sapply(merged[,-length(merged)],function(x){
      as.numeric(x) - merged$Freq
    }),2,sum)==0]
    ,
    colnames(pData)[sapply(pData,function(x){
      length(unique(x))
    }) == max(table_p)]
    )

    repeated_factor_name = repeated_factor_name[!repeated_factor_name%in%c('phenotype_index','sampleID','batch','Sample_specific_weight','QC','tim_of_injection','rank')]

    if(length(repeated_factor_name)==0){
      repeated_factor_name = ""
      independent_factor_name = ""
    }else{
      independent_factor_name = colnames(pData)[apply(sapply(pData,function(x){
        by(x,pData[,repeated_factor_name[1]],function(y){
          length(unique(y))
        })
      }),2,function(i){
        length(unique(i))
      }) == 1 &
        (!sapply(pData,function(x){
          by(x,pData[,repeated_factor_name[1]],function(y){
            length(unique(y))
          })
        })[1,]==1)]
      independent_factor_name = independent_factor_name[!independent_factor_name%in%c('phenotype_index','sampleID','batch','Sample_specific_weight','QC','tim_of_injection','rank')]

    }




    }

  if("treatment"%in%independent_factor_name){
    independent_factor_name = "treatment"
  }
  if("condition"%in%independent_factor_name){
    independent_factor_name = "condition"
  }
  independent_factor_name = independent_factor_name[length(independent_factor_name)]
  repeated_factor_name = repeated_factor_name[length(repeated_factor_name)]

return(c(repeated_factor_name,independent_factor_name))




}
