#'normalization
#'@description normalization
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


stat_norm = function(e, f, p,
                     sample_index,
                     mTICdid,Loessdid,medFCdid,BatchMediandid,
                     mTIC,Loess,medFC,BatchMedian,
                     sample_normalization = NULL,data_transformation = NULL,data_scaling = NULL){

  # mTICdid=Loessdid=medFCdid=BatchMediandid=F
  # mTIC=Loess=medFC=BatchMedian=list()
  #
  # e=eData;f=fData;p=pData
  # sample_normalization = "mTIC";data_transformation = "log";data_scaling = "Pareto"
  #
  # sample_index = ""
  # Median Fold Change

  # if(length(mTIC)==0){
  #   temp = temp2 = "a"
  #
  # }else{
  #   temp = mTIC
  #   temp2 = class(mTIC)
  # }

  sample_index = as.numeric(sample_index)
# sample normalization
  if(sample_normalization == "None"){
    if(length(sample_index)==0|| is.na(sample_index)){
      e_after_sample_normalization = e
    }else{
      e_after_sample_normalization = e[!p$phenotype_index%in%sample_index,]
    }
  }else if(sample_normalization == "mTIC"){
    if(mTICdid){
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = mTIC
      }else{
        e_after_sample_normalization = mTIC[!p$phenotype_index%in%sample_index,]
      }
    }else{
      mTIC = stat_mTIC(e,f,p)
      mTIC = data.frame(mTIC)
      colnames(mTIC) = colnames(e);rownames(mTIC) = rownames(e);
      mTICdid = T
        if(length(sample_index)==0|| is.na(sample_index)){
          e_after_sample_normalization = mTIC
        }else{
          e_after_sample_normalization = mTIC[!p$phenotype_index%in%sample_index,]  *   (mean(apply(e[!p$phenotype_index%in%sample_index,],1,mean))/mean(apply(e,1,mean)))
        }
    }

  }else if(sample_normalization == "Loess"){
    if(Loessdid){
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = Loess
      }else{
        e_after_sample_normalization = Loess[!p$phenotype_index%in%sample_index,]
      }
    }else{
      Loess = stat_LoessNorm(e)
      Loess = data.frame(Loess)
      colnames(Loess) = colnames(e);rownames(Loess) = rownames(e);
      Loessdid = T
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = Loess
      }else{
        e_after_sample_normalization = Loess[!p$phenotype_index%in%sample_index,]
      }
    }
#NA
  }else if(sample_normalization == "Median Fold Change"){
    if(medFCdid){
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = medFC
      }else{
        e_after_sample_normalization = medFC[!p$phenotype_index%in%sample_index,]
      }
    }else{
      medFC = stat_medFC(e,f,p)
      medFC = data.frame(medFC)
      colnames(medFC) = colnames(e);rownames(medFC) = rownames(e);
      medFCdid = T
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = medFC
      }else{
        e_after_sample_normalization = medFC[!p$phenotype_index%in%sample_index,]
      }
    }
  }else if(sample_normalization == "Batch Median"){
    if(BatchMediandid){
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = BatchMedian
      }else{
        e_after_sample_normalization = BatchMedian[!p$phenotype_index%in%sample_index,]
      }
    }else{
      BatchMedian = stat_BatchMedianNorm(e)
      BatchMedian = data.frame(BatchMedian)
      colnames(BatchMedian) = colnames(e);rownames(BatchMedian) = rownames(e);
      BatchMediandid = T
      if(length(sample_index)==0|| is.na(sample_index)){
        e_after_sample_normalization = BatchMedian
      }else{
        e_after_sample_normalization = BatchMedian[!p$phenotype_index%in%sample_index,]
      }
    }
    #NA
  }else{
    if(length(sample_index)==0|| is.na(sample_index)){
      e_after_sample_normalization = e
    }else{
      e_after_sample_normalization = e[!p$phenotype_index%in%sample_index,]
    }
  }

  e_after_sample_normalization = data.frame(e_after_sample_normalization)
  if(length(sample_index)==0|| is.na(sample_index)){
    colnames(e_after_sample_normalization) = colnames(e);rownames(e_after_sample_normalization) = rownames(e);
  }else{
    colnames(e_after_sample_normalization) = colnames(e[!p$phenotype_index%in%sample_index,]);rownames(e_after_sample_normalization) = rownames(e[!p$phenotype_index%in%sample_index,]);
  }

# data transformation
  if(data_transformation == "None"){
    e_after_transformation = e_after_sample_normalization

  }else if(data_transformation == "log"){
    e_after_sample_normalization[e_after_sample_normalization<=0] = 0.001 #!!!
    e_after_transformation = log(e_after_sample_normalization)
  }else if(data_transformation =="Cube root"){
    e_after_transformation = e_after_sample_normalization^(1/3)
  }else{
    e_after_transformation = e_after_sample_normalization
  }

# data scaling
  if(data_scaling == "None"){
    e_after_scaling = e_after_transformation
  }else if(data_scaling == "Auto"){
    e_after_scaling = scale(e_after_transformation)
  }else if(data_scaling =="Pareto"){
    e_after_scaling = stat_pareto_scale(e_after_transformation)
  }else if(data_scaling =="Range"){
    e_after_scaling = stat_range_scale(e_after_transformation)
  }else{
    e_after_scaling = e_after_transformation
  }
  if(length(sample_index)==0|| is.na(sample_index)){
    e_after_scaling = data.frame(e_after_scaling)
    colnames(e_after_scaling) = colnames(e);rownames(e_after_scaling) = rownames(e);
  }else{
    e_after_scaling = data.frame(e_after_scaling)
    colnames(e_after_scaling) = colnames(e[!p$phenotype_index%in%sample_index,]);rownames(e_after_scaling) = rownames(e[!p$phenotype_index%in%sample_index,]);
    p = p[!p$phenotype_index%in%sample_index,]
  }



#
  # mTICdid = mTICdid; Loessdid = Loessdid; medFCdid = medFCdid; BatchMediandid = BatchMediandid;
  # mTIC = mTIC; Loess = Loess; medFC = medFC; BatchMedian = BatchMedian
#
#   e_after_scaling[1,1]


  return(list(expression = e_after_scaling, feature  = f, phenotype = p,
              expression_only_rm_outlier = e[!p$phenotype_index%in%sample_index,],
              phenotype_only_rm_outlier = p[!p$phenotype_index%in%sample_index,],
              mTICdid = mTICdid, Loessdid = Loessdid, medFCdid = medFCdid, BatchMediandid = BatchMediandid,
              mTIC = mTIC, Loess = Loess, medFC = medFC, BatchMedian = BatchMedian))

}
