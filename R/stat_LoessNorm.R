#'stat_LoessNorm
#'@description stat_LoessNorm
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
stat_LoessNorm = function(data = e, f, p,
                          loess.para = 0.75,auto.batch.detection = FALSE,
                          robust = TRUE, QCIndicator, BatchIndicator, TimeIndicator){
  library(parallel);library(fANCOVA);

  if(sum(!p[,QCIndicator]%in%c("TRUE","FALSE"))>0){
    stop("<strong>QC must be EITHER 'TRUE' or 'FALSE' only.</strong>")
  }

  QC = p[,QCIndicator]=="TRUE"; batch = p[,BatchIndicator]
  # ;KnownornotKnown = f$KnownorUnknown;


    time = as.numeric(p[,TimeIndicator]); rank = rank(time);
    # p$time_of_injection = time


  data = data.frame(index = 1:nrow(data),time, rank, batch,QC, data)
  data.sort.time = data[order(rank),]#sort according to time and then detect if the diff of time is big. If big, then another batch.
  if(auto.batch.detection){
    data.sort.time = do.call("rbind",by(data.sort.time, data.sort.time$batch, FUN = function(x){
      num.new.batch <- sum(scale(diff(x$time))>1.96)
      split.point = which(scale(diff(x$time))>1.96)
      x$new.batch = rep(letters[1:(num.new.batch+1)], diff(c(0,split.point,nrow(x))))
      return(x)
    }))
    data.sort.time = data.sort.time[order(data.sort.time$time),]
    data.sort.time$batch.final = paste0(data.sort.time$batch,"_", data.sort.time$new.batch)
  }else{
    data.sort.time$batch.final = data.sort.time$batch
  }


  # check if there are at least 4 QC in the batch!
  if(sum(by(data.sort.time$QC,data.sort.time$batch.final,sum)<4)>0){
    stop(paste0("<strong>The QC is not enough!</strong> At least <strong>4</strong> QCs are needed for each batch!", names(by(data.sort.time$QC,data.sort.time$batch.final,sum))[by(data.sort.time$QC,data.sort.time$batch.final,sum)<4],
                " do not have enough QC. You may try not to do auto.batch.detection if you selected it OR add more QC."))
  }


  tf = t(f);colnames(tf) = f$feature_index;tf = data.frame(tf) # in order to have the colnames(data.sort.time) and f$compound matched.
  cl <- makeCluster(detectCores(logical = TRUE))
  o=parSapply(cl = cl, which(colnames(data.sort.time)%in%colnames(tf)),
              FUN = function(i,data.sort.time,stat_median_correction, span,loess.as,robust){
                # i = 49
                data.temp = data.frame(data.sort.time[,c("index", "time", "rank", "batch.final", "batch","QC", colnames(data.sort.time)[i])],
                                       value = data.sort.time[,colnames(data.sort.time)[i]]) # the last column is simply for plot.

                # x = data.temp[data.temp$batch.final=="A b",];span=.75

                data.temp2 = do.call("rbind",by(data.temp, data.temp$batch.final, FUN = function(x){
                  x.sub = x[x$QC,]
                  if(robust){
                    x.sub = x.sub[!x.sub$value%in%boxplot(x.sub$value,plot = FALSE)$out,] #!!! outlier!
                  }
                  loess <- tryCatch(loess.as(x.sub$rank, x.sub$value, plot=F),warning = function(w){
                    loess(value ~ rank, x.sub, span = span)
                  })
                  pred = predict(loess, x$rank)
                  pred.front = pred[1:round(length(pred)/2)]
                  pred.behind = pred[(round(length(pred)/2)+1):length(pred)]
                  pred.front[is.na(pred.front)] = pred.front[!is.na(pred.front)][1]
                  pred.behind[is.na(pred.behind)] = pred.behind[!is.na(pred.behind)][length(pred.behind[!is.na(pred.behind)])]
                  pred = c(pred.front, pred.behind)
                  normalized.value.minus = x$value / (pred / median(x$value[x$QC]))
                  result=data.frame(x,pred,normalized.value.minus)
                  colnames(result)[colnames(result)=="pred"] <- paste0("pred.",colnames(data.sort.time)[i])
                  return(result)
                }))
                data.temp2[,colnames(data.sort.time)[i]] <-
                  stat_median_correction(before = data.temp2$normalized.value.minus, batch = data.temp2$batch.final, QC = data.temp2$QC)
                return(list(data.temp2))
              },data.sort.time = data.sort.time,stat_median_correction = stat_median_correction, span = loess.para
              ,loess.as = loess.as,robust = robust
  )
  stopCluster(cl = cl)
  result.o = do.call("cbind",o) # merge them all and select the columns that have the same column name with data.sort.time.
  to.be.added = !colnames(data.sort.time)%in%colnames(result.o)
  result.o = cbind(result.o,data.sort.time[,to.be.added])
  colnames(result.o)[((ncol(result.o) - sum(to.be.added))+1):ncol(result.o)] = colnames(data.sort.time)[to.be.added]
  result = result.o[,colnames(data.sort.time)]
  result = result[order(result$index),]
  result.o = result.o[order(result.o$index),]
  rownames(result) = rownames(result.o) = paste0(1:nrow(data.sort.time),paste0(".",rownames(data)))
  attributes(result)$lines = result.o[,grepl("pred.", colnames(result.o))]
  colnames(attributes(result)$lines) = gsub('pred.','',colnames(attributes(result)$lines))
  attributes(result)$batch = result.o$batch.final
  return(result)
}
