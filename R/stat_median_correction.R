#'stat_median_correction
#'@description stat_median_correction
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
#'
stat_median_correction = function(before, batch, QC){ #before is a vector. batch is also a vector.
  medians = by(before[QC], batch[QC], median, na.rm = T)
  global.median = median(before[QC])
  factor = medians/global.median
  after  = before
  for(b in names(factor)){
    after[batch%in%b] = before[batch%in%b]/factor[b]
  }
  return(after)
}
