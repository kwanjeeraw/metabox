#'PCA
#'@description PCA.
#'
#'@usage
#'@param e should be the e, where rows are samples and columns are compounds.

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export

PCA_Data_Exploration = function(e = NULL, p = NULL, f = NULL,
                                # scree
                                scree_Title = "Scree Plot",
                                scree_xlab = paste("PC",PC_x), scree_ylab=paste("PC",PC_y),
                                # score
                                PC_x = 1, PC_y = 2, PC_z = 3,
                                score_Title = "Score Plot", score_xlab = paste("PC",PC_x), score_ylab = paste("PC",PC_y),
                                color_group = NULL, ellipse = T, names_on_scatter = NULL,
                                # loading
                                loading_Title, loading_xlab, loading_ylab){ # doesn't tolerate missing value!

  pca = prcomp(e, center = TRUE, scale. = TRUE)
  scores = pca$x[,c(PC_x, PC_y, PC_z)]
  variance_explained = pca$sdev^2





  prcomp(log.ir,
         center = TRUE,
         scale. = TRUE)
}
