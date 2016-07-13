#'stat_boxplot
#'@description stat_boxplot
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examples
#'@export
#'
#'

stat_boxplot = function(e,p,f,
                        independent_factor_name = NULL, repeated_factor_name = NULL,
                        compound_name_column_index = 1, order_of_factor = NULL)
{

  # order_of_factor = c("pre treatment","post 1","post 2", "AKI")
# because of stupid javas.

  if(length(repeated_factor_name)==0){
    repeated_factor_name = NULL
  }
  if(length(independent_factor_name)==0){
    independent_factor_name = NULL
  }
  factor_name = c(independent_factor_name,repeated_factor_name)
  dta = data.frame(value = e[,1], p[factor_name[!factor_name%in%repeated_factor_name]],p[repeated_factor_name])
  colnames(dta) = c("value",paste0("V",1:length(factor_name)))

  if(is.null(order_of_factor) | length(order_of_factor)==1){
    order_of_factor = unique(dta[,ncol(dta)])
  }else{
    order_of_factor = order_of_factor[-length(order_of_factor)]
  }


  if(ncol(dta)==3){
    dta$V2 = as.character(dta$V2)
    dta$V2 = factor(dta$V2,levels = order_of_factor)
    dta[,2] = as.factor(dta[,2])
    cols = gg_color_hue(length(unique(p[[factor_name[1]]])))
  }else{
    dta[,2] = as.factor(dta[,2])
    cols = "white"
  }

if(ncol(dta)==2){
  for(i in 1:ncol(e)){#for two way cases.
    dta$value = e[,i]

    png(paste0(i,'th_',f[i,compound_name_column_index],'.png'), width = 800, height = 600)
    boxplot(value ~ V1, data = dta,
            boxwex = 0.25,
            col = cols[1],
            main = f[i,compound_name_column_index],
            xlab = factor_name[1],
            ylab = "compound value",
            ylim = c(min(dta$value),max(dta$value)))
    dev.off()

  }
}else if(ncol(dta)==3){
  position = 1/(length(cols)+2)
  position = position*(1:2) - position

  for(i in 1:ncol(e)){#for two way cases.
    dta$value = e[,i]
    png(paste0(i,'th_',f[i,compound_name_column_index],'.png'), width = 800, height = 600)
    boxplot(value ~ V2, data = dta,
            boxwex = max(position),at = 1:length(unique(dta$V2))-max(position),
            col = cols[1],subset = dta$V1 == levels( dta$V1)[1],
            main = f[i,compound_name_column_index],
            xlab = factor_name[1],
            ylab = "compound value",
            ylim = c(min(dta$value),max(dta$value)))

    for(j in 2:length(cols)){# length of the cols is actually the level of V1.
      boxplot(value ~ V2, data = dta, add = TRUE,
              boxwex = max(position),col = cols[j],at = 1:length(unique(dta$V2))+position[j-1],
              subset = V1 == levels(V1)[j],xaxt='n')
    }
    if(median(dta$value[dta$V2==levels(dta$V2)[length(unique(dta$V2))]])>mean(c(min(dta$value),max(dta$value)))){
      legend("bottomright", levels(dta$V1),
             fill = cols,bty = "n")
    }else{
      legend("topright", levels(dta$V1),
             fill = cols,bty = "n")
    }
    dev.off()
  }
}





}

