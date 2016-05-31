#'hypothesis test
#'@description hypothesis test
#'
#'@usage
#'@param

#'@details
#'
#'@return
#'@author Sili Fan \email{fansili2013@gmail.com}
#'@seealso
#'@examplesz
#'@export
#

# library(ez)
# library(userfriendlyscience)
hypo_test = function(e,p,f, independent_factor_name = NULL, repeated_factor_name = NULL,
                     confound = NULL,
                     equal_variance_anova = F){#For repeated study design, samples should match.
if(length(repeated_factor_name)==0){
  repeated_factor_name = NULL
}
  library(parallel);library(userfriendlyscience);library(ez)


  factor_name = c(independent_factor_name,repeated_factor_name)
  e = e[-c(1:6),] #temp!
  p = p[-c(1:6),] #temp!

  dta = data.frame(value = e[,1], p[factor_name[!factor_name%in%repeated_factor_name]],p[repeated_factor_name])
  if(is.null(repeated_factor_name)){
    colnames(dta) = c("value", paste0("variable",1:sum(!factor_name%in%repeated_factor_name)))
  }else if(sum(!factor_name%in%repeated_factor_name)==0){
    colnames(dta) = c("value", paste0("repeated",1:sum(length(repeated_factor_name))))
  }else{
    colnames(dta) = c("value",paste0("variable",1:sum(!factor_name%in%repeated_factor_name)),paste0("repeated",1:length(repeated_factor_name)))
  }
  # dta$id = 1:nrow(dta)#!!!
  dta$id = p$sampleID
  # dta = dta[!dta$id%in%19:24,]
  for(i in 2:ncol(dta)){
    dta[,i] = factor(dta[,i])
  }
  if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==0){#oneway ANOVA including t test.
    result = matrix(nrow = ncol(e),ncol = sum((length(unique(dta$variable1))-1):1) + 1 + 1)#fdr
    for(i in 1:ncol(e)){
      dta$value = e[,i]
      p_value = oneway.test(value ~ variable1, data=dta)$p.value
      post.hoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
      result[i,-2] = c(p_value = p_value,post.hoc)#fdr
    }
    result[,2] = p.adjust(result[,1],"fdr")
    colnames(result) = c("p_value","Adjust_p_value",apply(combn(levels(dta$variable1), 2),2,function(x){paste(x[1],x[2],sep=" vs ")}))
    rownames(result) = colnames(e)
    result = data.frame(result,stringsAsFactors = F)
    result = data.frame(f,result)
    return(result)
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==2 & length(repeated_factor_name)==0){#twoway ANOVA
    result = matrix(nrow = ncol(e),ncol = 1+1+1+length(unique(dta$variable1)) + length(unique(dta$variable2)) +
                      length(unique(dta$variable1)) * (sum((length(unique(dta$variable2))-1):1)) + length(unique(dta$variable2)) * (sum((length(unique(dta$variable1))-1):1)))
    if(length(unique(dta$variable1))==2){# if variable 1 has only two levels.
      for(i in 1:ncol(e)){
        dta$value = e[,i]
        interaction_variable1_variable2 = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1,variable2), type = 3)$ANOVA[,5] # interaction term and other main effect terms.
        variable2_posthoc = posthocTGH(dta$value , dta$variable2,method="games-howell", digits=4)$output$games.howell[,3]
        variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable2, data=x)$p.value,
                   posthocTGH(x$value , x$variable2,method="games-howell", digits=4)$output$games.howell[,3]))
        })
        variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value))
        })
        result[i,] = unlist(c(interaction_variable1_variable2,variable2_posthoc,variable1_simple_main_effect,variable2_simple_main_effect))
      }
      name = names( unlist(c(interaction_variable1_variable2,variable1_posthoc,variable1_simple_main_effect,variable2_simple_main_effect)))
      name[1:3]=c("Interaction", "variable1", "variable2")
      name[7:(7+length(unique(dta$variable2))-1)] = paste0(name[7:(7+length(unique(dta$variable2))-1)], ": ",paste(unique(dta$variable1),collapse = " vs "))
      name = gsub("\\.",": ",gsub(":", " vs ", name))
    }else if(length(unique(dta$variable2))==2){# if variable 2 has only two levels.
      for(i in 1:ncol(e)){
        dta$value = e[,i]
        interaction_variable1_variable2 = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1,variable2), type = 3)$ANOVA[,5] # interaction term and other main effect terms.
        variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
        variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
                   posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
        })
        variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable2, data=x)$p.value))
        })
        result[i,] = unlist(c(interaction_variable1_variable2,variable1_posthoc,variable1_simple_main_effect,variable2_simple_main_effect))
      }
      name = names( unlist(c(interaction_variable1_variable2,variable1_posthoc,variable1_simple_main_effect,variable2_simple_main_effect)))
      name[1:3]=c("Interaction", "variable1", "variable2")
      name[7:(7+length(unique(dta$variable1))-1)] = paste0(name[7:(7+length(unique(dta$variable1))-1)], ": ",paste(unique(dta$variable2),collapse = " vs "))
      name = gsub("\\.",": ",gsub(":", " vs ", name))
    }else{# else.
      for(i in 1:ncol(e)){
        dta$value = e[,i]
        interaction_variable1_variable2 = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1,variable2), type = 3)$ANOVA[,5] # interaction term and other main effect terms.
        variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
        variable2_posthoc = posthocTGH(dta$value , dta$variable2,method="games-howell", digits=4)$output$games.howell[,3]
        variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
                   posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
        })
        variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
          return(c(p_value = oneway.test(value ~ variable2, data=x)$p.value,
                   posthocTGH(x$value , x$variable2,method="games-howell", digits=4)$output$games.howell[,3]))
        })
        result[i,] = unlist(c(interaction_variable1_variable2,variable1_posthoc,variable2_posthoc,variable1_simple_main_effect,variable2_simple_main_effect))
      }
      name = names( unlist(c(interaction_variable1_variable2,variable1_posthoc,variable1_simple_main_effect,variable2_simple_main_effect)))
      name[1:3]=c("Interaction", "variable1", "variable2")
      name = gsub("\\.",": ",gsub(":", " vs ", name))
    }
    result = data.frame(result)
    colnames(result) = name
    result = data.frame(f, result)
    return(result)
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==3 & length(repeated_factor_name)==0){# three way anova.
    return("Have not done yet!")
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==1){# one way repeated anova.
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==2){# two way repeated anova.
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==1){# mixed anova
    dta$value = e[,1]
    test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
                        within = .(repeated1), type = 3)
    interaction_repeated1 = test.temp$`Sphericity Corrections`[,3] # interaction term and other main effect terms.
    variable1 = test.temp$ANOVA[1,5]
    interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
    variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
    test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
    repeated1_posthoc = as.numeric(test.temp)
    repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
      return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
               posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
    })
    variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
      return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
               posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
    })
    result = matrix(nrow = ncol(e),ncol = length(unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))))
    result[1,] = unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))
    cl = makeCluster(detectCores())
    clusterExport(cl, c("ezANOVA","dta","e","posthocTGH","result"))
    parallel = parLapply(cl, 2:ncol(e),
              function(i){
                dta$value = e[,i]
                test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
                                    within = .(repeated1), type = 3)
                interaction_repeated1 = test.temp$`Sphericity Corrections`[,3] # interaction term and other main effect terms.
                variable1 = test.temp$ANOVA[1,5]
                interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
                variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
                test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
                repeated1_posthoc = as.numeric(test.temp)
                repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
                  return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
                           posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
                })
                variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
                  return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
                           posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
                })
                unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))

              })
    stopCluster(cl)
    result = rbind(result[1,],do.call(rbind,parallel))

    # for(i in 2:ncol(e)){
    #   dta$value = e[,i]
    #   test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
    #           within = .(repeated1), type = 3)
    #   interaction_repeated1 = test.temp$`Sphericity Corrections`[,3] # interaction term and other main effect terms.
    #   variable1 = test.temp$ANOVA[1,5]
    #   interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
    #   variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
    #   test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
    #   repeated1_posthoc = as.numeric(test.temp)
    #   repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
    #     return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
    #              posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
    #   })
    #   variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
    #     return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
    #              posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
    #   })
    #   result[i,] = unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))
    # }
    result=result[,!is.na(result[1,])]
    name = names( unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc[!is.na(repeated1_posthoc)], variable1_simple_main_effect,repeated1_simple_main_effect)))
    name[1:3]=c(paste0("Interaction:",factor_name[!factor_name%in%repeated_factor_name],"*",repeated_factor_name),
                factor_name[!factor_name%in%repeated_factor_name], repeated_factor_name)
    name = gsub("\\.",": ",gsub(":", " vs ", name))
    name[name==""] = apply(combn(levels(dta$repeated1),2),2,function(x){paste(x[1],"vs",x[2])})
    colnames(result) = name
    result = data.frame(f,result)
    return(result)
  }else{
    return("Haven't done yet!")
  }

}

#
# a = hypo_test(e = e[!p$id%in%17:24,],p=p[!p$id%in%17:24,],f = f,factor_name = c("treatment","time"),repeated_factor_name="time")
# b = hypo_test(e = e[!p$id%in%17:24,],p=p[!p$id%in%17:24,],f = f,factor_name = "treatment",repeated_factor_name=NULL)
#


#
# microbenchmark::microbenchmark(
#   {
#     for(i in 2:ncol(e)){
#       dta$value = e[,i]
#       test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
#                           within = .(repeated1), type = 3)
#       interaction_repeated1 = test.temp$`Sphericity Corrections`[,3] # interaction term and other main effect terms.
#       variable1 = test.temp$ANOVA[1,5]
#       interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
#       variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
#       test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
#       repeated1_posthoc = as.numeric(test.temp)
#       repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
#         return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
#                  posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
#       })
#       variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
#         return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
#                  posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
#       })
#       result[i,] = unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))
#     }
#   },
#   {
#     cl = makeCluster(detectCores())
#     clusterExport(cl, c("ezANOVA","dta","e","posthocTGH","result"))
#     parLapply(cl,
#               2:ncol(e),
#               function(i){
#                 dta$value = e[,i]
#                 test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
#                                     within = .(repeated1), type = 3)
#                 interaction_repeated1 = test.temp$`Sphericity Corrections`[,3] # interaction term and other main effect terms.
#                 variable1 = test.temp$ANOVA[1,5]
#                 interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
#                 variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
#                 test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
#                 repeated1_posthoc = as.numeric(test.temp)
#                 repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
#                   return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
#                            posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
#                 })
#                 variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
#                   return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
#                            posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
#                 })
#                 unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))
#
#               })
#     stopCluster(cl)
#   },
#   times = 10L
# )


