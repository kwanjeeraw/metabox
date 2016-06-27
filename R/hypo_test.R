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


hypo_test = function(e,p,f,
                     e_ori,p_ori, # This is for mean and sd.
                     independent_factor_name = NULL, repeated_factor_name = NULL,
                     confound = NULL,
                     equal_variance_anova = F){#For repeated study design, samples should match.
  library(parallel);library(userfriendlyscience);library(ez);library(FSA);library(WRS2);library(outliers)
if(length(repeated_factor_name)==0){
  repeated_factor_name = NULL
}
  if(length(independent_factor_name)==0){
    independent_factor_name = NULL
  }



  factor_name = c(independent_factor_name,repeated_factor_name)
  excluded = p$sampleID[!p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]]]
  e = e[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
  e_ori = e_ori[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
  p = p[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
  p_ori = p_ori[p_ori$sampleID%in%names(table(p_ori$sampleID))[table(p_ori$sampleID)%in%sort(table(p_ori$sampleID),decreasing=TRUE)[1]],]






  dta = data.frame(value = e[,1], p[factor_name[!factor_name%in%repeated_factor_name]],p[repeated_factor_name])
  if(is.null(repeated_factor_name)){
    colnames(dta) = c("value", paste0("variable",1:sum(!factor_name%in%repeated_factor_name)))
  }else if(sum(!factor_name%in%repeated_factor_name)==0){
    colnames(dta) = c("value", paste0("repeated",1:sum(length(repeated_factor_name))))
  }else{
    colnames(dta) = c("value",paste0("variable",1:sum(!factor_name%in%repeated_factor_name)),paste0("repeated",1:length(repeated_factor_name)))
  }
  #  ID that are not full, would be deleted. This is critical for repeated measure.


  dta$id = p$sampleID


  for(i in 2:ncol(dta)){
    dta[,i] = factor(dta[,i])
  }
  dta_nonPara = dta

  if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==0 & (length(unique(dta[,2]))>2)){#oneway ANOVA.
    num_factor_variable = length(unique(dta$variable1))
    sudo_matrix = matrix(nrow = num_factor_variable,ncol = num_factor_variable)#helping for cure the format issue with dunnTest().

    result = stat_one_way_ANOVA(data = e,dta,2,sudo_matrix,factor_name)
    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)
    result = data.frame(f,result,check.names = F)
    result_stat = matrix(nrow = ncol(e),ncol = 1 + 1 + (length(unique(dta[,2])))* 2) # global mean, sd. and mean and sd for each group.
    for(i in 1:ncol(e)){
      dta$value = e_ori[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+length(unique(dta[,2])))] = by(dta$value, dta[,2],mean,na.rm = T)
      result_stat[i,(1+length(unique(dta[,2])))+1] = sd(dta$value,na.rm = T)
      result_stat[i,((1+length(unique(dta[,2])))+2):ncol(result_stat)] = by(dta$value, dta[,2],sd,na.rm = T)
    }
    result_stat = data.frame(result_stat,check.names = F)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],sd,na.rm = T))))
    result = cbind(result,result_stat)
    return(result)
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==0 & (length(unique(dta[,2]))==2)){ # t test
    result = stat_t_test(e,dta,2) # 2nd column is the group
    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)
    result = data.frame(f,result,check.names = F)
    result_stat = matrix(nrow = ncol(e),ncol = 1 + 1 + (length(unique(dta[,2])))* 2) # global mean, sd. and mean and sd for each group.
    for(i in 1:ncol(e)){
      dta$value = e_ori[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+length(unique(dta[,2])))] = by(dta$value, dta[,2],mean,na.rm = T)
      result_stat[i,(1+length(unique(dta[,2])))+1] = sd(dta$value,na.rm = T)
      result_stat[i,((1+length(unique(dta[,2])))+2):ncol(result_stat)] = by(dta$value, dta[,2],sd,na.rm = T)
    }
    result_stat = data.frame(result_stat,check.names = F)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],sd,na.rm = T))))
    result = cbind(result,result_stat)
    writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!
    return(result)
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==2 & length(repeated_factor_name)==0){#twoway ANOVA

    num_factor_variable1 = length(unique(dta$variable1))
    num_factor_variable2 = length(unique(dta$variable2))
    sudo_matrix1 = matrix(nrow = num_factor_variable1,ncol = num_factor_variable1)
    sudo_matrix2 = matrix(nrow = num_factor_variable2,ncol = num_factor_variable2)

    cl = makeCluster(detectCores())
    parallel = parLapply(cl, 1:ncol(e),function(i,e,dta,ezANOVA,pbad2way){
      dta$value = e[,i]
      interaction_variable1_variable2 = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1,variable2), type = 3)$ANOVA[3,5]
      interaction_variable1_variable2_nonPara = pbad2way(value ~variable1*variable2,
                                   data = dta,est = "median")$AB.p.value

      return(data.frame(interaction_variable1_variable2,interaction_variable1_variable2=interaction_variable1_variable2_nonPara))
    },e,dta,ezANOVA,pbad2way)
    stopCluster(cl)
    interaction = do.call("rbind",parallel)
    colnames(interaction) = c(paste("p_value: Interaction between", paste(factor_name,collapse = "_and_")),
                              paste("non_para_p_value: Interaction between", paste(factor_name,collapse = "_and_")))

    if(!length(unique(dta$variable2))==2){ # if TRUE, then use ANOVA on main effect post hoc analysis.
      variable2_posthoc = stat_one_way_ANOVA(data = e,dta, i=3, sudo_matrix2, factor_name)
      variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
        # x = dta[dta$variable1==levels(dta$variable1)[1],]
        result = stat_one_way_ANOVA(data = e[dta$variable1==unique(x$variable1),],data2 = x,i=3,sudo_matrix=sudo_matrix2,factor_name) # the 3rd column is group.
        return(result)
      })
      for(i in 1:length(variable1_simple_main_effect)){
        colnames(variable1_simple_main_effect[[i]]) = paste0(levels(dta$variable1)[i],":_",colnames(variable1_simple_main_effect[[i]]))
      }
      result_temp = variable1_simple_main_effect[[1]]
      for(i in 2:length(variable1_simple_main_effect)){
        result_temp = cbind(result_temp,variable1_simple_main_effect[[i]])
      }
      variable1_simple_main_effect = result_temp
    }else{
      # if it is two levels, then no need to do post hoc analysis.

      variable2_t_test = stat_t_test(data = e,dta, i=3)

      variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
        # x = dta[dta$variable1=="drug 2",]
        result = stat_t_test(e[dta$variable1==unique(x$variable1),],data2 = x,3) # the 3rd column is group.
        return(result)
      })# returns two length of list. Ordered by levels of INDICES in by().
      for(i in 1:length(variable1_simple_main_effect)){
        colnames(variable1_simple_main_effect[[i]]) = paste0(levels(dta$variable1)[i],":_",colnames(variable1_simple_main_effect[[i]]))
      }
      result_temp = variable1_simple_main_effect[[1]]
      for(i in 2:length(variable1_simple_main_effect)){
        result_temp = cbind(result_temp,variable1_simple_main_effect[[i]])
      }
      variable1_simple_main_effect = result_temp
    }


    if(!length(unique(dta$variable1))==2){# if TRUE, then use ANOVA on main effect post hoc analysis.
      variable1_posthoc = stat_one_way_ANOVA(data = e,dta, i=2, sudo_matrix1, factor_name)
      variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
        # x = dta[dta$variable2==levels(dta$variable2)[1],]
        result = stat_one_way_ANOVA(data = e[dta$variable2==unique(x$variable2),],data2 = x,i=2,sudo_matrix=sudo_matrix1,factor_name) # the 3rd column is group.
        return(result)
      })
      for(i in 1:length(variable2_simple_main_effect)){
        colnames(variable2_simple_main_effect[[i]]) = paste0(levels(dta$variable2)[i],":_",colnames(variable2_simple_main_effect[[i]]))
      }
      result_temp = variable2_simple_main_effect[[1]]
      for(i in 2:length(variable2_simple_main_effect)){
        result_temp = cbind(result_temp,variable2_simple_main_effect[[i]])
      }
      variable2_simple_main_effect = result_temp
    }else{
      # if it is two levels, then no need to do post hoc analysis.
      variable1_t_test = stat_t_test(data = e,dta, i=2)

      variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
        # x = dta[dta$variable2=="time 1",]
        result = stat_t_test(e[dta$variable2==unique(x$variable2),],x, i=2) # 2nd column is group.
        return(result)
      })
      for(i in 1:length(variable2_simple_main_effect)){
        colnames(variable2_simple_main_effect[[i]]) = paste0(levels(dta$variable2)[i],":_",colnames(variable2_simple_main_effect[[i]]))
      }
      result_temp = variable2_simple_main_effect[[1]]
      for(i in 2:length(variable2_simple_main_effect)){
        result_temp = cbind(result_temp,variable2_simple_main_effect[[i]])
      }
      variable2_simple_main_effect = result_temp
    }


    if(num_factor_variable1 == 2 & (!num_factor_variable2 == 2)){
      result = data.frame(interaction,variable1_t_test,variable2_posthoc,variable1_simple_main_effect,variable2_simple_main_effect,check.names = FALSE)
    }else if(num_factor_variable2 == 2 & (!num_factor_variable1 == 2)){
      result = data.frame(interaction,variable1_posthoc,variable2_t_test,variable1_simple_main_effect,variable2_simple_main_effect,check.names = FALSE)
    }else if((num_factor_variable1 == 2) & (num_factor_variable2 == 2)){
      result = data.frame(interaction,variable1_t_test,variable2_t_test,variable1_simple_main_effect,variable2_simple_main_effect,check.names = FALSE)
    }else{
      result = data.frame(interaction,variable1_posthoc,variable2_posthoc,variable1_simple_main_effect,variable2_simple_main_effect,check.names = FALSE)
    }


    # result[,-1] = result[,!abs(diff(result[1,]))<0.000000001]
    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)

    result = data.frame(f,result,check.names = F)


    result_stat = matrix(nrow = ncol(e),ncol = (1 + length(unique(dta$variable1)) + length(unique(dta$variable2)) +
                           length(unique(dta$variable1)) * length(unique(dta$variable2)))*2) # global mean, sd. and mean and sd for each group.
    add = (1+num_factor_variable1 + num_factor_variable2 + num_factor_variable1*num_factor_variable2)
    for(i in 1:ncol(e)){
      dta$value = e_ori[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+num_factor_variable1)] = by(dta$value, dta$variable1,mean,na.rm = T)
      result_stat[i,(1+num_factor_variable1+1):(1+num_factor_variable1 + num_factor_variable2)] =
        by(dta$value, dta$variable2,mean,na.rm = T)

      result_stat[i,(1+num_factor_variable1 + num_factor_variable2+1):
                    (1+num_factor_variable1 + num_factor_variable2 + num_factor_variable1*num_factor_variable2)] =
        by(dta$value, paste(dta$variable1,dta$variable2, sep = "*"),mean,na.rm = T)


      result_stat[i,1+add] = sd(dta$value,na.rm = T)
      result_stat[i,(2:(1+num_factor_variable1))+add] = by(dta$value, dta$variable1,sd,na.rm = T)
      result_stat[i,((1+num_factor_variable1+1):(1+num_factor_variable1 + num_factor_variable2))+add] =
        by(dta$value, dta$variable2,mean,na.rm = T)

      result_stat[i,((1+num_factor_variable1 + num_factor_variable2+1):
                    (1+num_factor_variable1 + num_factor_variable2 + num_factor_variable1*num_factor_variable2))+add] =
        by(dta$value, paste(dta$variable1,dta$variable2, sep = "*"),sd,na.rm = T)

    }
    result_stat = data.frame(result_stat,check.names = F)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta$variable1,mean,na.rm = T))),
                              paste("Mean of", names( by(dta$value, dta$variable2,mean,na.rm = T))),
                              paste("Mean of", names( by(dta$value, paste(dta$variable1,dta$variable2, sep = "*"),mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta$variable1,sd,na.rm = T))),
                              paste("Standard Deviation of", names( by(dta$value, dta$variable2,sd,na.rm = T))),
                              paste("Standard Deviation of", names( by(dta$value, paste(dta$variable1,dta$variable2, sep = "*"),sd,na.rm = T)))
                              )

    result = cbind(result,result_stat)





    return(result)




  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==3 & length(repeated_factor_name)==0){# three way anova.
    return("Have not done yet!")
  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==1 & (length(unique(dta[,2]))>2)){# one way repeated anova.


    num_factor_variable = length(unique(dta$repeated1))
    sudo_matrix = matrix(nrow = num_factor_variable,ncol = num_factor_variable)#helping for cure the format issue with dunnTest().

    result = stat_one_way_repeated_ANOVA(data = e,data2 = dta,i = 2,sudo_matrix,factor_name)
    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)
    result = data.frame(f,result,check.names = F)
    result_stat = matrix(nrow = ncol(e),ncol = 1 + 1 + (length(unique(dta[,2])))* 2) # global mean, sd. and mean and sd for each group.
    for(i in 1:ncol(e)){
      dta$value = e_ori[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+length(unique(dta[,2])))] = by(dta$value, dta[,2],mean,na.rm = T)
      result_stat[i,(1+length(unique(dta[,2])))+1] = sd(dta$value,na.rm = T)
      result_stat[i,((1+length(unique(dta[,2])))+2):ncol(result_stat)] = by(dta$value, dta[,2],sd,na.rm = T)
    }
    result_stat = data.frame(result_stat,check.names = F)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],sd,na.rm = T))))
    result = cbind(result,result_stat)
    return(result)


  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==1 & (length(unique(dta[,2]))==2)){# paired t test
    result = stat_paired_t_test(e,dta,2) # 2nd column is the group
    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)
    result = data.frame(f,result,check.names = F)
    result_stat = matrix(nrow = ncol(e),ncol = 1 + 1 + (length(unique(dta[,2])))* 2) # global mean, sd. and mean and sd for each group.
    for(i in 1:ncol(e)){
      dta$value = e_ori[,i]
      result_stat[i,1] = mean(dta$value,na.rm = T)
      result_stat[i,2:(1+length(unique(dta[,2])))] = by(dta$value, dta[,2],mean,na.rm = T)
      result_stat[i,(1+length(unique(dta[,2])))+1] = sd(dta$value,na.rm = T)
      result_stat[i,((1+length(unique(dta[,2])))+2):ncol(result_stat)] = by(dta$value, dta[,2],sd,na.rm = T)
    }
    result_stat = data.frame(result_stat,check.names = F)
    colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta[,2],mean,na.rm = T))),
                              "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta[,2],sd,na.rm = T))))
    result = cbind(result,result_stat)
    return(result)

  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==2){# two way repeated anova.

  }else if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==1){# mixed anova

    dta$value = e[,1]
    test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
                        within = .(repeated1), type = 3)
    if(!length(unique(dta$repeated1))==2){
      interaction_repeated1 = test.temp$`Sphericity Corrections`[2:1,3] # interaction term and other main effect terms.
      variable1 = test.temp$ANOVA[1,5]
      interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
      test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
      repeated1_posthoc = as.numeric(test.temp)
    }else{
      interaction_variable1_repeated1 = test.temp$ANOVA[3:1,5]
      repeated1_posthoc = NA
    }


    if(!length(unique(dta$variable1))==2){
      variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
    }else{
      variable1_posthoc = NA
    }





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
    # clusterExport(cl, c("ezANOVA","dta","e","posthocTGH","result"))
    parallel = parLapply(cl, 2:ncol(e),
              function(i,dta,e,result,ezANOVA,posthocTGH){
                dta$value = e[,i]
                test.temp = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1),
                                    within = .(repeated1), type = 3)
                if(!length(unique(dta$repeated1))==2){
                  interaction_repeated1 = test.temp$`Sphericity Corrections`[2:1,3] # interaction term and other main effect terms.
                  variable1 = test.temp$ANOVA[1,5]
                  interaction_variable1_repeated1 = c(interaction_repeated1, variable1)
                  test.temp = pairwise.t.test(paired = T, x = dta$value, g = dta$repeated1, p.adjust.method  = "bonf")$p.value
                  repeated1_posthoc = as.numeric(test.temp)
                }else{
                  interaction_variable1_repeated1 = test.temp$ANOVA[3:1,5]
                  repeated1_posthoc = NA
                }


                if(!length(unique(dta$variable1))==2){
                  variable1_posthoc = posthocTGH(dta$value , dta$variable1,method="games-howell", digits=4)$output$games.howell[,3]
                }else{
                  variable1_posthoc = NA
                }

                repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
                  return(c(p_value = oneway.test(value ~ variable1, data=x)$p.value,
                           posthocTGH(x$value , x$variable1,method="games-howell", digits=4)$output$games.howell[,3]))
                })
                variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
                  return(c(p_value = oneway.test(value ~ repeated1, data=x)$p.value,
                           posthocTGH(x$value , x$repeated1,method="games-howell", digits=4)$output$games.howell[,3]))
                })
                unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect))

              },dta = dta, e=e,result=result,ezANOVA=ezANOVA,posthocTGH = posthocTGH)
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

    name = names( unlist(c(interaction_variable1_repeated1,variable1_posthoc,repeated1_posthoc, variable1_simple_main_effect,repeated1_simple_main_effect)))

    name=name[!is.na(result[1,])]
    result=result[,!is.na(result[1,])]
    name[1:3]=c(paste0("Interaction-",factor_name[!factor_name%in%repeated_factor_name],"*",repeated_factor_name),
                factor_name[!factor_name%in%repeated_factor_name], repeated_factor_name)
    name = gsub("\\.",": ",gsub(":", " vs ", name))
    name[name==""] = apply(combn(levels(dta$repeated1),2),2,function(x){paste(x[1],"vs",x[2])})


    colnames(result) = name
    result = round(result,7)


    result = result[,rowSums(apply(result,1,function(x){
      duplicated(x)
    }))/nrow(result)<0.50]

    f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)
    result = data.frame(f,result,check.names = F)





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


