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

stat_hypo_test = function(e,p,f,
                     e_ori,p_ori, # This is for mean and sd.
                     e_before, p_before,
                     independent_factor_name = NULL, repeated_factor_name = NULL,
                     need_power = T,desired_power =  NULL,

                     ttestmethod = NULL, ttestcorrection = NULL, nonparattestmethod = NULL, nonparattestcorrection = NULL,
                     ANOVAmethod = NULL, ANOVAposthoc = NULL, nonparaANOVAmethod = NULL, nonparaANOVAposthoc = NULL






                     # onewayANOVAmethod=NULL,nonpara_onewayANOVAmethod=NULL,onewayANOVAposthocmethod=NULL,nonpara_onewayANOVAposthocmethod=NULL,
                     #
                     # ttestmethod=NULL,ttestFDRmethod=NULL,nonpara_ttestmethod=NULL,nonpara_ttestFDRmethod=NULL,
                     #
                     # twowayANOVAmethod=NULL,maineffectANOVAmethod1=NULL,maineffectANOVAposthocmethod1=NULL,maineffectANOVAmethod2=NULL,maineffectANOVAposthocmethod2=NULL,
                     # simplemaineffectANOVAmethod1=NULL,simplemaineffectANOVAposthocmethod1=NULL,simplemaineffectANOVAmethod2=NULL,simplemaineffectANOVAposthocmethod2=NULL,
                     # nonpara_maineffectANOVAmethod1=NULL,nonpara_maineffectANOVAposthocmethod1=NULL,nonpara_maineffectANOVAmethod2=NULL,nonpara_maineffectANOVAposthocmethod2=NULL,
                     # nonpara_simplemaineffectANOVAmethod1=NULL,nonpara_simplemaineffectANOVAposthocmethod1=NULL,nonpara_simplemaineffectANOVAmethod2=NULL,nonpara_simplemaineffectANOVAposthocmethod2=NULL,
                     # maineffectttestmethod1=NULL,maineffectttestposthocmethod1=NULL,maineffectttestmethod2=NULL,maineffectttestposthocmethod2=NULL,
                     # simplemaineffectttestmethod1=NULL,simplemaineffectttestposthocmethod1=NULL,simplemaineffectttestmethod2=NULL,simplemaineffectttestposthocmethod2=NULL,
                     # nonpara_maineffectttestmethod1=NULL,nonpara_maineffectttestposthocmethod1=NULL,nonpara_maineffectttestmethod2=NULL,nonpara_maineffectttestposthocmethod2=NULL,
                     # nonpara_simplemaineffectttestmethod1=NULL,nonpara_simplemaineffectttestposthocmethod1=NULL,nonpara_simplemaineffectttestmethod2=NULL,nonpara_simplemaineffectttestposthocmethod2=NULL,
                     #
                     # onewayrepeatedANOVAmethod = "rANOVA",onewayrepeatedANOVAposthocmethod = "paired+bonf",onewaySpher_Corr = NULL,
                     # nonpara_onewayrepeatedANOVAmethod = "rANOVA",nonpara_onewayrepeatedANOVAposthocmethod = "nonpara_paired+bonf",
                     #
                     # pairedttestmethod = "paired t test", pairedttestFDRmethod = NULL, nonpara_pairedttestmethod = "Wil test", nonpara_pairedttestFDRmethod = NULL,
                     #
                     # mainSpher_Corr1 = NULL,mainSpher_Corr2 = NULL,simplemainSpher_Corr1 = NULL,simplemainSpher_Corr2 = NULL,
                     # simplemaineffectpairedttestposthocmethod1 = NULL, simplemaineffectpairedttestposthocmethod2 = NULL, nonpara_simplemaineffectpairedttestposthocmethod1 = NULL,
                     # nonpara_simplemaineffectpairedttestposthocmethod2 = NULL,
                     # maineffectpairedttestposthocmethod1 = NULL,nonpara_maineffectpairedttestposthocmethod1 = NULL,
                     #
                     # maineffectpairedttestposthocmethod2 = NULL,nonpara_maineffectpairedttestposthocmethod2 = NULL

                     ){#For repeated study design, samples should match.





  library(parallel);library(userfriendlyscience);library(ez);library(FSA);library(WRS2);library(outliers);library(pwr);library(reshape2);

  if(as.numeric(desired_power)>100 | as.numeric(desired_power) < 0 | is.na(as.numeric(desired_power))){
    stop("desired_power must between 0 ~ 100")
  }

    desired_power = as.numeric(desired_power)/100



  # Here need to check the whether the QC in p have right format!
  # the independent or repeated factor must be empty cell!


  if(length(repeated_factor_name)==0){
  repeated_factor_name = NULL

  if("QC"%in%colnames(p)){
    if(sum(!p[p$QC=="TRUE",independent_factor_name]=="NA")>0){
      stop(paste0("QC must have empty cell for the column ",independent_factor_name,". Please correct them before upload your file."))
    }
  }



  }else{
    if(sum(duplicated(p$sampleID))==0){
      stop("The sampleID you provided doesn't support the repeated design. Please close and check or not to select within subject factor.")
    }
  }

if(length(independent_factor_name)==0){
  independent_factor_name = NULL
  if("QC"%in%colnames(p)){
  if(sum(!p[p$QC=="TRUE",repeated_factor_name]=="NA")>0){
    stop(paste0("QC must have empty cell for the column ",repeated_factor_name,". Please correct them before upload your file."))
  }
  }
}

  factor_name = c(independent_factor_name,repeated_factor_name)







  noNA = !(tryCatch(p[,independent_factor_name]=="NA",error = function(e){return(rep(F,nrow(p)))}) | tryCatch(p[,repeated_factor_name]=="NA",error = function(e){return(rep(F,nrow(p)))}))

  e_withQC = e[!noNA,];p_withQC = p[!noNA,];

  e = e[noNA,]; p = p[noNA,]
  e_ori = e_ori[noNA,]; p_ori = p_ori[noNA,]


      excluded = p$sampleID[!p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]]]
      e = e[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
      e_ori = e_ori[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
      p = p[p$sampleID%in%names(table(p$sampleID))[table(p$sampleID)%in%sort(table(p$sampleID),decreasing=TRUE)[1]],]
      p_ori = p_ori[p_ori$sampleID%in%names(table(p_ori$sampleID))[table(p_ori$sampleID)%in%sort(table(p_ori$sampleID),decreasing=TRUE)[1]],]

      feature_contain_constant_group = unique(unlist(sapply(by(e,apply(data.frame(p[,factor_name]),1,function(y){
        paste(y,collapse = "")
      }),function(x){
        sapply(x,sd)
      }),function(x){
        which(x==0)
      })))

      e_before_delete_feature_contain_constant_group = e
      if(length(feature_contain_constant_group)>0){ # all the compounds that have constant value would be have NA as p value at the re

        for( i in feature_contain_constant_group){
          sds = by(e[,i],apply(data.frame(p[,factor_name]),1,function(y){
            paste(y,collapse = "!")
          }),function(z){
            sd(z)
          })
          name = names(sds)[which(sds==0)]
          for(n in name){
            var1_name = strsplit(n, "!")[[1]][1]
            var2_name = strsplit(n, "!")[[1]][2]

            if(length(factor_name)==2){
              e[p[,factor_name[1]] == var1_name & p[,factor_name[2]] == var2_name,i] = rnorm(length(e[p[,factor_name[1]] == var1_name & p[,factor_name[2]] == var2_name,i]))

            }else{
              e[p[,factor_name[1]] == var1_name,i] = rnorm(length(e[p[,factor_name[1]] == var1_name,i]))

            }

         }
        }
      }

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


      if(sum(table(dta[,c(-1,-ncol(dta))])==0)>0){
        return("Please check sample size for your study design. Cannot have empty class.")
      }else{
        cl = makeCluster(detectCores())
        if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==0 & (length(unique(dta[,2]))>2)){#oneway ANOVA.
          num_factor_variable = length(unique(dta$variable1))
          sudo_matrix = matrix(nrow = num_factor_variable,ncol = num_factor_variable)#helping for cure the format issue with dunnTest().




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




          result = stat_one_way_ANOVA(data = e,data2 = dta,i = 2,sudo_matrix,factor_name,cl,
                                      ANOVAmethod,ANOVAposthoc,nonparaANOVAmethod,nonparaANOVAposthoc)
          f$'outlier_exist?' = stat_test_outlier(e,f,p,factor_name)
          result = data.frame(f,result,check.names = F)
          result = cbind(result,result_stat)

          if(need_power){
            result_power = stat_ANOVA_power(e=e,dta = dta,i=2, sig.level = 0.05, desired_power = desired_power,independent_factor_name, cl)
            result = cbind(result,result_power)
          }

          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!

        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==0 & (length(unique(dta[,2]))==2)){ # t test
          # basic statistics.
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
          f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)



            result = stat_t_test(data = e,data2 = dta,i = 2,cl,
                                 ttestmethod,ttestcorrection, nonparattestmethod,nonparattestcorrection)

            result = data.frame(f,result,check.names = F)

          if(need_power){
            power = stat_t_test_power(e = e,dta=dta, i = 2,sig.level = 0.05, desired_power = desired_power,
                                      independent_factor_name = independent_factor_name,cl)
            result = cbind(result,power)
          }

          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!

        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==2 & length(repeated_factor_name)==0){#two way ANOVA

          num_factor_variable1 = length(unique(dta$variable1))
          num_factor_variable2 = length(unique(dta$variable2))
          sudo_matrix1 = matrix(nrow = num_factor_variable1,ncol = num_factor_variable1)
          sudo_matrix2 = matrix(nrow = num_factor_variable2,ncol = num_factor_variable2)


          parallel = parLapply(cl, 1:ncol(e),function(i,e,dta,ezANOVA,pbad2way){
            dta$value = e[,i]
            interaction_variable1_variable2 = ezANOVA(data = dta, dv = value, wid = id, between = .(variable1,variable2), type = 3)$ANOVA[3,5]
            interaction_variable1_variable2_nonPara = NA
              # pbad2way(value ~variable1*variable2,
                # data = dta,est = "median")$AB.p.value

            return(data.frame(interaction_variable1_variable2,interaction_variable1_variable2=interaction_variable1_variable2_nonPara))
          },e,dta,ezANOVA,pbad2way)

          interaction = do.call("rbind",parallel)
          colnames(interaction) = c(paste("p_value:_Interaction_between", paste(factor_name,collapse = "_and_")),
                                    paste("non_parametric_p_value:_Interaction_between", paste(factor_name,collapse = "_and_")))

          if(!length(unique(dta$variable2))==2){ # if TRUE, then use ANOVA on main effect post hoc analysis.
            variable2_posthoc = stat_one_way_ANOVA(data = e,dta, i=3, sudo_matrix2, factor_name,cl,
                                                   maineffectANOVAmethod2,maineffectANOVAposthocmethod2,nonpara_maineffectANOVAmethod2,nonpara_maineffectANOVAposthocmethod2)
            variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable1==levels(dta$variable1)[1],]
              result = stat_one_way_ANOVA(data = e[dta$variable1==unique(x$variable1),],data2 = x,i=3,sudo_matrix=sudo_matrix2,factor_name,cl,
                                          simplemaineffectANOVAmethod2,simplemaineffectANOVAposthocmethod2,nonpara_simplemaineffectANOVAmethod2,nonpara_simplemaineffectANOVAposthocmethod2) # the 3rd column is group.
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

            variable2_t_test = stat_t_test(data = e,dta, i=3,cl,
                                           maineffectttestmethod2,maineffectttestposthocmethod2, nonpara_maineffectttestmethod2,nonpara_maineffectttestposthocmethod2)

            variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable1=="drug 2",]
              result = stat_t_test(e[dta$variable1==unique(x$variable1),],data2 = x,3,cl,
                                   simplemaineffectttestmethod2,simplemaineffectttestposthocmethod2, nonpara_simplemaineffectttestmethod2,nonpara_simplemaineffectttestposthocmethod2) # the 3rd column is group.
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
            variable1_posthoc = stat_one_way_ANOVA(data = e,dta, i=2, sudo_matrix1, factor_name,cl,
                                                   maineffectANOVAmethod1,maineffectANOVAposthocmethod1,nonpara_maineffectANOVAmethod1,nonpara_maineffectANOVAposthocmethod1)
            variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable2==levels(dta$variable2)[1],]
              result = stat_one_way_ANOVA(data = e[dta$variable2==unique(x$variable2),],data2 = x,i=2,sudo_matrix=sudo_matrix1,factor_name,cl,
                                          simplemaineffectANOVAmethod1,simplemaineffectANOVAposthocmethod1,nonpara_simplemaineffectANOVAmethod1,nonpara_simplemaineffectANOVAposthocmethod1) # the 3rd column is group.
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
            variable1_t_test = stat_t_test(data = e,dta, i=2,cl,
                                           maineffectttestmethod1,maineffectttestposthocmethod1, nonpara_maineffectttestmethod1,nonpara_maineffectttestposthocmethod1)

            variable2_simple_main_effect = by(dta,dta$variable2,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable2=="time 1",]
              result = stat_t_test(e[dta$variable2==unique(x$variable2),],x, i=2,cl,
                                   simplemaineffectttestmethod1,simplemaineffectttestposthocmethod1, nonpara_simplemaineffectttestmethod1,nonpara_simplemaineffectttestposthocmethod1) # 2nd column is group.
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

          result_stat_variable1 = result_stat[,c(1,
                                                 2:(num_factor_variable1+1),
                                                 ((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1):
                                                   ((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1+num_factor_variable1))]

          result_stat_variable2 = result_stat[,c(1,
                                                 (num_factor_variable1+2):(num_factor_variable1+num_factor_variable2+1),
                                                 ((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1),
                                                  (((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1+num_factor_variable1)+1):
                                                   ((((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1+num_factor_variable1)+1)+num_factor_variable2-1))]

          result_stat_inter = result_stat[,c(1,
                                             (num_factor_variable1+num_factor_variable2+2):(num_factor_variable1+num_factor_variable2+num_factor_variable2*num_factor_variable1+1),
                                             ((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1),
                                               ((((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1+num_factor_variable1)+1)+num_factor_variable2-1):
                                               ((((num_factor_variable1+1)+num_factor_variable2+num_factor_variable1*num_factor_variable2+1+num_factor_variable1)+1)+num_factor_variable2-1+
                                                  num_factor_variable2*num_factor_variable1))]
          if(need_power){
          stat_power = stat_two_way_ANOVA_power(e=e,dta=dta,sig.level = 0.05, desired_power = 0.8,independent_factor_name = factor_name, cl)
            result = cbind(result,stat_power)
          }


          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!





        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==3 & length(repeated_factor_name)==0){# three way anova.
          return("Have not done yet!")
        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==1 & (length(unique(dta[,2]))>2)){# one way repeated anova.


          num_factor_variable = length(unique(dta$repeated1))
          sudo_matrix = matrix(nrow = num_factor_variable,ncol = num_factor_variable)#helping for cure the format issue with dunnTest().

          result = stat_one_way_repeated_ANOVA(data = e,data2 = dta,i = 2,sudo_matrix,factor_name,cl,onewaySpher_Corr)
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
            if(need_power){
              result_power = stat_repeated_ANOVA_power(e=e,p=p,f=f,dta=dta,i=2, sig.level = 0.05, desired_power = 0.8,factor_name, epsilon = 1, k = 1, cl)

              result = cbind(result,result_power)
            }

          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!


        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==1 & (length(unique(dta[,2]))==2)){# paired t test
          result = stat_paired_t_test(data = e,data2 = dta,i = 2,cl,
                                      pairedttestFDRmethod,nonpara_pairedttestFDRmethod) # 2nd column is the group
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
          if(need_power){
            result_power = stat_paired_t_test_power(e =e, f = f, p = p, dta =dta, i =2, sig.level = 0.05, desired_power = desired_power, factor_name = repeated_factor_name, cl)

            result = cbind(result,result_power)
          }


          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!




        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==0 & length(repeated_factor_name)==2){# two way repeated anova.


          num_factor_repeated1 = length(unique(dta$repeated1))
          num_factor_repeated2 = length(unique(dta$repeated2))
          sudo_matrix1 = matrix(nrow = num_factor_repeated1,ncol = num_factor_repeated1)
          sudo_matrix2 = matrix(nrow = num_factor_repeated2,ncol = num_factor_repeated2)


          parallel = parLapply(cl, 1:ncol(e),function(i,e,dta,ezANOVA){
            dta$value = e[,i]
            interaction_repeated1_repeated2 = ezANOVA(data = dta, dv = value, wid = id, within = .(repeated1,repeated2), type = 3)$ANOVA[3,5]
            # interaction_repeated1_repeated2_nonPara = pbad2way(value ~repeated1*repeated2,
            #                                                    data = dta,est = "median")$AB.p.value
            interaction_repeated1_repeated2_nonPara = "NA"

            return(data.frame(interaction_repeated1_repeated2,interaction_repeated1_repeated2=interaction_repeated1_repeated2_nonPara))
          },e,dta,ezANOVA)

          interaction = do.call("rbind",parallel)
          colnames(interaction) = c(paste("p_value:_Interaction_between", paste(factor_name,collapse = "_and_")),
                                    paste("non_parametric_p_value:_Interaction_between", paste(factor_name,collapse = "_and_")))

          if(!length(unique(dta$repeated2))==2){ # if TRUE, then use ANOVA on main effect post hoc analysis.


            repeated2_posthoc = stat_one_way_repeated_ANOVA(data = e,data2= dta, i=3, sudo_matrix=sudo_matrix2, factor_name,cl,
                                                            mainSpher_Corr2)
            repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #repeated1 simple main effect
              # x = dta[dta$repeated1==levels(dta$repeated1)[1],]
              result = stat_one_way_repeated_ANOVA(data = e[dta$repeated1==unique(x$repeated1),],data2 = x,i=3,sudo_matrix=sudo_matrix2,factor_name,cl,
                                                   simplemainSpher_Corr2) # the 3rd column is group.
              return(result)
            })
            for(i in 1:length(repeated1_simple_main_effect)){
              colnames(repeated1_simple_main_effect[[i]]) = paste0(levels(dta$repeated1)[i],":_",colnames(repeated1_simple_main_effect[[i]]))
            }
            result_temp = repeated1_simple_main_effect[[1]]
            for(i in 2:length(repeated1_simple_main_effect)){
              result_temp = cbind(result_temp,repeated1_simple_main_effect[[i]])
            }
            repeated1_simple_main_effect = result_temp
          }else{
            # if it is two levels, then no need to do post hoc analysis.

            repeated2_t_test = stat_paired_t_test(data = e,data2 = dta, i=3,cl,
                                                  maineffectpairedttestposthocmethod2,nonpara_maineffectpairedttestposthocmethod2)

            repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #repeated1 simple main effect
              # x = dta[dta$repeated1=="drug 2",]
              result = stat_paired_t_test(data = e[dta$repeated1==unique(x$repeated1),],data2 = x,3,cl,
                                          simplemaineffectpairedttestposthocmethod2,nonpara_simplemaineffectpairedttestposthocmethod2) # the 3rd column is group.
              return(result)
            })# returns two length of list. Ordered by levels of INDICES in by().
            for(i in 1:length(repeated1_simple_main_effect)){
              colnames(repeated1_simple_main_effect[[i]]) = paste0(levels(dta$repeated1)[i],":_",colnames(repeated1_simple_main_effect[[i]]))
            }
            result_temp = repeated1_simple_main_effect[[1]]
            for(i in 2:length(repeated1_simple_main_effect)){
              result_temp = cbind(result_temp,repeated1_simple_main_effect[[i]])
            }
            repeated1_simple_main_effect = result_temp
          }


          if(!length(unique(dta$repeated1))==2){# if TRUE, then use ANOVA on main effect post hoc analysis.
            repeated1_posthoc = stat_one_way_repeated_ANOVA(data = e,dta, i=2, sudo_matrix1, factor_name,cl,
                                                            mainSpher_Corr1)
            repeated2_simple_main_effect = by(dta,dta$repeated2,FUN = function(x){ #repeated1 simple main effect
              # x = dta[dta$repeated2==levels(dta$repeated2)[1],]
              result = stat_one_way_repeated_ANOVA(data = e[dta$repeated2==unique(x$repeated2),],data2 = x,i=2,sudo_matrix=sudo_matrix1,factor_name,cl,
                                                   simplemainSpher_Corr1) # the 3rd column is group.
              return(result)
            })
            for(i in 1:length(repeated2_simple_main_effect)){
              colnames(repeated2_simple_main_effect[[i]]) = paste0(levels(dta$repeated2)[i],":_",colnames(repeated2_simple_main_effect[[i]]))
            }
            result_temp = repeated2_simple_main_effect[[1]]
            for(i in 2:length(repeated2_simple_main_effect)){
              result_temp = cbind(result_temp,repeated2_simple_main_effect[[i]])
            }
            repeated2_simple_main_effect = result_temp
          }else{
            # if it is two levels, then no need to do post hoc analysis.
            repeated1_t_test = stat_paired_t_test(data = e,data2 = dta, i=2,cl,
                                                  maineffectpairedttestposthocmethod1,nonpara_maineffectpairedttestposthocmethod1)

            repeated2_simple_main_effect = by(dta,dta$repeated2,FUN = function(x){ #repeated1 simple main effect
              # x = dta[dta$repeated2=="time 1",]
              result = stat_paired_t_test(e[dta$repeated2==unique(x$repeated2),],x, i=2,cl,
                                          simplemaineffectpairedttestposthocmethod1,nonpara_simplemaineffectpairedttestposthocmethod1) # 2nd column is group.
              return(result)
            })
            for(i in 1:length(repeated2_simple_main_effect)){
              colnames(repeated2_simple_main_effect[[i]]) = paste0(levels(dta$repeated2)[i],":_",colnames(repeated2_simple_main_effect[[i]]))
            }
            result_temp = repeated2_simple_main_effect[[1]]
            for(i in 2:length(repeated2_simple_main_effect)){
              result_temp = cbind(result_temp,repeated2_simple_main_effect[[i]])
            }
            repeated2_simple_main_effect = result_temp
          }


          if(num_factor_repeated1 == 2 & (!num_factor_repeated2 == 2)){
            result = data.frame(interaction,repeated1_t_test,repeated2_posthoc,repeated1_simple_main_effect,repeated2_simple_main_effect,check.names = FALSE)
          }else if(num_factor_repeated2 == 2 & (!num_factor_repeated1 == 2)){
            result = data.frame(interaction,repeated1_posthoc,repeated2_t_test,repeated1_simple_main_effect,repeated2_simple_main_effect,check.names = FALSE)
          }else if((num_factor_repeated1 == 2) & (num_factor_repeated2 == 2)){
            result = data.frame(interaction,repeated1_t_test,repeated2_t_test,repeated1_simple_main_effect,repeated2_simple_main_effect,check.names = FALSE)
          }else{
            result = data.frame(interaction,repeated1_posthoc,repeated2_posthoc,repeated1_simple_main_effect,repeated2_simple_main_effect,check.names = FALSE)
          }


          # result[,-1] = result[,!abs(diff(result[1,]))<0.000000001]
          f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)

          result = data.frame(f,result,check.names = F)


          result_stat = matrix(nrow = ncol(e),ncol = (1 + length(unique(dta$repeated1)) + length(unique(dta$repeated2)) +
                                                        length(unique(dta$repeated1)) * length(unique(dta$repeated2)))*2) # global mean, sd. and mean and sd for each group.
          add = (1+num_factor_repeated1 + num_factor_repeated2 + num_factor_repeated1*num_factor_repeated2)
          for(i in 1:ncol(e)){
            dta$value = e_ori[,i]
            result_stat[i,1] = mean(dta$value,na.rm = T)
            result_stat[i,2:(1+num_factor_repeated1)] = by(dta$value, dta$repeated1,mean,na.rm = T)
            result_stat[i,(1+num_factor_repeated1+1):(1+num_factor_repeated1 + num_factor_repeated2)] =
              by(dta$value, dta$repeated2,mean,na.rm = T)

            result_stat[i,(1+num_factor_repeated1 + num_factor_repeated2+1):
                          (1+num_factor_repeated1 + num_factor_repeated2 + num_factor_repeated1*num_factor_repeated2)] =
              by(dta$value, paste(dta$repeated1,dta$repeated2, sep = "*"),mean,na.rm = T)


            result_stat[i,1+add] = sd(dta$value,na.rm = T)
            result_stat[i,(2:(1+num_factor_repeated1))+add] = by(dta$value, dta$repeated1,sd,na.rm = T)
            result_stat[i,((1+num_factor_repeated1+1):(1+num_factor_repeated1 + num_factor_repeated2))+add] =
              by(dta$value, dta$repeated2,mean,na.rm = T)

            result_stat[i,((1+num_factor_repeated1 + num_factor_repeated2+1):
                             (1+num_factor_repeated1 + num_factor_repeated2 + num_factor_repeated1*num_factor_repeated2))+add] =
              by(dta$value, paste(dta$repeated1,dta$repeated2, sep = "*"),sd,na.rm = T)

          }
          result_stat = data.frame(result_stat,check.names = F)
          colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta$repeated1,mean,na.rm = T))),
                                    paste("Mean of", names( by(dta$value, dta$repeated2,mean,na.rm = T))),
                                    paste("Mean of", names( by(dta$value, paste(dta$repeated1,dta$repeated2, sep = "*"),mean,na.rm = T))),
                                    "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta$repeated1,sd,na.rm = T))),
                                    paste("Standard Deviation of", names( by(dta$value, dta$repeated2,sd,na.rm = T))),
                                    paste("Standard Deviation of", names( by(dta$value, paste(dta$repeated1,dta$repeated2, sep = "*"),sd,na.rm = T)))
          )
          result = cbind(result,result_stat)
          if(need_power){
result$power = "NOT AVAILABLE FOR TWO_WAY REPEATED DESIGN"
          }


          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!




        }else if(length(factor_name[!factor_name%in%repeated_factor_name])==1 & length(repeated_factor_name)==1){# mixed two way anova

          num_factor_variable1 = length(unique(dta$variable1))
          num_factor_repeated1 = length(unique(dta$repeated1))
          sudo_matrix1 = matrix(nrow = num_factor_variable1,ncol = num_factor_variable1)
          sudo_matrix2 = matrix(nrow = num_factor_repeated1,ncol = num_factor_repeated1)


          parallel = parLapply(cl, 1:ncol(e),function(i,e,dta,ezANOVA){
            dta$value = e[,i]
            interaction_variable1_repeated1 = ezANOVA(data = dta, dv = value, wid = id, within = .(repeated1),between = .(variable1), type = 3)$`Sphericity Corrections`[2,3]
            interaction_variable1_repeated1_nonPara = NA

            return(data.frame(interaction_variable1_repeated1,interaction_variable1_repeated1=interaction_variable1_repeated1_nonPara))
          },e,dta,ezANOVA)
         #
          interaction = do.call("rbind",parallel)
          colnames(interaction) = c(paste("p_value:_Interaction_between", paste(factor_name,collapse = "_and_")),
                                    paste("non_parametric_p_value:_Interaction_between", paste(factor_name,collapse = "_and_")))

          if(!length(unique(dta$repeated1))==2){ # if TRUE, then use ANOVA on main effect post hoc analysis.


            repeated1_posthoc = stat_one_way_repeated_ANOVA(data = e,data2= dta, i=3, sudo_matrix=sudo_matrix2, factor_name,cl = cl,
                                                            onewaySpher_Corr = mainSpher_Corr2)
            variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable1==unique(dta$variable1)[2],]
              result = stat_one_way_repeated_ANOVA(data = e[dta$variable1==unique(x$variable1),],
                                                   data2 = x,i=3,sudo_matrix=sudo_matrix2,factor_name,cl=cl,
                                                   simplemainSpher_Corr2) # the 3rd column is group.
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

            repeated1_t_test = stat_paired_t_test(data = e,data2 = dta, i=3,cl,
                                                  maineffectpairedttestposthocmethod2,nonpara_maineffectpairedttestposthocmethod2)

            variable1_simple_main_effect = by(dta,dta$variable1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$variable1=="drug 2",]
              result = stat_paired_t_test(e[dta$variable1==unique(x$variable1),],data2 = x,3,cl,
                                          simplemaineffectpairedttestposthocmethod2,nonpara_simplemaineffectpairedttestposthocmethod2) # the 3rd column is group.
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
            variable1_posthoc = stat_one_way_ANOVA(data = e,dta, i=2, sudo_matrix1, factor_name,cl,
                                                   maineffectANOVAmethod1,maineffectANOVAposthocmethod1,nonpara_maineffectANOVAmethod1,nonpara_maineffectANOVAposthocmethod1)
            repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$repeated1==levels(dta$repeated1)[1],]
              result = stat_one_way_ANOVA(data = e[dta$repeated1==unique(x$repeated1),],data2 = x,i=2,sudo_matrix=sudo_matrix1,factor_name,cl,
                                          simplemaineffectANOVAmethod1,simplemaineffectANOVAposthocmethod1,nonpara_simplemaineffectANOVAmethod1,nonpara_simplemaineffectANOVAposthocmethod1
                                          ) # the 3rd column is group.
              return(result)
            })
            for(i in 1:length(repeated1_simple_main_effect)){
              colnames(repeated1_simple_main_effect[[i]]) = paste0(levels(dta$repeated1)[i],":_",colnames(repeated1_simple_main_effect[[i]]))
            }
            result_temp = repeated1_simple_main_effect[[1]]
            for(i in 2:length(repeated1_simple_main_effect)){
              result_temp = cbind(result_temp,repeated1_simple_main_effect[[i]])
            }
            repeated1_simple_main_effect = result_temp
          }else{
            # if it is two levels, then no need to do post hoc analysis.
            variable1_t_test = stat_t_test(data = e,data2 = dta, i=2,cl,
                                           maineffectttestmethod1, maineffectttestposthocmethod1,
                                           nonpara_maineffectttestmethod1, nonpara_maineffectttestposthocmethod1)

            repeated1_simple_main_effect = by(dta,dta$repeated1,FUN = function(x){ #variable1 simple main effect
              # x = dta[dta$repeated1=="time 1",]
              result = stat_t_test(e[dta$repeated1==unique(x$repeated1),],x, i=2,cl,
                                   simplemaineffectttestmethod1, simplemaineffectttestposthocmethod1,
                                   nonpara_simplemaineffectttestmethod1, nonpara_simplemaineffectttestposthocmethod1) # 2nd column is group.
              return(result)
            })
            for(i in 1:length(repeated1_simple_main_effect)){
              colnames(repeated1_simple_main_effect[[i]]) = paste0(levels(dta$repeated1)[i],":_",colnames(repeated1_simple_main_effect[[i]]))
            }
            result_temp = repeated1_simple_main_effect[[1]]
            for(i in 2:length(repeated1_simple_main_effect)){
              result_temp = cbind(result_temp,repeated1_simple_main_effect[[i]])
            }
            repeated1_simple_main_effect = result_temp
          }


          if(num_factor_variable1 == 2 & (!num_factor_repeated1 == 2)){
            result = data.frame(interaction,variable1_t_test,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect,check.names = FALSE)
          }else if(num_factor_repeated1 == 2 & (!num_factor_variable1 == 2)){
            result = data.frame(interaction,variable1_posthoc,repeated1_t_test,variable1_simple_main_effect,repeated1_simple_main_effect,check.names = FALSE)
          }else if((num_factor_variable1 == 2) & (num_factor_repeated1 == 2)){
            result = data.frame(interaction,variable1_t_test,repeated1_t_test,variable1_simple_main_effect,repeated1_simple_main_effect,check.names = FALSE)
          }else{
            result = data.frame(interaction,variable1_posthoc,repeated1_posthoc,variable1_simple_main_effect,repeated1_simple_main_effect,check.names = FALSE)
          }


          # result[,-1] = result[,!abs(diff(result[1,]))<0.000000001]
          f$'outlier exist?' = stat_test_outlier(e,f,p,factor_name)

          result = data.frame(f,result,check.names = F)


          result_stat = matrix(nrow = ncol(e),ncol = (1 + length(unique(dta$variable1)) + length(unique(dta$repeated1)) +
                                                        length(unique(dta$variable1)) * length(unique(dta$repeated1)))*2) # global mean, sd. and mean and sd for each group.
          add = (1+num_factor_variable1 + num_factor_repeated1 + num_factor_variable1*num_factor_repeated1)
          for(i in 1:ncol(e)){
            dta$value = e_ori[,i]
            result_stat[i,1] = mean(dta$value,na.rm = T)
            result_stat[i,2:(1+num_factor_variable1)] = by(dta$value, dta$variable1,mean,na.rm = T)
            result_stat[i,(1+num_factor_variable1+1):(1+num_factor_variable1 + num_factor_repeated1)] =
              by(dta$value, dta$repeated1,mean,na.rm = T)

            result_stat[i,(1+num_factor_variable1 + num_factor_repeated1+1):
                          (1+num_factor_variable1 + num_factor_repeated1 + num_factor_variable1*num_factor_repeated1)] =
              by(dta$value, paste(dta$variable1,dta$repeated1, sep = "*"),mean,na.rm = T)


            result_stat[i,1+add] = sd(dta$value,na.rm = T)
            result_stat[i,(2:(1+num_factor_variable1))+add] = by(dta$value, dta$variable1,sd,na.rm = T)
            result_stat[i,((1+num_factor_variable1+1):(1+num_factor_variable1 + num_factor_repeated1))+add] =
              by(dta$value, dta$repeated1,mean,na.rm = T)

            result_stat[i,((1+num_factor_variable1 + num_factor_repeated1+1):
                             (1+num_factor_variable1 + num_factor_repeated1 + num_factor_variable1*num_factor_repeated1))+add] =
              by(dta$value, paste(dta$variable1,dta$repeated1, sep = "*"),sd,na.rm = T)

          }
          result_stat = data.frame(result_stat,check.names = F)
          colnames(result_stat) = c("Global Mean", paste("Mean of", names( by(dta$value, dta$variable1,mean,na.rm = T))),
                                    paste("Mean of", names( by(dta$value, dta$repeated1,mean,na.rm = T))),
                                    paste("Mean of", names( by(dta$value, paste(dta$variable1,dta$repeated1, sep = "*"),mean,na.rm = T))),
                                    "Global Standard Deviation", paste("Standard Deviation of", names( by(dta$value, dta$variable1,sd,na.rm = T))),
                                    paste("Standard Deviation of", names( by(dta$value, dta$repeated1,sd,na.rm = T))),
                                    paste("Standard Deviation of", names( by(dta$value, paste(dta$variable1,dta$repeated1, sep = "*"),sd,na.rm = T)))
          )

          result = cbind(result,result_stat)
        if(need_power){
          result_power = stat_mixed_ANOVA_power(e=e,p=p,dta=dta,sig.level = 0.05,desired_power = desired_power,factor_name = factor_name,epsilon=1,cl=cl)

          result = cbind(result,result_power)
        }


          writeLines(jsonlite::toJSON(colnames(result)),"colnames.json")#!!!

        }else{
          return("Your Design is so complicated that we couldn't analysis. If you have any question, please contact Sili: slfan@ucdavis.edu")
        }




        if(length(feature_contain_constant_group)>0){

          for( i in feature_contain_constant_group){

result[i, -c(1:ncol(f))] = "NA"

          }

          writeLines(paste(paste(feature_contain_constant_group,collapse = ","),"th feature contains group that has constant value. No result would be given for that feature."),
                     "messages_hypo_test.txt")

        }else{
          writeLines("Done!",
                     "messages_hypo_test.txt")
        }
        if(nrow(e_withQC)>1){
          result_RSD = stat_QC_RSD(e_before,p_before,f,cl)
          result =cbind(result,result_RSD)
        }
        return(result)

      }






}

