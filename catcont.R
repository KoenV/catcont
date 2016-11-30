library(Hmisc)
library(pander)
#format_pval

catcont = function(list_of_variables=c(),data=c(),group=c(),which.group=c(),con.test=c()){
  #require(Hmisc)
  ## list to save info on all variables (length is number of considerd variables)
  seperate_info = vector('list',length(list_of_variables))
  if (group==FALSE){
    for(i in 1:length(list_of_variables)){
  # group is none ---------------------------------------
    if(is.numeric(data[,list_of_variables[i]]) | is.integer(data[,list_of_variables[i]])){
      ## Continuous --------------------------------------------------------------
        TemplateMatrix_OneGroup_Continuous = matrix(c(''),5,3)
        TemplateMatrix_OneGroup_Continuous[,2] = c('','N','mean (std)','median (IQR)','range')
        colnames(TemplateMatrix_OneGroup_Continuous) = c('variable','statistic','')
        seperate_info[[i]] = TemplateMatrix_OneGroup_Continuous
          # calculate the statistics 
          seperate_info[[i]][1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
          seperate_info[[i]][2,3] = length(data[,list_of_variables[i]])
          seperate_info[[i]][3,3] = paste00(round(mean(data[,list_of_variables[i]],na.rm=TRUE),2),'[',round(sd(data[,list_of_variables[i]],na.rm=TRUE),2),']') 
          seperate_info[[i]][4,3] = paste00(median(data[,list_of_variables[i]]),'[',paste0(round(quantile(data[,list_of_variables[i]],c(.25),na.rm=TRUE),2),','
                                                                                        ,round(quantile(data[,list_of_variables[i]],c(.75),na.rm=TRUE),2)),']')
          seperate_info[[i]][5,3] = paste0(range(data[,list_of_variables[i]],na.rm=TRUE)[1],',',range(data[,list_of_variables[i]],na.rm=TRUE)[2]) 
        }    
   
    if(is.factor(data[,list_of_variables[i]]) | is.character(data[,list_of_variables[i]]) ){
      ## Discrete --------------------------------------------------------------
        TemplateMatrix_OneGroup_Discrete = matrix(c(''),nlevels(data[,list_of_variables[i]])+1,3)
        TemplateMatrix_OneGroup_Discrete[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        TemplateMatrix_OneGroup_Discrete[2:(nlevels(data[,list_of_variables[i]])+1),2] = 'n/N (%)'
        colnames(TemplateMatrix_OneGroup_Discrete) = c('variable','statistic','')
      
        seperate_info[[i]] = TemplateMatrix_OneGroup_Discrete
        for(level in 1:nlevels(data[,list_of_variables[i]])){
          data[,'count_dummy'] = ifelse(data[,list_of_variables[i]]==levels(data[,list_of_variables[i]])[level],1,0)
          seperate_info[[i]][level+1,1] = levels(data[,list_of_variables[i]])[level]
          seperate_info[[i]][level+1,3] = paste0(sum(data[,'count_dummy']),'/',nrow(data),'(',(sum(data[,'count_dummy'])/nrow(data))*100,'%',')')
          }
        }
      }
      full_table = do.call("rbind", seperate_info)
      return(knitr::kable(full_table))
    }
  if (group==TRUE){
    for(i in 1:length(list_of_variables)){
    ## Continuous --------------------------------------------------------------
    if(is.numeric(data[,list_of_variables[i]]) | is.integer(data[,list_of_variables[i]])){
      TemplateMatrix_MultipleGroups_Continuous = matrix(c(''),5,nlevels(data[,which.group])+3)
      colnames(TemplateMatrix_MultipleGroups_Continuous) = c('variable','statistic','total',levels(data[,which.group]))
      TemplateMatrix_MultipleGroups_Continuous[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
      TemplateMatrix_MultipleGroups_Continuous[2:5,2] = c('N','mean (std)','median (IQR)','range')
      seperate_info[[i]]=TemplateMatrix_MultipleGroups_Continuous
      # total information
      seperate_info[[i]][2,3] = length(data[,list_of_variables[i]])
      seperate_info[[i]][3,3] = paste0(round(mean(data[,list_of_variables[i]],na.rm=TRUE),2),'[',round(sd(data[,list_of_variables[i]],na.rm=TRUE),2),']') 
      seperate_info[[i]][4,3] = paste0(median(data[,list_of_variables[i]]),'[',paste0(round(quantile(data[,list_of_variables[i]],c(.25),na.rm=TRUE),2),',',
                                                                                    round(quantile(data[,list_of_variables[i]],c(.75),na.rm=TRUE),2)),']')
      seperate_info[[i]][5,3] = paste0(range(data[,list_of_variables[i]],na.rm=TRUE)[1],',',range(data[,list_of_variables[i]],na.rm=TRUE)[2]) 
      
      # seperate group information
      for(ngroup in 1:nlevels(data[,which.group])){
        data_subset = subset(data,data[,which.group]==levels(data[,which.group])[ngroup]) # take subset on which.group levels
        
        seperate_info[[i]][2,3+ngroup] = length(data_subset[,list_of_variables[i]])
        seperate_info[[i]][3,3+ngroup] = paste0(round(mean(data_subset[,list_of_variables[i]],na.rm=TRUE),2),'[',round(sd(data_subset[,list_of_variables[i]],na.rm=TRUE),2),']') 
        seperate_info[[i]][4,3+ngroup] = paste0(median(data_subset[,list_of_variables[i]]),'[',paste0(round(quantile(data_subset[,list_of_variables[i]],c(.25),na.rm=TRUE),2),',',
                                                                                                    round(quantile(data_subset[,list_of_variables[i]],c(.75),na.rm=TRUE),2)),']')
        seperate_info[[i]][5,3+ngroup] = paste0(range(data_subset[,list_of_variables[i]],na.rm=TRUE)[1],',',range(data_subset[,list_of_variables[i]],na.rm=TRUE)[2]) 
        
      }
      if(con.test==TRUE){
        # overal test
        Template_OveralTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),1)
        colnames(Template_OveralTest) = 'Overall'
        Template_OveralTest[1,1] = 'p.val'
        Template_OveralTest[2,1] = format_pval(kruskal.test(data[,list_of_variables[1]] ~ data[,which.group])$p.value)
        
        
        Template_ContinuousTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),
                                         nlevels(data[,which.group])*(nlevels(data[,which.group])-1)/2)
        colnames(Template_ContinuousTest) = letters[1:ncol(Template_ContinuousTest)]
        
        testnr=1
        for(k in 1:(length(levels(data[,which.group]))-1)){
          for(j in 2:length(levels(data[,which.group]))){
            if(k<j){
              colnames(Template_ContinuousTest)[testnr] = paste0('[',levels(data[,which.group])[k],' vs.',
                                                                 levels(data[,which.group])[j],']')
              
              Template_ContinuousTest[1,testnr] = 'AUC (p.val)'
              
              data_subset = subset(data,data[,which.group] %in% levels(data[,which.group])[c(k,j)]) # take subset on which.group levels
              
              Template_ContinuousTest[2,testnr] = paste0(round(auc(droplevels(data_subset[,which.group]), data_subset[,list_of_variables[1]]),2),
                                                         '(',
                                                         format_pval(wilcox.test(data_subset[,list_of_variables[1]] ~ droplevels(data_subset[,which.group]))$p.value),
                                                         ')')
              testnr =  testnr + 1
            }
          }
        }
      }
      seperate_info[[i]] = cbind(seperate_info[[i]],Template_OveralTest,Template_ContinuousTest)
      
    }
    ## Discrete --------------------------------------------------------------
      if(is.factor(data[,list_of_variables[i]]) | is.character(data[,list_of_variables[i]])){
        TemplateMatrix_MultipleGroups_Discrete = matrix(c(''),nlevels(data[,list_of_variables[i]])+1,nlevels(data[,which.group])+3)
        TemplateMatrix_MultipleGroups_Discrete[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        TemplateMatrix_MultipleGroups_Discrete[2:(nlevels(data[,list_of_variables[i]])+1),2] = 'n/N (%)'
        colnames(TemplateMatrix_MultipleGroups_Discrete) = c('variable','statistic','total',c(levels(data[,which.group])))
        seperate_info[[i]] = TemplateMatrix_MultipleGroups_Discrete
        
        
        for(ngroup in 1:nlevels(data[,which.group])){
          data_subset = subset(data,data[,which.group]==levels(data[,which.group])[ngroup]) # take subset on which.group levels
          
          for(level in 1:nlevels(data[,list_of_variables[i]])){
            data[,'count_dummy'] = ifelse(data[,list_of_variables[i]]==levels(data[,list_of_variables[i]])[level],1,0)
            
            seperate_info[[i]][level+1,1] = levels(data_subset[,list_of_variables[i]])[level]
            
            seperate_info[[i]][level+1,3] = paste0(sum(data[,'count_dummy']),'/',nrow(data),'(',round((sum(data[,'count_dummy'])/nrow(data))*100,2),'%',')')
            
            data_subset[,'count_dummy'] = ifelse(data_subset[,list_of_variables[i]]==levels(data_subset[,list_of_variables[i]])[level],1,0)
            seperate_info[[i]][level+1,3+ngroup] = paste0(sum(data_subset[,'count_dummy']),'/',nrow(data_subset),'(',round((sum(data_subset[,'count_dummy'])/nrow(data_subset))*100,2),'%',')')
          }
        }
      }
    }
  }
  full_table = do.call("rbind", seperate_info)
  knitr::kable(full_table,caption = 'Summary table', digits = 2)
}





########################
catcont(list_of_variables=c('leeftijd','Gewicht.kg'),
        data=tonsil_work,
        group=TRUE,
        which.group=c('condition.cat'),
        con.test = TRUE)





list_of_variables=c('leeftijd')
data=tonsil_work
group=TRUE
which.group=c('gender.cat')
TemplateMatrix_MultipleGroups_Continuous = matrix(c(''),5,nlevels(data[,which.group])+3)
con.test=TRUE
i = 1
#con.test = logical







if(con.test==TRUE){
  # overal test
  Template_OveralTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),1)
  colnames(Template_OveralTest) = 'Overall'
  Template_OveralTest[1,1] = 'p.val'
  Template_OveralTest[2,1] = format_pval(kruskal.test(data[,list_of_variables[1]] ~ data[,which.group])$p.value)
  
  
  Template_ContinuousTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),
                                   nlevels(data[,which.group])*(nlevels(data[,which.group])-1)/2)
  colnames(Template_ContinuousTest) = letters[1:ncol(Template_ContinuousTest)]
  
  testnr=1
  for(k in 1:(length(levels(data[,which.group]))-1)){
    for(j in 2:length(levels(data[,which.group]))){
      if(k<j){
        colnames(Template_ContinuousTest)[testnr] = paste0('[',levels(data[,which.group])[k],' vs.',
                                                           levels(data[,which.group])[j],']')
        
        Template_ContinuousTest[1,testnr] = 'AUC (p.val)'
        
        data_subset = subset(data,data[,which.group] %in% levels(data[,which.group])[c(k,j)]) # take subset on which.group levels
        
        Template_ContinuousTest[2,testnr] = paste0(round(auc(droplevels(data_subset[,which.group]), data_subset[,list_of_variables[1]]),2),
                                                   '(',
                                                   format_pval(wilcox.test(data_subset[,list_of_variables[1]] ~ droplevels(data_subset[,which.group]))$p.value),
                                                   ')')
        testnr =  testnr + 1
      }
    }
  }
}
cbind(Template_OveralTest,Template_ContinuousTest)







