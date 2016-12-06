##########################################################################################################################
# Function; boxplotAUC - boxplot function with AUC, CI, and p-value
# date: 3/12/2016
###########################################################################################################################
# dependencies
library(ggplot2); theme_set(theme_bw())
library(pROC)
library(Hmisc)

# function
boxplotAUC = function(cat=c(),con=c(),data=c(),conf.level = .95){
  # function to round p.values
  format_pval <- function(x){
    if (x < .001) return('p <.001')
    else paste0('p = ',round(x, 3))   # 3 = no. of digits to round p value to if .001 < p < .250.
  }

  AUC = round(pROC::ci(data[,cat],data[,con], conf.level=conf.level),2)
  wilcox = wilcox.test(data[,con] ~ data[,cat])
  text = paste('AUC =',AUC[2],',' ,paste0(conf.level,'CI'),paste0('[',AUC[1],';',AUC[3],']'),',',format_pval(wilcox$p.value))
  max =  max(data[,con])
  labels_list =  list(y = ifelse(label(data[,con])=='',con,label(data[,con])),
                    x = ifelse(label(data[,cat])=='',cat,label(data[,cat])))
  
  data =  data.frame(con=as.numeric(data[,con]),cat=data[,cat])
    ggplot(data=data) + geom_boxplot(aes(cat,con)) + geom_text(aes(label=text,y=(max*1.1),x=1.5),size=5) + labs(labels_list) + theme(axis.title.x=element_blank())
      
}




# ## example
# random.Data = data.frame(cat.var = sample(c('a','b'),100,replace = TRUE),
#                          cont.var = rnorm(100))
# 
# #e e.g., change label name of a variable
# label(random.Data$cat.var) 'categorial variable'
# 
# # use function
# boxplotAUC(con = 'cont.var', cat = 'cat.var', data=random.Data) 


boxplotAUC(cat=c('condition.cat'),con = c('verblijfsduur_Min'),data=tonsil_work)

