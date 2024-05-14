#### Generate random correlation plot for pulled data ####
#install.packages(reshape)
#install.packages('ggcorrplot')
#install.packages('ggplot2')

library(tidyverse)
library(reshape)
library(RColorBrewer)
library('ggplot2')
library(ggcorrplot)
library('tidyverse')

#function to generate correlations
random_correlations<-function(dataset, dataset_title){
  
  dataset_random<-dataset %>% dplyr::select(all_of(random_sample)) %>% drop_na() %>% 
    filter()
  
  #get sd to remove constants (i.e, for proportion of certain land cover values)
  dataset_sd<-apply(dataset_random, 2, sd) %>% as.data.frame() 
  dataset_sd$sd<-dataset_sd$.
  dataset_sd$var<-row.names(dataset_sd)
  
  dataset_random<-dataset_random %>% dplyr::select(all_of(dataset_sd$var[which(dataset_sd$sd>0)]))
  
  #update names:
  #var_names<-read.csv('variable_names.csv')
  names<-colnames(dataset_random)%>%as.data.frame()
  names<-merge(names, var_names, by.x='.', by.y='variable_names', all.x=TRUE)
  
  #colnames(dataset_random)<-names$name_clean
  
  corr_matrix<-cor(dataset_random %>% drop_na()) %>% as.table() %>% as.data.frame() %>% arrange(Freq)
  corr_matrix<-merge(corr_matrix, var_names, by.x='Var1', by.y='variable_names')%>%dplyr::rename(., Varname1=name_clean)
  corr_matrix<-merge(corr_matrix, var_names, by.x='Var2', by.y='variable_names')%>%dplyr::rename(., Varname2=name_clean)
  
  corr_plot<-ggplot(data=corr_matrix, aes(x=Varname1, y=Varname2, fill=Freq)) + 
    geom_tile(color='white')+
    scale_fill_gradient2(low='#9E0142', high='#5E4FA2', mid='#FDFEBD', midpoint=0,limit=c(-1,1), space='Lab', 
                         name='Correlation\nCoefficient')+theme_minimal()+
    theme(axis.text.x=element_text(angle=45, vjust=1, 
                                   size=8, hjust=1),
          axis.title = element_blank())
  return(corr_plot)
  
}


acmt_correlations<-function(){
# get list of data files
files <- list.files(path="~/workspace/ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE)

if(length(files)==0){
  data_correlations<-data.frame(total_n='No data pulled')
}

if(length(files)>0){

#pull all data into list
  dataset_full<-lapply(files, function(x){
    t<-read.csv(x, header=TRUE, row.names = NULL)
  })

    #merge all data frames in list
  dataset_full_merge<-dataset_full%>%reduce(full_join, by=c('id'))
  
  
  #choose random variables to check correlation
  variable_names<-colnames(dataset_full_merge%>%dplyr::select(-id))
  set.seed(1233)
  random_sample<-sample(variable_names, 40)

  random_correlations(dataset_full_merge)
}
}

