#### Correlation Analyses ##### ------------------------------------------------
#### Get full list of years, radii, and dataset names

all_names_of_data<-function(){
  files <- list.files(path="ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE)
  
  dataset_names_list<-lapply(files, function(x) {
    gsub("\\..*", "", str_replace(x, '(.*?)dataset_(.*?)', '')) # get dataset name
  }) %>% unlist()
}

dataset_names<-all_names_of_data()

all_years_of_data<-function(dataset_names_list=dataset_names){
  files<-sapply(dataset_names_list, function(x){list.files(path="ACMT_Shiny_App/data_pull_measures", pattern = x,  full.names=TRUE)})
  print(files)
  if(length(files)==0){
  year_list<-c()
  }
  else{
  #files <- list.files(path="ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE) %>% grepl(any_of(dataset_names_list), files)
  year_list<-reduce(lapply(files, function(x) {
    y<-read.csv(x, header=TRUE)%>%mutate(dataset_name=str_extract(x,"(?<=dataset_).*?(?=\\.)")) %>% dplyr::select(year, dataset_name)%>%unique()}
  ), rbind) 
  year_list<-year_list %>% dplyr::select(year)%>% unique() %>% arrange(year)
  }
  return(year_list)
}

all_radii_of_data<-function(dataset_names_list=dataset_names){
  files<-sapply(dataset_names_list, function(x){list.files(path="ACMT_Shiny_App/data_pull_measures", pattern = x,  full.names=TRUE)})
  print(files)
  if(length(files)==0){
    year_list<-c()
  }
  else{
  # files <- list.files(path="ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE)
  radius_list<-reduce(lapply(files, function(x) {
      y<-read.csv(x, header=TRUE) %>% mutate(dataset_name=str_extract(x,"(?<=dataset_).*?(?=\\.)")) %>% dplyr::select(radius, dataset_name)%>%unique()}
  ), rbind) 
  radius_list<-radius_list %>% dplyr::select(radius) %>% unique()%>% arrange(radius)
  }
  return(radius_list)
}

# year_list<-all_years_of_data()
# radius_list<-all_radii_of_data()


### Import & combine Datasets ##### ----------------------------------------------
combine_files<-function(years=years, radius_meters=radius_meters, data_names=data_names){

  #create list of chosen data files
  full_data_names<-lapply(data_names,function(x){
    paste0('ACMT_Shiny_App/data_pull_measures/dataset_', x, '.csv')
  })
  #filter full file list to subset list
  files <- list.files(path="ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE)
  files<-files[files %in% full_data_names]
  
  #combine the list of file names
  data_summary<-lapply(files, function(x) {
  t <- read.csv(x, header=TRUE) %>% filter(radius %in% radius_meters & year %in% years)%>% 
    dplyr::select(-any_of('X'), id, everything()) 
  id_list<-t$id
    t<-t %>%t() %>% data.frame() 
    colnames(t)<-id_list
    t<-t %>%
      mutate(measure_name=rownames(.)) %>% dplyr::select(measure_name, everything())
    return(t)
})

data_full_summary<-reduce(data_summary, rbind.fill) %>% filter(!(measure_name %in% c('radius', 'year', 'X', 'id')))%>% t() %>% data.frame() %>% row_to_names(row_number = 1) %>%
  mutate(id=rownames(.)) %>% dplyr::select(id, everything()) %>% janitor::remove_empty(which='cols')

return(data_full_summary)
}

### examine random sets of correlations ####
random_correlations<-function(corr_number=5, years=c(2015,2017), radius_meters=1000, dataset_names=dataset_names_list){
  
  if(!is.null(dataset_names)){
  #Combine datasets:
  dataset<-combine_files(years=years, radius_meters = radius_meters, data_names=dataset_names)
  
  #set color palette
  corPalette<-colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')))
  sc<-scale_fill_gradientn(colors=corPalette(100), limits=c(-1, 1))
  
  variable_names<-colnames(dataset[-1])
  print(variable_names)
  #set.seed(1233)
  random_sample<-sample(variable_names, corr_number, replace=TRUE)
  print(random_sample)
  
  dataset_random<-dataset %>% dplyr::select(all_of(random_sample)) %>% drop_na()

  #get sd to remove constants (i.e, for proportion of certain land cover values)
  dataset_sd<-apply(dataset_random, 2, sd) %>% as.data.frame() 
  dataset_sd$sd<-dataset_sd$.
  dataset_sd$var<-row.names(dataset_sd)
  
  dataset_random<-dataset_random %>% dplyr::select(all_of(dataset_sd$var[which(dataset_sd$sd>0)]))
  
  #update names:
# var_names<-random_sample
# names<-colnames(dataset_random)%>%as.data.frame()
# names<-merge(names, var_names, by.x='.', by.y='variable_names', all.x=TRUE)
  
# colnames(dataset_random)<-names$name_clean
  
  corr_matrix<-cor(dataset_random %>% drop_na()%>%mutate_all(as.numeric))%>% as.table() %>% as.data.frame() %>% arrange(Freq)
#  corr_matrix<-merge(corr_matrix, var_names, by.x='Var1', by.y='variable_names')%>%dplyr::rename(., Varname1=name_clean)
#  corr_matrix<-merge(corr_matrix, var_names, by.x='Var2', by.y='variable_names')%>%dplyr::rename(., Varname2=name_clean)
  
  # corr_plot<-corr_matrix %>% ggplot(aes(x=Var1, y=Var2, fill=Freq)) + geom_tile() +
  #   theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))+sc
  
  corr_plot<-ggcorrplot::ggcorrplot(cor(dataset_random%>%mutate_all(as.numeric)), 
                                    #method='circle', 
                                    #hc.order=TRUE, 
                                    lab=FALSE, lab_size=2.5, colors=c("#9E0142", "#FDFEBD", '#5E4FA2'), 
                                    title="Random Correlations") + theme(axis.text.x=element_text(angle=45, size=8), 
                                                                 axis.text.y=element_text(size=8))
  
  return(corr_plot)
  }
}

#TEST#
#data_full_summary<-combine_files(years=c(2010:2020), radius_meters=c(1000), data_names=c('walk', 'mrfei'))
#random_correlations(dataset=data_full_summary, corr_number = 5)
