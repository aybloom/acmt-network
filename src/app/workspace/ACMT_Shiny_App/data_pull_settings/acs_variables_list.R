### Get ACS variable Lists ####
acs_years<-c(2010:2020)

for(y in 1:length(acs_years)){
  year<-acs_years[y]
  acs_variables<-load_variables(year, 'acs5', cache=TRUE)
  acs_variables$year<-year
  
  if(y==1){
    acs_variables_full<-acs_variables%>%dplyr::select(name, label, concept, year)
  }
  
  if(y>1){
    (acs_variables_full<-rbind(acs_variables_full, acs_variables%>%dplyr::select(name, label, concept, year)))}
}

##add interpolation
acs_variables_full<-acs_variables_full %>% 
  mutate(acs_variable_name_to_interpolate_by_sum_boolean_mapping=ifelse(grepl('median', label) | grepl('mean', label), FALSE, TRUE))

update_acs_var_list<-function(years=acs_years){
  acs_var_list<-acs_variables_full %>% filter(year %in% years)%>%
    dplyr::select(name, label, concept)
}

acsVarInputList<-update_acs_var_list(years=acs_years)

