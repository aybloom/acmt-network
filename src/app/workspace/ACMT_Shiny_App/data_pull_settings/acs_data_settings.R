#### ACS Data Settings ####

## ============= Using the default ACS variables list ========================== ####
#set ACS variable list
##set up data pull specs:

get_default_acs_var_list<-function(year=year){
  
  #import ACS list based on the year
  if(year==2010){
    acs_columns<-read.csv('ACMT/ACSColumns2010.csv')
  }
  if(year==2011){
    acs_columns<-read.csv('ACMT/ACSColumns2011.csv')
  }
  if(year>2011 & year<2019){
    acs_columns<-read.csv('ACMT/ACSColumns2012_thru_18.csv')
  }
  if(year==2019){
    acs_columns <- read.csv("ACMT/ACSColumns2019.csv")
  }
  if(year==2020){
    acs_columns<-read.csv('ACMT/ACSColumns2020.csv')
  }
  
  ##create 'count' versions of each variable name and 'proportion' versions for each #ACS variable where applicable
  acs_count_names<-paste(acs_columns$var_name, "count", sep="_")
  if (length(acs_columns$var_name[acs_columns$universe_col != ""]) == 0) {   # prevent having something that is exactly "_proportion"
    acs_proportion_names <- character(0)
  } else {
    acs_proportion_names <- paste(acs_columns$var_name[acs_columns$universe_col !=''], "proportion", sep="_")   # only non-universal variables have proportions
  }
  
  #save file as ACSColumns file
  write.csv(acs_columns, '~/workspace/ACMT/ACSColumns.csv')
  
  names_of_variables_to_get<-c(acs_count_names, acs_proportion_names)
  return(names_of_variables_to_get)
  
}

acs_years<-c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
acs_selected_years<-c(2017)

acs_description<-'The American Community Survey (ACS) is a national survey that provides sociodemographic information. In the 5-year estimates, data is pooled across 5 years of surveying to minimize measurement error.
We have generated a list of ACS variables to be pulled for each participant in Inspace across three buffered areas (500m, 1000m, and 5000m). 
ACS variables will be pulled for 2017 (2013-2017 5-year ACS) as well as for the 5-year period centered around your initial year of enrollment.'


##Upload list of ACS variables for custom data pull ## -------------------------------

# preview of data formatting:
example_acs_vars<-data.frame(acs_col=c('variable code', 'B01001_001', 'B01001_002', 'B01001_026', 'B25058_001'), 
                             var_name=c('variable name', 'total_pop', 'total_men', 'total_women', 'median_rent'), 
                             universe_col = c('denominator for calculating proportions', '', 'B01001_001', 'B01001_001', ''), 
                             acs_variable_name_to_interpolate_by_sum_boolean_mapping = c('whether to sum (TRUE) or average (FALSE) the estimate', TRUE, TRUE, TRUE, FALSE))


# get_custom_acs_var_list<-function(){
#   acs_columns<-read.csv('ACMT/ACSColumns.csv')
#   
#   ##create 'count' versions of each variable name and 'proportion' versions for each #ACS variable where applicable
#   acs_count_names<-paste(acs_columns$var_name, "count", sep="_")
#   if (length(acs_columns$var_name[acs_columns$universe_col != ""]) == 0) {   # prevent having something that is exactly "_proportion"
#     acs_proportion_names <- character(0)
#   } else {
#     acs_proportion_names <- paste(acs_columns$var_name[acs_columns$universe_col !=''], "proportion", sep="_")   # only non-universal variables have proportions
#   }
#   
#   names_of_variables_to_get<-c(acs_count_names, acs_proportion_names)
#   return(names_of_variables_to_get)
# }
# 
# # generate a full list of default ACS variables # ------------------------------------
# ACSVarListNames<-c('acs_col', 'var_name', 'universe_col', 'pretty_name_count', 'pretty_name_proportion', 'acs_variable_name_to_interpolate_by_sum_boolean_mapping', 'year')
# full_acs<-rbind(read.csv('ACMT/ACSColumns2010.csv')%>%mutate(year='2010')%>%dplyr::select(ACSVarListNames), 
#                 read.csv('ACMT/ACSColumns2011.csv')%>%mutate(year='2011')%>%dplyr::select(ACSVarListNames), 
#                 read.csv('ACMT/ACSColumns2012_thru_18.csv')%>%mutate(year='2012-2018')%>%dplyr::select(ACSVarListNames), 
#                 read.csv('ACMT/ACSColumns2019.csv')%>%mutate(year='2019')%>%dplyr::select(ACSVarListNames), 
#                 read.csv('ACMT/ACSColumns2020.csv')%>%mutate(year='2020')%>%dplyr::select(ACSVarListNames))
# 
# write.csv(full_acs, 'ACMT/full_default_acs_list.csv')
