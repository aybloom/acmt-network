## Sidewalk z-scores ##

sidewalk_zscores<-function(dataset_sidewalk){
  dataset_sidewalk<-dataset_sidewalk%>%
    mutate(prop_sidewalk = total_sidewalk/total_num, 
           prop_crosswalk=total_crosswalk/total_num)
  
  #calculate mean prop_sidewalk and mean prop_crosswalk
  mean.sidewalk=mean(dataset_sidewalk$prop_sidewalk, na.rm = TRUE)
  mean.crosswalk=mean(dataset_sidewalk$prop_crosswalk, na.rm=TRUE)
  sd.sidewalk=sd(dataset_sidewalk$prop_sidewalk, na.rm=TRUE)
  sd.crosswalk=sd(dataset_sidewalk$prop_crosswalk, na.rm=TRUE)
  
  #calculate sidewalk and crosswalk z-scores
  dataset_sidewalk<- dataset_sidewalk%>%
    mutate(sidewalk_z=(prop_sidewalk-mean.sidewalk)/sd.sidewalk, 
           crosswalk_z=(prop_crosswalk-mean.crosswalk)/sd.crosswalk)
  
}

dataset_sidewalk<-sidewalk_zscores(read.csv('~/workspace/Inspace/data_pull_measures/dataset_sidewalk.csv')%>%
                                     dplyr::select(id, year, radius, total_num, total_crosswalk, total_sidewalk, prop_sidewalk, prop_crosswalk))

write.csv(dataset_sidewalk,'~/workspace/Inspace/data_pull_measures/dataset_sidewalk.csv', row.names = FALSE)
