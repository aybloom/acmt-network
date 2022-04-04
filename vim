diff --git a/src/app/workspace/GeocoderACMT.R b/src/app/workspace/GeocoderACMT.R
index 5a26b7d..9208f7c 100644
--- a/src/app/workspace/GeocoderACMT.R
+++ b/src/app/workspace/GeocoderACMT.R
@@ -208,8 +208,25 @@ get_acs_results_for_available_variables <- function (acs_var_names, state, count
   return(acs_results)
 }
 
-acs_variable_name_to_interpolate_by_sum_boolean_mapping <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
-names(acs_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_026", "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B25001_001", "B05012_002", "B05012_003", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006", "B06008_001", "B06008_002", "B06008_003", "B06008_004", "B06008_005", "B06008_006", "B06008_007", "B06008_013", "B07201_002", "B07201_004", "B07201_014", "B08006_001", "B08006_003", "B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_016", "B08006_017", "B08302_002", "B08302_003", "B08302_004", "B08302_005", "B08302_006", "B08302_007", "B08302_008", "B08302_009", "B08302_010", "B08302_011", "B08302_012", "B08302_013", "B08302_014", "B08302_015", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025", "B15002_002", "B15002_011", "B15002_015", "B15002_019", "B15002_028", "B15002_032")
+acs_variable_name_to_interpolate_by_sum_boolean_mapping <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
+                                                             TRUE, TRUE, TRUE, TRUE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE ,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE, FALSE,TRUE,TRUE,TRUE,TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
+                                                             TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
+
+names(acs_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_026", "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B25001_001", "B05012_002", "B05012_003", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006", "B06008_001", "B06008_002", "B06008_003", "B06008_004", "B06008_005", "B06008_006", "B06008_007", "B06008_013", "B07201_002", "B07201_004", "B07201_014", "B08006_001", "B08006_003", "B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_016", "B08006_017", "B08302_002", "B08302_003", "B08302_004", "B08302_005", "B08302_006", "B08302_007", "B08302_008", "B08302_009", "B08302_010", "B08302_011", "B08302_012", "B08302_013", "B08302_014", "B08302_015", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025", "B15002_002", "B15002_011", "B15002_015", "B15002_019", "B15002_028", "B15002_032", 
+                                                                    "B01001H_001", "B03002_001", "B03002_004", "B03002_001", "B06009_001", "B06009_002", "B06012_001", "B06012_002", "B06012_003", "B11012_001", "B11012_008", "B11012_010", "B11012_014", "B11012_015", "B16005_001", "B16005_004", "B16005_007", "B16005_008", "B16005_009", "B16005_012", "B16005_013", "B16005_014", "B16005_017", "B16005_018", "B16005_019", "B16005_022", "B16005_023", "B16005_024", "B16005_026", "B16005_029", "B16005_030", "B16005_031", "B16005_034", "B16005_035",
+                                                                    "B16005_036", "B16005_039", "B16005_040", "B16005_041", "B16005_044", "B16005_045", "B17001_001",  "B17001_002", "B17012_001", "B17012_002", "B17023_001", "B17023_002", "B18101_001", "B18101_002", 
+           "B18101_003", "B18101_004", "B18101_006", "B18101_007", "B18101_009", "B18101_010", "B18101_012", "B18101_013", "B18101_015", "B18101_016", "B18101_018", "B18101_019", "B18101_021", "B18101_022", "B18101_023", "B18101_025", "B18101_026", 
+           "B18101_028", "B18101_029", "B18101_031", "B18101_032", "B18101_034", "B18101_035", "B18101_037", "B18101_038", "B19001_001", "B19001_002", "B19001_003", "B19013_001", "B19054_001", "B19054_002", "B19058_001", "B19058_002", "B23025_001", "B23025_005", "B25002_001", "B19001_004", "B25003_001",
+           "B25003_002", "B25014_001", "B25014_002", "B25014_005", "B25014_006", "B25014_007", "B25014_008", "B25014_011", "B25014_012", "B25014_013", "B25016_001", "B25016_002", "B25016_011", "B25024_001", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25038_002", 
+           "B19001_005",  "B25038_009", "B19001_006", "B25043_001", "B25043_002", "B25043_007", "B25002_003", "B25043_016", "B25044_001", "B25044_002", "B25044_003", "B25044_009", "B25044_010", "B25070_001", "B25070_010", "B25077_001", "B25091_001", "B25091_002", "B25091_011", 
+           "B25091_013", "B25043_011", 
+           "B25091_022", "B25129_001", "B25129_002", "B25129_003", "B25129_038", "B25129_039", "B26001_001", "B19001_011", "B19001_012", "B19001_013", "B19001_014", "B19001_015", "B19001_016", "B19001_017", "B25064_001", "B25088_002",
+           "B06012_001", "B06012_002", "B06012_003", "B25038_007", "B25038_008", "B25038_014", "B25038_015", "B25043_011", 
+           "C24030_002", "C24030_018", "C24030_019", "C24030_029", "C24030_045", "C24030_046", "C24060_001", "C24060_002", "A17002A_001", "A17002A_007", 
+           "B23001_009", "B23001_016", "B23001_023", "B23001_030", "B23001_037", "B23001_044", "B23001_051", "B23001_058", "B23001_065", "B23001_072", "B23001_077", "B23001_082", "B23001_087", "B23001_002", 
+           "B23001_003", "B23001_010", "B23001_017", "B23001_024", "B23001_031", "B23001_038", "B23001_045", "B23001_052", "B23001_059", "B23001_066", "B23001_073", "B23001_078", "B23001_083")
+          
+
 
 get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names=NULL, year=year, external_data=NULL, geoid_type = "Census Tract", fill_missing_GEOID_with_zero=FALSE, use_lower_resolution_geo_data=FALSE, variable_name_to_interpolate_by_sum_boolean_mapping=NULL, return_point_estimate=FALSE, custom_buffer=NULL) {  # count_results might not have the variable measures for all GEOIDs in census tracts, in that case, use 0 for the measure; if this is not done, the returned result will be NA
 
diff --git a/src/files/ACSColumns.csv b/src/files/ACSColumns.csv
index 4162a46..2afabcd 100644
--- a/src/files/ACSColumns.csv
+++ b/src/files/ACSColumns.csv
@@ -1 +1,278 @@
-var_name,acs_col,universe_col,pretty_name_count,pretty_name_proportion
\ No newline at end of file
+acs_col,var_name,universe_col,pretty_name_count,pretty_name_proportion,acs_variable_name_to_interpolate_by_sum_boolean_mapping
+B23001_009,males_16_19_not_in_labor_force,B23001_003,Total males ages 16 to 19 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 16 to 19 with labor force determined),TRUE
+B23001_016,males_20_21_not_in_labor_force,B23001_010,Total males ages 20 to 21 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 20 to 21 with labor force determined),TRUE
+B23001_023,males_22_24_not_in_labor_force,B23001_017,Total males ages 22 to 24 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 22 to 24 with labor force determined),TRUE
+B23001_030,males_25_29_not_in_labor_force,B23001_024,Total males ages 25 to 29 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 25 to 29 with labor force determined),TRUE
+B23001_037,males_30_34_not_in_labor_force,B23001_031,Total males ages 30 to 34 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 033 to 34 with labor force determined),TRUE
+B23001_044,males_35_44_not_in_labor_force,B23001_038,Total males ages 35 to 44 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 35 to 44 with labor force determined),TRUE
+B23001_051,males_45_54_not_in_labor_force,B23001_045,Total males ages 45 to 54 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 45 to 54 with labor force determined),TRUE
+B23001_058,males_55_59_not_in_labor_force,B23001_052,Total males ages 55 to 59 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 55 to 59 with labor force determined),TRUE
+B23001_065,males_60_61_not_in_labor_force,B23001_059,Total males ages 60 to 61 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 60 to 61 with labor force determined),TRUE
+B23001_072,males_62_64_not_in_labor_force,B23001_066,Total males ages 62 to 64 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 62 to 64 with labor force determined),TRUE
+B23001_077,males_65_69_not_in_labor_force,B23001_073,Total males ages 65 to 69 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 65 to 69 with labor force determined),TRUE
+B23001_082,males_70_74_not_in_labor_force,B23001_078,Total males ages 70 to 74 not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 70 to 74 with labor force determined),TRUE
+B23001_087,males_75_older_not_in_labor_force,B23001_083,Total males ages 75 and older not in the labor force (count),Percent of total males not in the labor force (proportion of males ages 75 and older with labor force determined),TRUE
+B23001_002,total_males_16_over_labor_force_det,,"Total males 16 and older, labor force involvement determined (count)",,TRUE
+B01001_001,total_pop,,Total Population (count),,TRUE
+B01001_002,men,B01001_001,Male residents (count),Males (proportion of population),TRUE
+B01001_003,males_under_5,B01001_002,Male residents under 5 (count),Male residents under 5 (proportion of all male residents),TRUE
+B01001_004,males_5_to_9,B01001_002,Male residents 5 to 9 (count),Male residents 5 to 9 (proportion of all male residents),TRUE
+B01001_005,males_10_to_14,B01001_002,Male residents 10 to 14 (count),Male residents 10 to 14 (proportion of all male residents),TRUE
+B01001_006,males_15_to_17,B01001_002,Male residents 15 to 17 (count),Male residents 15 to 17 (proportion of all male residents),TRUE
+B01001_007,males_18_to_19,B01001_002,Male residents 18 to 19 (count),Male residents 18 to 19 (proportion of all male residents),TRUE
+B01001_008,males_20_to_24,B01001_002,Male residents 20 to 24 (count),Male residents 20 to 24 (proportion of all male residents),TRUE
+B01001_009,males_25_to_29,B01001_002,Male residents 25 to 29 (count),Male residents 25 to 29 (proportion of all male residents),TRUE
+B01001_010,males_30_to_34,B01001_002,Male residents 30 to 34 (count),Male residents 30 to 34 (proportion of all male residents),TRUE
+B01001_011,males_35_to_44,B01001_002,Male residents 35 to 44 (count),Male residents 35 to 44 (proportion of all male residents),TRUE
+B01001_012,males_45_to_54,B01001_002,Male residents 45 to 54 (count),Male residents 45 to 54 (proportion of all male residents),TRUE
+B01001_013,males_55_to_64,B01001_002,Male residents 55 to 64 (count),Male residents 55 to 64 (proportion of all male residents),TRUE
+B01001_014,males_65_to_74,B01001_002,Male residents 65 to 74 (count),Male residents 65 to 74 (proportion of all male residents),TRUE
+B01001_015,males_75_to_84,B01001_002,Male residents 75 to 84 (count),Male residents 75 to 84 (proportion of all male residents),TRUE
+B01001_016,males_85_and_older,B01001_002,Male residents 85 and older (count),Male residents 85 and older (proportion of all male residents),TRUE
+B01001_026,women,B01001_001,Female residents (count),Female residents (proportion of population),TRUE
+B01001_027,females_under_5,B01001_026,Female residents under 5 (count),Female residents under 5 (proportion of all female residents),TRUE
+B01001_028,females_5_to_9,B01001_026,Female residents 5 to 9 (count),Female residents 5 to 9 (proportion of all female residents),TRUE
+B01001_029,females_10_to_14,B01001_026,Female residents 10 to 14 (count),Female residents 10 to 14 (proportion of all female residents),TRUE
+B01001_030,females_15_to_17,B01001_026,Female residents 15 to 17 (count),Female residents 15 to 17 (proportion of all female residents),TRUE
+B01001_031,females_18_to_19,B01001_026,Female residents 18 to 19 (count),Female residents 18 to 19 (proportion of all female residents),TRUE
+B01001_032,females_20_to_24,B01001_026,Female residents 20 to 24 (count),Female residents 20 to 24 (proportion of all female residents),TRUE
+B01001_033,females_25_to_29,B01001_026,Female residents 25 to 29 (count),Female residents 25 to 29 (proportion of all female residents),TRUE
+B01001_034,females_30_to_34,B01001_026,Female residents 30 to 34 (count),Female residents 30 to 34 (proportion of all female residents),TRUE
+B01001_035,females_35_to_44,B01001_026,Female residents 35 to 44 (count),Female residents 35 to 44 (proportion of all female residents),TRUE
+B01001_036,females_45_to_54,B01001_026,Female residents 45 to 54 (count),Female residents 45 to 54 (proportion of all female residents),TRUE
+B01001_037,females_55_to_64,B01001_026,Female residents 55 to 64 (count),Female residents 55 to 64 (proportion of all female residents),TRUE
+B01001_038,females_65_to_74,B01001_026,Female residents 65 to 74 (count),Female residents 65 to 74 (proportion of all female residents),TRUE
+B01001_039,females_75_to_84,B01001_026,Female residents 75 to 84 (count),Female residents 75 to 84 (proportion of all female residents),TRUE
+B01001_040,females_85_and_older,B01001_026,Female residents 85 and older (count),Female residents 85 and older (proportion of all female residents),TRUE
+B01001H_001,non_hisp_white,B01001_001,Non-hispanic white residents (count),Percent of non-hispanic white residents (proportion of all residents),TRUE
+B02001_002,white_alone,B01001_001,White residents (count),White residents (proportion of all residents),TRUE
+B02001_003,black_alone,B01001_001,Black residents (count),Black residents (proportion of all residents),TRUE
+B02001_004,americanindian_or_alaskanative_alone,B01001_001,American Indian or Alaska Native residents (count),American Indian or Alaska Native residents (proportion of all residents),TRUE
+B02001_005,asian_alone,B01001_001,Asian residents (count),Asian residents (proportion of all residents),TRUE
+B02001_006,pacific_islander_alone,B01001_001,Pacific islander residents (count),Pacific islander residents (proportion of all residents),TRUE
+B02001_007,other_race_alone,B01001_001,Other race residents (count),Other race residents (proportion of all residents),TRUE
+B02001_008,two_or_more_races,B01001_001,Two or more race residents (count),Two or more race residents (proportion of all residents),TRUE
+B03002_001,total_pop_ethnicity,,"Total population, ethnicity determined",,TRUE
+B03002_004,nh_black_alone,B03002_001,Non-Hispanic black residents,Percent of non-Hispanic Black residents (proportion of all residents),TRUE
+B05001_002,citizen_born_in_states,,Citizens born in US (count),,TRUE
+B05001_003,citizen_born_in_territories,,Citizens born in US Territories (count),,TRUE
+B05001_004,citizen_born_abroad,,Citizens born abroad (count),,TRUE
+B05001_005,citizen_naturalized,,Naturalized citizens (count),,TRUE
+B05001_006,non_citizen,,Non-citizens (count),,TRUE
+B05012_002,native,,Native born residents (count),,TRUE
+B05012_003,foreign_born,,Foreign born residents (count),,TRUE
+B06008_001,pop_15_and_over,,Residents aged 15 and older (count),,TRUE
+B06008_002,never_married,B06008_001,Never-married residents aged 15 and older (count),Never-married residents aged 15 and older (proportion of residents aged 15 and older),TRUE
+B06008_003,married_not_separated,B06008_001,Married not separated residents (count),Married not separated residents (proportion of residents aged 15 and older),TRUE
+B06008_004,divorced,B06008_001,Divorced residents (count),Divorced residents (proportion of residents aged 15 and older),TRUE
+B06008_005,separated,B06008_001,Married separated residents (count),Married separated residents (proportion of residents aged 15 and older),TRUE
+B06008_006,widowed,B06008_001,Windowed residents (count),Windowed residents (proportion of residents aged 15 and older),TRUE
+B06008_007,born_in_state,B06008_001,Residents born in current state (count),Residents born in current state (proportion of residents aged 15 and older),TRUE
+B06008_013,born_in_other_state,B06008_001,Residents born in another state (count),Residents born in another state (proportion of residents aged 15 and older),TRUE
+B06009_001,pop_25_above,,Population 25 years and older (count),,TRUE
+B06009_002,no_hsdiploma,B06009_001,Residents (25+) with less than hs education (count),Proportion of residents (ages 25+) with less than a high school diploma (proportion of all residents 25 years and older),TRUE
+B06012_001,total_pop_poverty_level_deter,,Total population for whom poverty level is determined (count),,TRUE
+B06012_002,pop_below_100_poverty_threshold,B06012_001,Population below 100% of the poverty level (count),Percent of population below 100% of the poverty level (proportion of total population for whome poverty status is determined),TRUE
+B06012_003,pop_100_to_149_poverty_threshold,B06012_001,Population at 100 to 149 percent of the poverty level (count,Percent of population at 100 to 149 percent of the poverty level (proportion of total populatoin for whom poverty status is determined),TRUE
+B07201_002,same_house_last_year,,People living in the same house they lived last year (count),,TRUE
+B07201_004,moved_within_msa,,People moved within the same metropolitan statistical area (count),,TRUE
+B07201_014,moved_from_abroad,,People moved from abroad (count),,TRUE
+B08006_001,workers_over_15,,Workers aged 15 and older (count),,TRUE
+B08006_003,drive_alone_commuters,B08006_001,Drive alone commuters (count),Drive alone commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_004,carpool_commuters,B08006_001,Carpool commuters (count),Carpool commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_008,transit_commuters,B08006_001,Transit commuters (count),Transit commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_014,bike_commuters,B08006_001,Bicycle commuters (count),Bicycle commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_015,walk_commuters,B08006_001,Walking commuters (count),Walking commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_016,other_commuters,B08006_001,Other mode commuters (count),Other mode commuters (proportion of employed residents aged 15 and older),TRUE
+B08006_017,work_at_home,B08006_001,Work at home (count),Work at home (proportion of employed residents aged 15 and older),TRUE
+B08302_002,start_commute_before_5am,B08006_001,Start commute before 5 am (count),Start commute before 5 am (proportion of employed residents aged 15 and older),TRUE
+B08302_003,commute_start_5_530,B08006_001,Start commute between 5:00 and 5:30 am (count),Start commute between 5:00 and 5:30 am (proportion of employed residents aged 15 and older),TRUE
+B08302_004,commute_start_530_6,B08006_001,Start commute between 5:30 and 6:00 am (count),Start commute between 5:30 and 6:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_005,commute_start_6_630,B08006_001,Start commute between 6:00 and 6:30 am (count),Start commute between 6:00 and 6:30 am (proportion of employed residents aged 15 and older),TRUE
+B08302_006,commute_start_630_7,B08006_001,Start commute between 6:30 and 7:00 am (count),Start commute between 6:30 and 7:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_007,commute_start_7_730,B08006_001,Start commute between 7:00 and 7:30 am (count),Start commute between 7:00 and 7:30 am (proportion of employed residents aged 15 and older),TRUE
+B08302_008,commute_start_730_8,B08006_001,Start commute between 7:30 and 8:00 am (count),Start commute between 7:30 and 8:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_009,commute_start_8_830,B08006_001,Start commute between 8:00 and 8:30 am (count),Start commute between 8:00 and 8:30 am (proportion of employed residents aged 15 and older),TRUE
+B08302_010,commute_start_830_9,B08006_001,Start commute between 8:30 and 9:00 am (count),Start commute between 8:30 and 9:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_011,commute_start_9_10,B08006_001,Start commute between 9:00 and 10:00 am (count),Start commute between 9:00 and 10:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_012,commute_start_10_11,B08006_001,Start commute between 10:00 and 11:00 am (count),Start commute between 10:00 and 11:00 am (proportion of employed residents aged 15 and older),TRUE
+B08302_013,commute_start_11_12,B08006_001,Start commute between 11:00 am and 12:00 pm (count),Start commute between 11:00 am and 12:00 pm (proportion of employed residents aged 15 and older),TRUE
+B08302_014,commute_start_12_4pm,B08006_001,Start commute between 12:00 and 4:00 pm (count),Start commute between 12:00 and 4:00 pm (proportion of employed residents aged 15 and older),TRUE
+B08302_015,commute_start_4pm_on,B08006_001,Start commute between after 4:00 pm (count),Start commute between after 4:00 pm (proportion of employed residents aged 15 and older),TRUE
+B11012_001,total_households,,Total households (count),,TRUE
+B11012_008,living_alone_female,B11012_001,"Female householder, no spouse or partner (count)",Percent of female householders with no spouse or partner (proportin of all households),TRUE
+B11012_010,female_head_kids,B11012_001,"Female households, no spouse or partner, with own children under 18 (count)","Percent of female householders, no spouse or partner, with own children under 18 (percent of households)",TRUE
+B11012_014,living_alone_male,B11012_001,Male householder living alone (count),Percent of males living alone (Proportion of all households),TRUE
+B11012_015,male_householder_kids,B11012_001,"Male householder, no spouse or partner, with kids under 18 years (count)","Percent of male householder, no spouse or partner, with kids under 18 years (proportion of all households)",TRUE
+B15002_002,male_pop_25_and_over,,Male Residents aged 25 and older (count),,TRUE
+B15002_011,male_high_school_grad,B15002_002,Male residents aged 25 and older who graduated from high school (count),Male residents aged 25 and older who graduated from high school (proportion of male residents aged 25 and older),TRUE
+B15002_015,male_bachelors_degree,B15002_002,Male residents aged 25 and older with a bachelor's degree (count),Male residents aged 25 and older with a bachelor's degree (proportion of male residents aged 25 and older),TRUE
+B15002_019,female_pop_25_and_over,,Male Residents aged 25 and older (count),,TRUE
+B15002_028,female_high_school_grad,B15002_019,Male residents aged 25 and older who graduated from high school (count),Male residents aged 25 and older who graduated from high school (proportion of male residents aged 25 and older),TRUE
+B15002_032,female_bachelors_degree,B15002_019,Male residents aged 25 and older with a bachelor's degree (count),Male residents aged 25 and older with a bachelor's degree (proportion of male residents aged 25 and older),TRUE
+B15003_001,pop_25_and_over,,Residents aged 25 and older (count),,TRUE
+B15003_002,no_education,B15003_001,Residents with no education (count),Residents with no education (proportion of residents aged 25 and older),TRUE
+B15003_003,pre_school,B15003_001,Residents with pre-school education (count),Residents with pre-school education (proportion of residents aged 25 and older),TRUE
+B15003_004,kindergarten,B15003_001,Residents with kingdergarten education (count),Residents with kingdergarten education (proportion of residents aged 25 and older),TRUE
+B15003_005,first_grade,B15003_001,Residents with first grade education (count),Residents with first grade education (proportion of residents aged 25 and older),TRUE
+B15003_006,second_grade,B15003_001,Residents with second grade education (count),Residents with second grade education (proportion of residents aged 25 and older),TRUE
+B15003_007,third_grade,B15003_001,Residents with third grade education (count),Residents with third grade education (proportion of residents aged 25 and older),TRUE
+B15003_008,fourth_grade,B15003_001,Residents with fourth grade education (count),Residents with fourth grade education (proportion of residents aged 25 and older),TRUE
+B15003_009,fifth_grade,B15003_001,Residents with fifth grade education (count),Residents with fifth grade education (proportion of residents aged 25 and older),TRUE
+B15003_010,sixth_grade,B15003_001,Residents with sixth grade education (count),Residents with sixth grade education (proportion of residents aged 25 and older),TRUE
+B15003_011,seventh_grade,B15003_001,Residents with seventh grade education (count),Residents with seventh grade education (proportion of residents aged 25 and older),TRUE
+B15003_012,eighth_grade,B15003_001,Residents with eighth grade education (count),Residents with eighth grade education (proportion of residents aged 25 and older),TRUE
+B15003_013,ninth_grade,B15003_001,Residents with ninth grade education (count),Residents with ninth grade education (proportion of residents aged 25 and older),TRUE
+B15003_014,tenth_grade,B15003_001,Residents with tenth grade education (count),Residents with tenth grade education (proportion of residents aged 25 and older),TRUE
+B15003_015,eleventh_grade,B15003_001,Residents with eleventh grade education (count),Residents with eleventh grade education (proportion of residents aged 25 and older),TRUE
+B15003_016,twelfth_grade_no_diploma,B15003_001,Residents with twelfth grade education (count),Residents with twelfth grade education (proportion of residents aged 25 and older),TRUE
+B15003_017,high_school_grad,B15003_001,Residents with high school graduation education (count),Residents with high school graduation education (proportion of residents aged 25 and older),TRUE
+B15003_018,ged_or_alt_diploma,B15003_001,Residents with GED or Alt diploma (count),Residents with GED or Alt diploma (proportion of residents aged 25 and older),TRUE
+B15003_019,some_college_less_than_1_year,B15003_001,Residents with some college education of less than 1 year (count),Residents with some college education of less than 1 year (proportion of residents aged 25 and older),TRUE
+B15003_020,some_college_1_year_or_more,B15003_001,Residents with some college education of greater or equal to 1 year (count),Residents with some college education of greater or equal to 1 year (proportion of residents aged 25 and older),TRUE
+B15003_021,associates_degree,B15003_001,Residents with associates degrees (count),Residents with associates degrees (proportion of residents aged 25 and older),TRUE
+B15003_022,bachelors_degree,B15003_001,Residents with bachelors degrees (count),Residents with bachelors degrees (proportion of residents aged 25 and older),TRUE
+B15003_023,masters_degree,B15003_001,Residents with masters degrees (count),Residents with masters degrees (proportion of residents aged 25 and older),TRUE
+B15003_024,professional_degree,B15003_001,Residents with professional degrees (count),Residents with professional degrees (proportion of residents aged 25 and older),TRUE
+B15003_025,doctoral_degree,B15003_001,Residents with doctoral degrees (count),Residents with doctoral degrees (proportion of residents aged 25 and older),TRUE
+B16005_001,total_ages_5_up,,Population 5 years and over,,TRUE
+B16005_004,native_born_spanish,,Native born persons (ages 5+) who speak Spanish (count),,TRUE
+B16005_007,ntv_sp_lmt_eng_notwell,B16005_004,Native-born persons (age 5+) who speak Spanish and who spead English 'not well' (count),Percent of native-born persons (age 5+) who speak Spanish and speak English 'not well' (proportion of all native-born persons (ages 5) who speak Spanish),TRUE
+B16005_008,ntv_sp_lmt_eng_notatall,B16005_004,Native-born persons (age 5+) who speak Spanish and who speak English 'not at all' (count),Percent of native-born persons (age 5+) who speak Spanish and speak English 'not at all' (proportion of all native-born persons (ages 5+) who speak Spanish),TRUE
+B16005_009,native_born_indo_europe,,Native born persons (ages 5+) who speak Indo-European languages (count),,TRUE
+B16005_012,ntv_ie_lmt_eng_notwell,B16005_009,Native-born persons (age 5+) who speak Indo-European languages and speak English not well (count),Percent of native born persons (age 5+) who speak Indo-European languages and speak English 'not well' (proportion of all persons (ages 5+) who speak Indo-European languages),TRUE
+B16005_013,ntv_ie_lmt_eng_notatall,B16005_009,Native-born persons (age 5+) who speak Indo-European languages and speak English not at all (count),Percent of native born persons (age 5+) who speak Indo-European languages and speak English 'not at all' (proportion of all persons (ages 5+) who speak Indo-European languages),TRUE
+B16005_014,native_born_asian_pi,,Native born persons (ages 5+) who speak Asian and Pacific Islander languages (count),,TRUE
+B16005_017,ntv_api_lmt_en_notwell,B16005_014,Native-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not well' (count),Percent of native born persons (age 5+) who speak Asian and Pacific Islander languages and speak English 'not well' (proportion of all native born persons (ages 5+) who speak Asian and Pacific Islander languages),TRUE
+B16005_018,ntv_api_lmt_en_notatall,B16005_014,Native-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not at all' (count),Percent of native-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not at all' (proportion of native-born persons (ages 5+) who speak Asian and Pacific Islander languages),TRUE
+B16005_019,native_born_other,,Native born persons (ages 5+) who speak other languages (count),,TRUE
+B16005_022,ntv_oth_lmt_en_notwell,B16005_019,Native-born persons (age 5+) who speak other languages and speak English 'not well' (count),"Percent , native-born persons (age 5+) who speak other languages and speak English 'not well' (proportion of native-born persons (ages 5+) who speak other languages)",TRUE
+B16005_023,ntv_oth_lmt_en_notatall,B16005_019,Native-born persons (age 5+) who speak other languages and speak English 'not at all' (count),Percent of native-born persons (age 5+) who speak other languages and speak English 'not at all' (proportin of native-born persons (ages 5+) who speak other languages),TRUE
+B16005_024,total_foreign_born,B16005_001,Total foreign born population ages 5+ (count),Percent of foreign born persons (proportion of all resdients ages 5 and older),TRUE
+B16005_026,total_fb_spanish,B16005_024,Total foreign born population (ages 5+) who speak Spanish (count),Percent of foreign born persons (age 5+) who speak spanish (proportion of all foreign born population ages 5+),TRUE
+B16005_029,fb_sp_lmt_eng_notwell,B16005_026,Foreign-born persons (age 5+) who speak Spanish and who spead English 'not well' (count),Percent of foreign-born persons (age 5+) who speak Spanish and who spead English 'not well' (proportion of foreign-born persons (ages 5+) who speak Spanish),TRUE
+B16005_030,fb_sp_lmt_eng_notatall,B16005_026,Foreign-born persons (age 5+) who speak Spanish and who speak English 'not at all' (count),Percent of foreign-born persons (age 5+) who speak Spanish and who speak English 'not at all' (proportion of foreign-born persons (ages 5+) who speak Spanish),TRUE
+B16005_031,total_tb_indo_europe,B16005_024,Total foreign born population (ages 5+) who speak Indo-Eropean languages (count),Percent of foreign born persons (age 5+) who speak Indo-Eropean languages (proportion of all foreign born population),TRUE
+B16005_034,fb_ie_lmt_eng_notwell,B16005_031,Foreign-born persons (age 5+) who speak Indo-European languages and speak English not well (count),Percent of foreign-born persons (age 5+) who speak Indo-European languages and speak English 'not well' (proportion of foreign-born persons (ages 5+) who speak Indo-European languages),TRUE
+B16005_035,fb_ie_lmt_eng_notatall,B16005_031,Foreign-born persons (age 5+) who speak Indo-European languages and speak English not at all (count),Percent of foreign-born persons (age 5+) who speak Indo-European languages and speak English 'not at all' (Proportion of foreign-born persons who speak Indo-European languages),TRUE
+B16005_036,total_fb_asian_pi,B16005_024,Total foreign born population (ages 5+) who speak Asian and Pacific Island languages (count),Percent of total foreign born population (ages 5+) who speak Asian and Pacific Island languages (Proportio nof all foreign born population ages 5+),TRUE
+B16005_039,fb_api_lmt_en_notwell,B16005_036,Foreign-born persons (age 5+) who spead Asian and Pacific Island languages and speak English 'not well' (count),Percent of foreign-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not well' (proportion of foreign-born persons (ages 5+) who speak Asian and Pacific Islander languages),TRUE
+B16005_040,fb_api_lmt_en_notatall,B16005_036,Foreign-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not at all' (count),Percent of foreign-born persons (age 5+) who speak Asian and Pacific Island languages and speak English 'not at all' (proportion of foreign-born persons (ages 5+) who speak Asian and Pacific Islander languages),TRUE
+B16005_041,total_fb_other,B16005_024,Total foreign born population (ages 5+) who speak other languages (count),Percent of total foreign born population (ages 5+) who speak other languages (Proportio nof all foreign born population ages 5+),TRUE
+B16005_044,fb_oth_lmt_en_notwell,B16005_041,Foreign-born persons (age 5+) who speak other languages and speak English 'not well' (count),Percent of foreign-born persons (age 5+) who speak other languages and speak English 'not well' (proportion of foreign-born persons (ages 5+) who speak other languages),TRUE
+B16005_045,fb_oth_lmt_en_notatall,B16005_041,Foreign-born persons (age 5+) who speak other languages and speak English 'not at all' (count),Percent of foreign-born persons (age 5+) who speak other languages and speak English 'not at all' (proportion of foreign-born persons (ages 5+) who speak other languages),TRUE
+B17001_001,total_poverty_determined,,"Total population, poverty status is determined (count)",,TRUE
+B17001_002,below_pov,B17001_001,Residents with income in the past 12 months below poverty level (Count),Percent of residents with income in the past 12 months below poverty level (Proportion of all Residents for whom poverty status is determined),TRUE
+B17012_001,households,,"Total number of households, poverty status determined",,TRUE
+B17012_002,households_in_poverty,B17012_001,Households in poverty (count),Percent of households in poverty (proportion of all households),TRUE
+B17023_001,total_pop_poverty,,"Total population of families, poverty status is determined (count)",,TRUE
+B17023_002,fam_below_poverty,B17023_001,Families below the povery level (count),Percent of families below the povery level (proportion of all families),TRUE
+B18101_001,civilian_pop,,Total civilian uninstitutionalized population (count),,TRUE
+B18101_002,non_inst_males,,Civilian uninstitutionalized males (count),Percent of civilian uninstutionalized males (proportion of all civilian uninstitutionalized population),TRUE
+B18101_003,non_inst_males_under_5,B18101_002,Civilian uninstitutionalized males ages under 5 (count),Percent of noninstitutionalized males under 5 (proportion of all noninstitutionalized males),TRUE
+B18101_004,males_under5_disability,B18101_003,Males under 5 with a disability (count),Percent of males under 5 with a disability (proportion of all males under 5),TRUE
+B18101_006,non_inst_males_5_to_17,B18101_002,Civilian uninstitutionalized males ages 5 to 17 (count),Percent of civilian noninstitutionalized males ages 5 to 17 (proportion of all noninstitutionalized males),TRUE
+B18101_007,males_5_17_disability,B18101_006,Males 5-17 with a disability (count),Percent of males 5-17 with a disability (proportion of all males 5-17),TRUE
+B18101_009,non_inst_males_18_to_34,B18101_002,Civilian uninstitutionalized males ages 18 to 34 (count),Percent of civilian noninstitutionalized males ages 18 to 34 (proportion of all noninstitutionalized males),TRUE
+B18101_010,males_18_34_disability,B18101_009,Males 18-34 with a disability (count),Percent of males 18-34 with a disability (proportion of all males 18-34),TRUE
+B18101_012,non_inst_males_35_to_64,B18101_002,Civilian uninstitutionalized males ages 35 to 64 (count),Percent of civilian noninstitutionalized males ages 35 to 64 (proportion of all noninstitutionalized males),TRUE
+B18101_013,males_35_64_disability,B18101_012,Males 35-64 with a disability (count),Percent of males 35-64 with a disability (proportion of all males 35-64),TRUE
+B18101_015,non_inst_males_65_to_74,B18101_002,Civilian uninstitutionalized males ages 65 to 74 (count),Percent of civilian noninstitutionalized males ages 65 to 74 (proportion of all noninstitutionalized males),TRUE
+B18101_016,males_65_74_disability,B18101_015,Males 65-74 with a disability (count),Percent of males 65-74 with a disability (proportion of all males 65-74),TRUE
+B18101_018,non_inst_males_75_and_older,B18101_002,Civilian uninstitutionalized males ages 75 and older (count),Percent of civilian noninstitutionalized males ages 75 and older (proportion of all noninstitutionalized males),TRUE
+B18101_019,males_75_plus_disability,B18101_018,Males 75 and older with a disability (count),Percent of males 75 and older with a disability (proportion of all males 75 and older),TRUE
+B18101_021,females_pop,B18101_001,Civilian uninstitutionalized females (count),Percent of civilian uninstitutionalized females (proportion of all civilian unstinstitutionalized persons),TRUE
+B18101_022,noninst_females_under_5,B18101_021,Total civilian uninstitutionalized female population under 5 years (count),Percent of civilian uninstitutionalized females under 5 (proportion of all civilian uninstutionalized persons),TRUE
+B18101_023,females_under5_disability,B18101_022,Females under 5 with a disability (count),Percent of females under 5 with a disability (Proportion of all females under 5),TRUE
+B18101_025,females_5_to_17,B18101_021,Total civilian uninstitutionalized female population ages 5 to 17 (count),Percent of total civilian uninstitutionalized female population ages 5 to 17 (proportion of all uninstitutionalized civilian females),TRUE
+B18101_026,females_5_17_disability,B18101_025,Females 5-17 with a disability (count),Percent of females 5-17 with a disability (proportion of all uninstitutionalized civilian females 5 to 17),TRUE
+B18101_028,females_18_to_34,B18101_021,Total civilian uninstitutionalized female population ages 18 to 34 (count),Percent of total civilian uninstitutionalized female population ages 18 to 34 (proportion of total civilian uninstitutionalized population),TRUE
+B18101_029,females_18_34_disability,B18101_028,Females 18-34 with a disability (count),Percent of females 18-34 with a disability (proportion of all females 18 to 34),TRUE
+B18101_031,females_35_to_64,B18101_021,Total civilian uninstitutionalized female population ages 35 to 64 (count),Percent of total civilian uninstitutionalized female population ages 35 to 64 (proportion of total civilian uninstitutionalized population),TRUE
+B18101_032,females_35_64_disability,B18101_031,Females 35-64 with a disability (count),Percent of females 35-64 with a disability (proportion of all females 35 to 64),TRUE
+B18101_034,noninst_females_65_to_74,B18101_021,Total civilian uninstitutionalized female population ages 65 to 74 (count),,TRUE
+B18101_035,females_65_74_disability,B18101_034,Females 65-74 with a disability (count),Percent of females 65-74 with a disability (proportion of all females 65-74),TRUE
+B18101_037,females_75_older,B18101_021,Total civilian uninstitutionalized female population ages 75 and older (count),,TRUE
+B18101_038,females_75_plus_disability,B18101_037,Females 75 and older with a disability (count),Percent of females 75 and older with a disability (proporption of all females 75 and older),TRUE
+B19001_001,households_income_determined,,"Total housedholds, income determined",,TRUE
+B19001_002,hhincome_less_than_10k,B19001_001,"Households with income of less than 10,000 (count)",,TRUE
+B19001_003,household_income_10_14k,B19001_001,"Households earning $10,000 to 14,999 (count)","Percent of households earning $10,000 to $14,999 (proportion of all housedholds)",TRUE
+B19001_004,houseohld_income_15_19k,B19001_001,"Households earning $15,000 to 19,999 (count)","Percent of households earning $15,000 to $19,999 (proportion of all households)",TRUE
+B19001_005,household_income_20_24k,B19001_001,"Households earning $20,000 to 24,999 (count)","Percent of households earning $20,000 to $24,999 (proportion of all households)",TRUE
+B19001_006,household_income_25_29k,B19001_001,"Households earning $25,000 to $29,999 (count)","Percent of households earning $25,000 to $29,999 (proportion of all households)",TRUE
+B19001_011,hhincome_50k_to_59k,B19001_001,"Households with income of 50,000 to 59,999 (count)","Percent of households earning $50,000 to 59,999 (proportion of all households)",TRUE
+B19001_012,hhincome_60k_to_74k,B19001_001,"Households with income of 60,000 to 74,999 (count)","Percent of households earning $60,000 to 74,999 (proportion of all households)",TRUE
+B19001_013,hhincome_75k_to_99k,B19001_001,"Households with income of 75,000 to 99,999 (count)","Percent of households earning $75,000 to $99,999 (proportion of all households)",TRUE
+B19001_014,hhincome_100k_to_124k,B19001_001,"Households with income of 100,000 to 124,999 (count)","Percent of households earning $100,000 to 124,999 (proportion of all households)",TRUE
+B19001_015,hhincome_125k_to_149k,B19001_001,"Households with income of 125,000 to 149,999(count)","Percent of households earning $125,000 to 149,999 (proportion all households)",TRUE
+B19001_016,hhincome_150k_to_199k,B19001_001,"Households with income of 150,000 to 199,999 (count)","Percent of households earning $150,000 to 199,999 (proportion of all households)",TRUE
+B19001_017,hhincome_200k_or_more,B19001_001,"Households with income of 200,000 or more (count)","Percent of households earning $200,000 (proportion of all households)",TRUE
+B19013_001,med_hincome,,Median Household Income (dollars),,FALSE
+B19054_001,total_receive_idr,,"Total population, receipt of interest, dividends, or net rental income is determined (count)",,TRUE
+B19054_002,receive_idr,B19054_001,"Households receiving dividends, interest, or rental income (count)","Percent of households receiving dividends, interest, or rental income (Proportion of all households)",TRUE
+B19058_001,total_public_asst,,"Total population, public assistance status is determined",,TRUE
+B19058_002,public_asst,B19058_001,Households receiving public assistance (count),Percent of households receiving public assistance (proportion of all households),TRUE
+B23025_001,age_16_plus,,Residents 16 years and older (count),,TRUE
+B23025_005,unemployed,B23025_001,Residents 16 and older who are unemployed (count),Percent of residents 16 and older who are unemployed (proportion of all residents 16 and over),TRUE
+B25001_001,housing_units,,Housing units (count),,TRUE
+B25002_001,total_housing_units_occ,,Housing units (count),,TRUE
+B25002_003,vacant_units,B25002_001,Housing units that are vacant,Percent of housing units that are vacant (proportion of all housing units),TRUE
+B25003_001,occupied_units,B25002_001,Housing units that are occupied (count),Percent of housing units that are occupied (Proportion of all housing units),TRUE
+B25003_002,owner_occupied_units,B25003_001,Housing units that are owner-occupied (count),Percent of housing units that are owner-occupied (proportion of all occupied housing units),TRUE
+B25014_001,total_occupied_housing_units_room,,"Occupied housing units, rooms determined (count)",,TRUE
+B25014_002,total_owner_occ_housing_units_rooms,B25014_001,"Owner occupied housing unit, rooms determined (count)","Percent of owner occupied housing, rooms determined (proportion of all occupied housing, rooms determined)",TRUE
+B25014_005,owner_1.01_to_1.5_per_room,B25014_002,Owner-occupied households with 1.01 to 1.5 people per room (count),Percent of owner-occupied households with 1.01 to 1.5 people per room (proportion of all owner-occupied housing that is currently occuped),TRUE
+B25014_006,owner_1.51_to_2.0_per_room,B25014_002,Owner-occupied households with 1.51 to 2.0 people per room (count),Percent of owner-occupied households with 1.51 to 2.0 people per room (proportion of owner-occupied housing),TRUE
+B25014_007,owner_2.01_or_more_per_room,B25014_002,Owner occupied households with 2.01 or more people per room (count),Percent of owner occupied households with 2.01 or more people per room (proportion of all owner-occupied housing),TRUE
+B25014_008,total_renter_occ_units_rooms,,"Total renter occupied units, rooms determined (count)",,TRUE
+B25014_011,renter_1.01_to_1.5_per_room,B25014_008,Renter-occuped households with 1.01 to 1.5 people per room (count),Percent of renter-occuped households with 1.01 to 1.5 people per room (Proportion of all renter-occupied housing),TRUE
+B25014_012,renter_1.51_to_2.0_per_room,B25014_008,Renter-occupied households with 1.51 to 2.0 people per room (count),Percent of renter-occupied households with 1.51 to 2.0 people per room (proportion of all renter-occupied housing),TRUE
+B25014_013,renter_2.01_or_more_per_room,B25014_008,Renter-occupied ouseholds with 2.01 or more people per room (count),Renter-occupied households with 2.01 or more people per room (proportion of all renter-occupied housing),TRUE
+B25016_001,total_occupied_housing_units_plumb,,"Occupied housing units, plumbing determined (count)",,TRUE
+B25016_002,no_comp_plumb_owner,B25016_001,Owner-occupied households without adequate plumbing (count),Percent of owner-occupied households without complete plumbing  (proportion of all households),TRUE
+B25016_011,no_comp_plumb_renter,B25016_001,Renter-occupied households without complete plumbing (count),Percent of renter-occupied households without complete plumbing (proportion of all households),TRUE
+B25024_001,total_housing_units_count_in_structure,,"Housing units, units in strutcure determined (count)",,TRUE
+B25024_007,units_10_to_19,B25024_001,Total housing structures with 10 to 19 units (count),Percent of total housing structures with 10 to 19 units (proportion of all housing strutcures),TRUE
+B25024_008,units_20_to_49,B25024_001,Total housing structures with 20 to 49 units (count),Percent of total housing structures with 20 to 49 units (proportion of all housing strutcures),TRUE
+B25024_009,units_50_ormore,B25024_001,Total housing structures with 50 or more units (count),Percent of total housing sof tructures with 50 or more units (proportion of all housing strutcures),TRUE
+B25024_010,mobile_homes,B25024_001,Total mobile homes (count),Percent of mobile homes (proportion of all housing structures),TRUE
+B25038_002,owner_occ_tenure,,"Owner occupied housing, tenure determined",,TRUE
+B25038_007,owner_occ_moved_in_1999_to_1990,B25038_002,Owner occupied households who moved in 1990 to 1999 (count),Percent of owner occupied households who moved in 1990 to 1999 (proportion of all owner occupied housing),TRUE
+B25038_008,owner_occ_moved_in_1989_earlier,B25038_002,Owner occupied households who moved in 1989 or earlier (count),Percent of owner occupied households who moved in 1989 or earlier (proportion of all owner occupied housing),TRUE
+B25038_009,renter_occ_tenure,,"Renter occupied households, tenure determined",,TRUE
+B25038_014,renter_occ_moved_in_1999_to_1990,B25038_009,Renter occupied households who moved in 1990 to 1999 (count),Percent of renter occupied households who moved in 1990 to 1999 (proportion of all renter occupied housing,TRUE
+B25038_015,renter_occ_moved_in_1989_earlier,B25038_009,Renter occupied households who moved in 1989 or earlier (count),Percent of renter occupied households who moved in 1989 or earlier (proportion of all renter ocuppied households),TRUE
+B25043_001,total_occupied_housing_units_tele,,"Occupied housing units, telephone determined (count)",,TRUE
+B25043_002,total_owner_occupied_housing_tele,B25043_001,"Owner occupied housing units, telephone determined (count)","Percent of owner occupied housing units, telephone determined (proportion of all occupied housing units, tenure determined)",TRUE
+B25043_007,no_phone_owner,B25043_002,Owner occupied households without a telephone (count),Percent of households without a telephone (proportion of all households),TRUE
+B25043_011,renter_occupied,B25003_001,Renter-occupied households (count),Percent of housing units that are renter-occupied (percent of all housing units),TRUE
+B25043_016,no_phone_renter,B25043_011,Renter occupied households without a telephone (count),Percent of renter occupied households without a telephone (proportion of all households),TRUE
+B25044_001,occupied_housing_vehicle_determined,,"Total occupied housing units, vehicles determined",,TRUE
+B25044_002,total_owner_occ_housing_units_vehic,B25044_001,"Owner occupied housing units, vehicles determined (count)","Percent of owner occupied housing units, vehicles determined (proportion of all households with vehicles determined)",TRUE
+B25044_003,owner_no_vehicle,B25044_002,Owner occupied households with no vehicles available (count),Percent of owner occupied households with no vehicle available (Porportion out of all owner occupied households),TRUE
+B25044_009,total_renter_occ_vehicles,B25044_001,"Total renter occupied households, vehicles determined (count)","Percent of renter occupied housing units, vehicles determined (proportion of all households with vehicles determined)",TRUE
+B25044_010,renter_no_vehicle,B25044_009,Renter occupied households with no vehicles available (count),Percent of renter occupied households with no vehicle available (Proportion of all renter occupied households),TRUE
+B25064_001,median_rent,,Median monthly rent (dollars),,FALSE
+B25070_001,renter_occupied,,Renter-occupied units,,TRUE
+B25070_010,renter_occupied_cost_50_or_more,B25070_001,Renter-occupied households with rental costs 50% or more of income,Percent of renter-occupied households with rental costs 50% or more of income (proportion of all renter-occupied units),TRUE
+B25077_001,med_home_val,,Median Home value (dollars),,FALSE
+B25088_002,median_mortgage,,Median monthly housing costs for housing units with a mortgage (dollars),,FALSE
+B25091_001,owner_occupied,,owner-occupied housing units,,TRUE
+B25091_002,owner_occ_w_mortgage,B25091_001,Owner-occupied housing units with a mortgage (count),Percent of owner-occupied housing units with a mortgage (proportion of all owner-occupied housing),TRUE
+B25091_011,owner_occupied_w_mortgage_cost_50_or_more,B25091_002,Owner-occupied households with a mortgate & with rental costs 50% or more of income,Percent of owner-occupied households with a mortgage and with rental costs 50% or more of income (proportion of all renter-occupied units),TRUE
+B25091_013,owner_occ_wo_mortgage,B25091_001,Owner-occupied housing units without a mortgage (count),Percent of owner-occupied housing units without a mortgage (proportion of all owner-occupied housing),TRUE
+B25091_022,owner_occ_wo_mortgage_cost_50_or_more,B25091_013,Owner-occupied households without a mortgate & with rental costs 50% or more of income,Percent of owner-occupied households without a mortgage and with rental costs 50% or more of income (proportion of all renter-occupied units),TRUE
+B25129_001,total_occupied_housing_year,,"Occupied housing units, year determined (count)",,TRUE
+B25129_002,total_owner_occ_housing_year,B25129_001,"Owner occupied housing units, tenure determined (count)","Percent of owner occupied housing units, tenure determined (proportion of all occupied housing units, tenure determined)",TRUE
+B25129_003,owner_occupied_recent_move,B25129_002,Owner occupied housing with residents who recently moved in (2015 or later; count),Percent of owner occupied housing with residents who recently moved in (Proportion of all owner occupied housing),TRUE
+B25129_038,total_renter_occ_housing_year,B25129_001,"Total renter occupied units, tenure determined (count)","Percent of renter occupied units, tenure determined (proportion of all occupied units, tenure determined)",TRUE
+B25129_039,renter_occupied_recent_move,B25129_038,Renter occupied housing with residents who recently moved in (2015 or later; count),Percent of renter occupied housing with residents who recently moved in (proportion of all renter occupied housing),TRUE
+B26001_001,group_quarters,,Persons in group a quarters (count),,TRUE
+C24030_002,males_16_older_workforce,,Males 16 and older in the workforce (count),,TRUE
+C24030_018,males_in_professional_occup,C24030_002,"Males 16 and older in a Professional, Scientific, or Technical occupations (count)","Percent of males 16 and older in a Professional, Scientific, And Technical occupation (proportion of all residents 16 and older)",TRUE
+C24030_019,males_in_management,C24030_002,"Males 16 and older in a management, business, science, or arts occupation (count)","Percent of males 16 and older in a management, business, science, or arts occupation (proportion of all residents 16 and older)",TRUE
+C24030_029,females_16_older_workforce,,Females 16 and older in the workforce,,TRUE
+C24030_045,females_in_professional_occup,C24030_029,"Females 16 and older in a Professional, Scientific, or Technical occupations (count)","Percent of females 16 and older in a Professional, Scientific, And Technical occupation (proportion of all residents 16 and older)",TRUE
+C24030_046,females_in_management,C24030_029,"Females 16 and older in a management, business, science, or arts occupation (count)","Percent of females 16 and older in a management, business, science, or arts occupation (proportion of all residents 16 and older)",TRUE
+C24060_001,employed_pop_16_above,,Civilian employed population 16 and older (count),,TRUE
+C24060_002,mgmt_busi_sci_arts_occup,C24060_001,"Residents 16 and older in a management, business, science, or arts occupation (count)","Percent of residents 16 and older in a management, business, science, or arts occupation (proportion of all residents 16 and older)",TRUE