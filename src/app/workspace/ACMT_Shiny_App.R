## Shiny App Set up ##################################################################################################################

list.of.packages <- c("shiny","ggmap", "excelR", 'rhandsontable', 'DT', 'tidycensus', 'tidyverse', 'dplyr', 'janitor', 'reshape2', 
                      'promises', 'future', 'shinythemes', 'future', 'gridExtra', 'formattable', 'shinyWidgets', 'plyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
future::plan(multisession)

#set up all of the functions and files
source('setup-acmt.R')
source('external_data-presets.R')
source('external_data-file_loader.R')
#source('ACMT_Shiny_App/data_pull_settings/acs_variables_list.R')
source('ACMT_Shiny_App/data_pull_settings/shinyapp-functions.R')
source('ACMT_Shiny_App/data_pull_settings/shinyapp_external_data_functions.R')
source('ACMT_Shiny_App/data_pull_settings/cdc_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/acs_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/walk_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/mrfei_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/park_data_settings.R')
#source('ACMT_Shiny_App/data_pull_settings/crimerisk_data_settings.R')
#source('ACMT_Shiny_App/data_pull_settings/sidewalk_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/rpp_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/gentrification_data_settings.R')
source('ACMT_Shiny_App/data_pull_settings/nlcd_data_settings.R')
source('ACMT_Shiny_App/correlation_analysis.R')


data.preview.choices<-c('Show geocoded dataset', 'Show environmental measures data pull', 'Show measure summary', 'Show missingness/count summary')

### SHINY APP UI ######
ui<-shinyUI(
  fluidPage(theme=shinythemes::shinytheme('simplex'),
            
            tags$style("#shiny-notification-status_notif {position: fixed; top: 45%; left: 50%; width: 15em; opacity: 1;}"),
            tags$style("#shiny-notification-process_notif {position: fixed; top: 50%;  left: 50%; width: 15em; opacity: 1;}"),
            tags$style("#shiny-notification-stop_message {position: fixed; top: 55%;  left: 50%; width: 15em; opacity: 1;}"),
            
            navbarPage(title='ACMT ENVIRONMENTAL MEASURES APP',
                       
# ----- GEOCODER -----------------------------------------
                       tabPanel('GEOCODER', 
                                
                                navlistPanel(widths=c(3, 9),
# ======= Upload address data ==================================================
                                             'GEOCODER',
                                             
                                             tabPanel('1. Upload Address data',
                                                      ##upload instructions ##
                                                      fluidRow(style = 'padding-left:30px',
                                                               h4(em('1. Format your address data according to the example dataset below')),
                                                               em('If your dataset is already geocoded, proceed to uthe ACMT and upload your geocoded data there'),
                                                               div(tableOutput('example_table_preview'), style = "padding: 10px; font-size: 95%; width: 70%")),
                                                      fluidRow(style = 'padding-left:30px',
                                                               h4(em('2. Save the file as a .csv file'))),
                                                      fluidRow(style = 'padding-left:30px',
                                                               h4(em('3. Upload file below')),
                                                               column(width=8, 
                                                                      wellPanel(
                                                                        #textInput('datafile_name', 'Enter name for data pulls (do not include spaces; i.e., study_name):'),
                                                                        strong('Upload datafile.'), 
                                                                        em('Note you will receive an error if the dataset is formatted incorrectly'),
                                                                        fileInput('datafile', 'Choose CSV file',
                                                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain')), 
                                                                      ))
                                                      )),

# ======= Geocode data ===================================================================
                                             tabPanel('2. Geocode Address Data', 
                                                      fluidRow(style = 'padding-left:30px', 
                                                              h4(em('Address dataset:')),
                                                              div(DT::dataTableOutput('filetable_preview'), style = "padding: 10px; font-size: 85%; width: 80%")),
                                                      fluidRow(style = 'padding-left:30px',
                                                               h2(em('Click the button below to run the geocoding function'))),
                                                      fluidRow(style='padding-left:30px', 
                                                               actionButton('geocodeButton', 'Geocode dataset')),
                                                      fluidRow(style='padding-left:30px', 
                                                               h2(em('Summary of geocoding ratings:')),
                                                               actionButton('checkratings', 'Get ratings summary'),
                                                               p('Ratings range from 0-100, with ratings of 0 the highest level of confidence.'),
                                                               DT::dataTableOutput('ratingstable'), 
                                                               p())
                                        
                                                      # button to combine/rename address fields into one columns (may add this for a later iteration)
                                             ),
# ======= Check geocodes & update addresses =======================================
                                             tabPanel(
                                               '3. Verify & update geocodes',
                                               fluidRow(style='padding-left:30px',
                                                        h5(strong('Click button to upload geocoded data or to refresh after updating dataset')),
                                               ),
                                               fluidRow(style='padding-left:30px', 
                                                        div(actionButton('refreshgeocode', 'Load/refresh geocoded data'), style='display:center-align')),
                                               fluidRow(style='padding-left:30px', 
                                                        br(), 
                                                        h4("Review and update addresses and re-geocode:"), 
                                                        tags$ul(
                                                          tags$li("Double click address cell to update address text (i.e., spell out abbreviated names, fix misspellings)"), 
                                                          tags$li("After updating, click", strong("'Load/regresh geocoded data'"), "to re-geocode the updated addresses")
                                                        ), 
                                                        em("For addresses that appear correct, but have high geocode ratings, use the mapping tool below")
                                                        ),
                                               # dataset_geocoded data table
                                               conditionalPanel(
                                                 condition = "input.idNumber !=''", 
                                                 fluidRow(
                                                   column(width=12, 
                                                          DT::dataTableOutput('dataset_geocoded2'))
                                                   ),
                                                 br(),
                                                 fluidRow(style='padding-left:30px',
                                                          h4("Mapping tool: Select participant ID to map"),
                                                          column(width=3, 
                                                                 selectInput('idNumber', 'Select ID', "")
                                                          ),
                                                          column(width=4,
                                                                 br(),
                                                                 div(actionButton('mapButton', 'Generate map'))
                                                          ) 
                                                 )),
                                               
                                               conditionalPanel(
                                                 condition="input.mapButton>0 & input.idNumber != 'Select a single id to map'",
                                                 fluidRow(
                                                   column(width=4,
                                                          tags$h4(strong("Follow these steps to check and update addresses and lat/long values:")),
                                                          wellPanel(
                                                            tags$h4("Check map & add notes:"),
                                                            tags$ul(
                                                              tags$li(" Mapped to correct street?"),
                                                              tags$li(" Mapped to the correct city?"),
                                                              tags$li(" Any obvious errors in mapped location? (i.e., mapped into water, mapped into area with no houses)")),
                                                            radioButtons('geocode_check', '', choices=c('Geocode looks accurate (correct street/city)', 'Geocode does not look accurate')),
                                                            actionButton('geocode_notes', 'Update Notes'),
                                                          ),
                                                          wellPanel(tags$h4("Manually update geocode location"), 
                                                                    tags$li("Click directly on the map to update the geocode 
                                                (you should be positive that this is the correct location before clicking)"),
                                                                    tags$li("Once you have clicked on the map, click the 'Update geocode' button below to add the updated lat/long to the dataset,
                                               then reload your data."),
                                                                    tableOutput('updated_geocode'),
                                                                    actionButton('update_geocode', 'Update geocode'))
                                                   ), 
                                                   column(width=8,
                                                          leafletOutput('map', width='700px', height='700px'),
                                                          wellPanel(textOutput('maplabel'), width='100%'), 
                                                   )
                                                 ), 
                                               ),
                                               class="p-3 border border-top- rounded-bottom"
                                             )
                                ) #end of navList
                                
                       ),
# ----- ACMT ####
           tabPanel('Automated Context Measure Assembler', 
                    navlistPanel(widths=c(3,9), 
# ----- Introduction Page ----------------------------------------
                                 tabPanel(title='INTRODUCTION', 
                                          h1('Introduction'),
                                          fluidRow(style='padding-left:50px; padding-right:50px', 
                                                   p('The ACMT (Automatic Context Measurement Tool) Application provides a user-friendly interface for geocoding datasets and 
                                           assemble context measurements from publically available data sources, including:'),
                                                   uiOutput('measureList'),   
                                                   p("The ACMT can create interpolated environmental measures for a given radius around a latitude/longitude point (i.e., an individual's address). 
                                         On each tab you can select from the list of available years of data, input the radius or radii of interest and pull data for your geocoded dataset. 
                                         The data process can be paused and restarted as needed. In your ACMT_Shiny_App folder, you will find a folder titled data_pull_measures, 
                                         which is where you will find your environmental measures datasets with all personal information removed."),
                                                   p("Begin by uploading your dataset with either address data or if your data is already geocoded, upload your dataset with latitude and longitude columns. 
                                         If your data is not yet geocoded, you will then proceed to the Geocode step, where the ACMT will geocode your data and you will have the opportunity to verify, clean and update
                                         address data as needed before exporting your final geocoded dataset for your records."),
                                                   p("Finally, once you have a dataset with latitude and longitude values, you can proceed to pulling environmental measures.")
                                          )
                                 ),
                                 
                                 
# ======== Upload geocoded data ===================================================
                      tabPanel('Upload geocoded data', 
                               fluidRow(style='padding-left:30px',
                                        h4('If you are uploading already geocoded data, ensure the data file is formatted according to the example below,
                                           save the file as a .csv file, then click below to upload.'),
                                        em('If you used the ACMT Geocoder tool, you can skip this step.'),
                                        p(em('Your geocoded dataset will be saved in your R environment as'), strong('dataset_geocoded.csv')),
                                        div(tableOutput('example_table_geocoded'), style = "padding: 10px; font-size: 95%; width: 70%"),   
                               ), 
                               fluidRow(style='padding-left:30px; padding-bottom:5px', 
                                        column(width=8,
                                               wellPanel(
                                                # textInput('datafile_name', 'Enter name for data pulls (do not include spaces; i.e., study_name):'),
                                                 strong('Upload geocoded data'), 
                                                 p(em('Note you will receive an error if the dataset is formatted incorrectly')),
                                                 fileInput('geodatafile', 'Choose CSV file',
                                                           accept=c('text/csv', 'text/comma-separated-values,text/plain')) 
                                               ))
                               ),
                               fluidRow(style='padding-left:30px', 
                                        h4(em('Uploaded dataset:')),
                                        div(DT::dataTableOutput('geofiletable_preview'), style = "padding: 10px; font-size: 85%; width: 80%"))
                      ),
# ======== ACMT MEASURES =================================================
                  'Environmental Measures',
# ======== Walkability ====================================================
                            tabPanel('>> Walkability', 
                                     #Walkability settings
                                     ## Step 1 ##
                                     fluidRow(style = 'padding-left:30px; padding-bottom: 0px; padding-top:0px;', 
                                              h4('Year of data:'),
                                              tags$div(style='padding-left:30px', em('Walkabilty data is only available for 2019')),
                                              h4('Buffer radius:'),
                                              tags$div(style='padding-left:30px', align='left', class='multicol', 
                                                       column(width=4,
                                                              selectizeInput('selectradii_walk', 
                                                                             label='Select or enter each radius of interest for interpolation buffers',
                                                                             choices = radiiDF$radius, 
                                                                             selected=NULL, 
                                                                             multiple = TRUE, 
                                                                             width = '100%', 
                                                                             options = list(
                                                                               'plugins' = list('remove_button'), 
                                                                               'create' = TRUE, 
                                                                               'persist' = TRUE
                                                                             )
                                                              ))
                                              )
                                     ),
                                     wellPanel(style = 'padding:20px;',
                                               fluidRow(
                                                 column(width=3,style='padding-left:30px;',
                                                        fluidRow(
                                                          #button to preview data and show progress
                                                          shinyWidgets::actionBttn('status_walk', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                                                        fluidRow( 
                                                          div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_walk_message'))),),
                                                 column(width=2,
                                                        fluidRow(style = 'padding:5px;padding-top:0px',
                                                                 ##button to run loop to pull data
                                                                 shinyWidgets::actionBttn('pull_walk', 'Pull Walkability data', style='simple', color='success', size='xs')),
                                                        #column(width=2, 
                                                        fluidRow(style = 'padding:5px;',
                                                                 tags$div(style='align-center'),
                                                                 ##button to pause the loop
                                                                 shinyWidgets::actionBttn('stop_walk', 'STOP data pull', style='simple', color='danger', size='xs')), 
                                                 )),
                                               fluidRow(style='padding:5px',
                                                        div(DT::dataTableOutput('dataset_walk'), style = "font-size: 95%; width: 100%") 
                                                        #        ## add data pull preview / progress to well panel ##
                                               )
                                     ), # wellPanel
                                     width=3,style='min-width:200px'
                            
                                     ), #end of Walkability tabpanel
# ======= MRFEI ====================================================
tabPanel('>>  Modified Food Retail Environment Index (MRFEI)', 
         #MRFEI settings
         fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
                  h4('Year of data:'), 
                  tags$div(style='padding-left:30px', em('MRFEI data is only available for 2011')),
                  h4('Buffer Radius:'),
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           column(width=4,
                                  selectizeInput('selectradii_mrfei', 
                                                 label='Select or enter each radius of interest for interpolation buffers',
                                                 choices = radiiDF$radius, 
                                                 selected=NULL, 
                                                 multiple = TRUE, 
                                                 width = '100%', 
                                                 options = list(
                                                   'plugins' = list('remove_button'), 
                                                   'create' = TRUE, 
                                                   'persist' = TRUE
                                                 )
                                  ))
                  )),
         wellPanel(style = 'padding:20px;',
                   fluidRow(
                     column(width=3,style='padding-left:30px;',
                            fluidRow(
                              #button to preview data and show progress
                              shinyWidgets::actionBttn('status_mrfei', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                            fluidRow( 
                              div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_mrfei_message'))),),
                     column(width=2,
                            fluidRow(style = 'padding:5px;padding-top:0px',
                                     ##button to run loop to pull data
                                     shinyWidgets::actionBttn('pull_mrfei', 'Pull MRFEI data', style='simple', color='success', size='xs')),
                            #column(width=2, 
                            fluidRow(style = 'padding:5px;',
                                     tags$div(style='align-center'),
                                     ##button to pause the loop
                                     shinyWidgets::actionBttn('stop_mrfei', 'STOP data pull', style='simple', color='danger', size='xs')), 
                     )),
                   fluidRow(style='padding:5px',
                            div(DT::dataTableOutput('dataset_mrfei'), style = "font-size: 95%; width: 100%") 
                            #        ## add data pull preview / progress to well panel ##
                   )
         ), # wellPanel
         width=3,style='min-width:200px'
         
), #end of MRFEI tabPanel

# ======= CDC ====================================================
tabPanel('>>  CDC Places Data', 
         #cdc settings
         fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
                  h4('Year of data:'), 
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           checkboxGroupInput('selectyear_cdc', 'Select years', choices=cdc_years, selected=cdc_selected_years, inline=TRUE)),
                  h4('Buffer Radius:'),
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           column(width=4,
                                  selectizeInput('selectradii_cdc', 
                                                 label='Select or enter each radius of interest for interpolation buffers',
                                                 choices = radiiDF$radius, 
                                                 selected=NULL, 
                                                 multiple = TRUE, 
                                                 width = '100%', 
                                                 options = list(
                                                   'plugins' = list('remove_button'), 
                                                   'create' = TRUE, 
                                                   'persist' = TRUE
                                                 )
                                  ))
                  )),
         wellPanel(style = 'padding:20px;',
                   fluidRow(
                     column(width=3,style='padding-left:30px;',
                            fluidRow(
                              #button to preview data and show progress
                              shinyWidgets::actionBttn('status_cdc', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                            fluidRow( 
                              div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_cdc_message'))),),
                     column(width=2,
                            fluidRow(style = 'padding:5px;padding-top:0px',
                                     ##button to run loop to pull data
                                     shinyWidgets::actionBttn('pull_cdc', 'Pull cdc data', style='simple', color='success', size='xs')),
                            #column(width=2, 
                            fluidRow(style = 'padding:5px;',
                                     tags$div(style='align-center'),
                                     ##button to pause the loop
                                     shinyWidgets::actionBttn('stop_cdc', 'STOP data pull', style='simple', color='danger', size='xs')), 
                     )),
                   fluidRow(style='padding:5px',
                            div(DT::dataTableOutput('dataset_cdc'), style = "font-size: 95%; width: 100%") 
                            #        ## add data pull preview / progress to well panel ##
                   )
         ), # wellPanel
         width=3,style='min-width:200px'
         
), #end of cdc tabPanel

# ======= ParkServe Data =====================================================
tabPanel('>>  ParkServe Data', 
         #parks settings
         fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
                  h4('Year of data:'), 
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           checkboxGroupInput('selectyear_parks', 'Select years', choices=parks_years, selected=parks_selected_years, inline=TRUE)),
                  h4('Buffer Radius:'),
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           column(width=4,
                                  selectizeInput('selectradii_parks', 
                                                 label='Select or enter each radius of interest for interpolation buffers',
                                                 choices = radiiDF$radius, 
                                                 selected=NULL, 
                                                 multiple = TRUE, 
                                                 width = '100%', 
                                                 options = list(
                                                   'plugins' = list('remove_button'), 
                                                   'create' = TRUE, 
                                                   'persist' = TRUE
                                                 )
                                  ))
                  )),
         wellPanel(style = 'padding:20px;',
                   fluidRow(
                     column(width=3,style='padding-left:30px;',
                            fluidRow(
                              #button to preview data and show progress
                              shinyWidgets::actionBttn('status_parks', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                            fluidRow( 
                              div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_parks_message'))),),
                     column(width=2,
                            fluidRow(style = 'padding:5px;padding-top:0px',
                                     ##button to run loop to pull data
                                     shinyWidgets::actionBttn('pull_parks', 'Pull parks data', style='simple', color='success', size='xs')),
                            #column(width=2, 
                            fluidRow(style = 'padding:5px;',
                                     tags$div(style='align-center'),
                                     ##button to pause the loop
                                     shinyWidgets::actionBttn('stop_parks', 'STOP data pull', style='simple', color='danger', size='xs')), 
                     )),
                   fluidRow(style='padding:5px',
                            div(DT::dataTableOutput('dataset_parks'), style = "font-size: 95%; width: 100%") 
                            #        ## add data pull preview / progress to well panel ##
                   )
         ), # wellPanel
         width=3,style='min-width:200px'
         
), #end of parks tabPanel

# ===== National Land Cover Database ================================
tabPanel('>> National Land Cover Database', 
         #nlcd settings
         fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
                  h4('Year of data:'), 
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           checkboxGroupInput('selectyear_nlcd', 'Select years', choices=nlcd_years, selected=nlcd_selected_years, inline=TRUE)),
                  h4('Buffer Radius:'),
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           column(width=4,
                                  selectizeInput('selectradii_nlcd', 
                                                 label='Select or enter each radius of interest for interpolation buffers',
                                                 choices = radiiDF$radius, 
                                                 selected=NULL, 
                                                 multiple = TRUE, 
                                                 width = '100%', 
                                                 options = list(
                                                   'plugins' = list('remove_button'), 
                                                   'create' = TRUE, 
                                                   'persist' = TRUE
                                                 )
                                  ))
                  )),
         wellPanel(style = 'padding:20px;',
                   fluidRow(
                     column(width=3,style='padding-left:30px;',
                            fluidRow(
                              #button to preview data and show progress
                              shinyWidgets::actionBttn('status_nlcd', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                            fluidRow( 
                              div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_nlcd_message'))),),
                     column(width=2,
                            fluidRow(style = 'padding:5px;padding-top:0px',
                                     ##button to run loop to pull data
                                     shinyWidgets::actionBttn('pull_nlcd', 'Pull nlcd data', style='simple', color='success', size='xs')),
                            #column(width=2, 
                            fluidRow(style = 'padding:5px;',
                                     tags$div(style='align-center'),
                                     ##button to pause the loop
                                     shinyWidgets::actionBttn('stop_nlcd', 'STOP data pull', style='simple', color='danger', size='xs')), 
                     )),
                   fluidRow(style='padding:5px',
                            div(DT::dataTableOutput('dataset_nlcd'), style = "font-size: 95%; width: 100%") 
                            #        ## add data pull preview / progress to well panel ##
                   )
         ), # wellPanel
         width=3,style='min-width:200px'
         
), #end of NLCD tabPanel

# ===== American Community Survey ================================
tabPanel('>> American Community Survey', 
         #ACS settings
         fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
                  h4('Year of data:'), 
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                           #checkboxGroupInput('selectyear_acs', 'Select years', choices=acs_years, selected=acs_selected_years, inline=TRUE)),
                            selectizeInput('selectyear_acs', 
                                           label='Select year (note: selected year indicates the last year of a 5-year span; i.e., selecting 2015 pulls 2011-2015 data)', 
                                           choices=acs_years, selected=2015, multiple=FALSE,
                                           options=list('plugins'=list('remove_button'), 
                                                        'create'=FALSE))),
                  h4('Buffer Radius:'),
                  tags$div(style='padding-left:30px', align='left', class='multicol', 
                                  selectizeInput('selectradii_acs', 
                                                 label='Select or enter each radius of interest for interpolation buffers',
                                                 choices = radiiDF$radius, 
                                                 selected=NULL, 
                                                 multiple = TRUE, 
                                                 options = list(
                                                   'plugins' = list('remove_button'), 
                                                   'create' = TRUE, 
                                                   'persist' = TRUE
                                  ))
                  )),
          # fluidRow(style = 'padding:30px; padding-top:0px; padding-bottom: 0px;',
          #           h4('Select ACS Variables to pull:'),
          #           tags$div(style='padding-left:30px', align='left',
          #                    radioButtons('acs_default', '', choices=c('Use default ACS list', 'upload list of ACS variables'), selected='Use default ACS list')),
          #           conditionalPanel(
          #             condition="input.acs_default=='upload list of ACS variables'",
          #            fluidRow(style = 'padding-left:30px',
          #                     shinyWidgets::actionBttn('download_full_acs_list', 'Download full list of ACS variables for selected year(s)', style='simple', color='danger', size='xs'),
          #                     p(),
          #               p(em('Format your list of ACS variables according to the example below and save as a .csv file:')),
          #               div(tableOutput('example_acs_table_preview'), style = "padding: 10px; font-size: 95%; width: 70%")),
          #            fileInput('acs_variable_file', 'Choose CSV file',
          #                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          #            div(dataTableOutput('acs_var_preview'), style = "padding: 10px; font-size: 95%; width: 70%"),
          #            
          # 
          #           ), 
          #          ## button to download a list of default ACS variables ##
          #          conditionalPanel(
          #            condition="input.acs_default=='Use default ACS list'", 
          #            fluidRow(style='padding-left:30px', 
          #                     column(width=7, 
          #                            downloadButton('download_default_acs_list', 'Download default list of ACS variables')
          #                            #actionButton('download_default_acs_list', 'Download default list of ACS variables'), 
          #                     ),
          #                     column(width=4, 
          #                            textOutput("acs_message"))
          #            ), 
          #            p(),
          #          )),
 
         wellPanel(style = 'padding:20px;',
                   fluidRow(
                     column(width=3,style='padding-left:30px;',
                            fluidRow(
                              #button to preview data and show progress
                              shinyWidgets::actionBttn('status_acs', 'Show/update data progress', style='simple', color='primary', size='xs')), 
                            fluidRow( 
                              div(style='font-size: 95%; color:#DC714D;font-style: italic',tableOutput('dataset_acs_message'))),),
                     column(width=2,
                            fluidRow(style = 'padding:5px;padding-top:0px',
                                     ##button to run loop to pull data
                                     shinyWidgets::actionBttn('pull_acs', 'Pull acs data', style='simple', color='success', size='xs')),
                            #column(width=2, 
                            fluidRow(style = 'padding:5px;',
                                     tags$div(style='align-center'),
                                     ##button to pause the loop
                                     shinyWidgets::actionBttn('stop_acs', 'STOP data pull', style='simple', color='danger', size='xs')), 
                     )),
                   fluidRow(style='padding:5px',
                            div(DT::dataTableOutput('dataset_acs'), style = "font-size: 95%; width: 100%") 
                            #        ## add data pull preview / progress to well panel ##
                   )
         ), # wellPanel
         width=3,style='min-width:200px'
         
) #end of ACS tabPanel

                    )#end of navlist
                    ),#end of ACMT TabPanel
## ----- Data Summaries & Correlation Analysis ####
tabPanel('Data Summaries & Correlation Analyses', 
         navlistPanel(widths=c(3,9), 
         # ======= Data Pull Summaries =============================================
         tabPanel('Data Pull Summaries', 
                  fluidRow(style='padding:30px; padding-bottom:0px',
                           actionButton('refreshprogress', 'Refresh Data Summary')),
                  fluidRow(
                    #div(DT::dataTableOutput('output$tables'), style = "padding: 10px; font-size: 100%; width: 30%")
                    tags$div(style='padding:30px;padding-top:0px;font-size:85%; width:40%', uiOutput("tables"))
                  )
         ), # end of Data Pull Summaries 
         # ======= Correlation Analysis =================================================
         tabPanel('Correlation Analyses', 
                  column(style='padding:30px', width = 3,
                         #select datasets for correlation analysis
                         shinyWidgets::pickerInput('selectdatasets_corr', 
                                     'Select datasets to include in correlation analysis',
                                     choices = all_names_of_data(), 
                                     #selected = all_names_of_data(),
                                     options = list(`actions-box` = TRUE),multiple = T),
                         
                         conditionalPanel(
                           'input.selectdatasets_corr.length>0',
                           #select radii for correlation analysis
                           shinyWidgets::pickerInput('selectradii_corr', 
                                          label='Select radius to include in correlation analysis',
                                          choices = c(500, 1000, 5000), 
                                       options = list(`actions-box` = TRUE),multiple = T),
                           #select years for correlation analysis
                           shinyWidgets::pickerInput('selectyear_corr', 
                                          'Select years to include in correlation analysis',
                                          choices = c(2017), 
                                       options = list(`actions-box` = TRUE),multiple = T),
                          
                           #select number of variables for correlation analysis
                           shinyWidgets::pickerInput('selectcorrNumber', 
                                       'Select number of variables to include in correlation analysis',
                                       choices = c(5, 10, 20)),
                           
                           
                           actionButton('run_corr_analysis', 'Run Random Correlation Analysis')
                         )
                           ), 
                  column(style='padding:30px', width=9,
                           plotOutput('randomCorrPlot')))
         
         )
                       )
            )
  )
)

### SERVER actions ##################################################
server<-function(input, output, session) {
  
# Status functions ####
  
  #Data pull status functions
  status_file <- tempfile()
  current_id<-tempfile()
  
  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }
  
  get_current_id<-function(){
    scan(current_id, what="character", sep="\n")
  }
  
  set_status <- function(msg){
    write(msg, status_file)
  }
  set_current_id<-function(msg){
    write(msg, current_id)
  }
  
  fire_interrupt <- function(){
    set_status("Data process stopping, please wait")
  }
  
  fire_ready <- function(){
    set_status("Ready...")
    set_current_id('Click the pull data button to begin or resume data pull')
  }
  
  fire_running <- function(perc_complete){
    if(missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0(perc_complete, "% Complete")
    set_status(msg)
  }
  
  auto_status<-function(id=id, radius, year){
    req(id)
    msg<-paste0('Currently processing id # ', id, ' for radius = ', radius, ' and year = ', year)
    set_current_id(msg)
  }
  
  interrupted <- function(){
    get_status() == "Data process stopping, please wait"
  }
  
  # Delete file at end of session
  onStop(function(){
    if(file.exists(status_file))
      unlink(status_file)
    if(file.exists(current_id))
      unlink(current_id)
  })  
  
  # Create Status File
  fire_ready()
  nclicks <- reactiveVal(0)
  result_val <- reactiveVal()
  
# ----- Section 1: INTRODUCTION --------------------------------------------
  output$measureList<-renderUI(HTML(markdown::renderMarkdown(text = paste(paste0("- ", measures.list, "\n"), collapse = ""))))

# ----- Section 2: GEOCODER ------------------------------------------------
## Show example data table
  example_table<-reactiveValues(data=example_address)
  
# ======= upload data ===================================================  
  observeEvent(input$upload_dataset, {
    if(input$upload_dataset == 'Participant id & address (no geocodes)'){
      example_table$data <-example_address}
    if(input$upload_dataset == 'Geocoded participant data'){
      example_table$data<-example_geocoded
    }
  })
  
  # show example dataset
  output$example_table_preview<-renderTable({example_table$data})
  
  ## Upload data file action ###
  filedata <- reactive({
    req(input$datafile)
    tryCatch({
      infile <- input$datafile
      #if (is.null(infile)) {
      # User has not uploaded a file yet
      #  return(NULL)
      #}
      read.csv(infile$datapath)
      
    }, error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error',id = 'status_notif', duration=NULL)
      return()}
    )
  })
  
  # save data after uploading file 
  observeEvent(input$datafile, {
      tryCatch({data<-filedata() %>% dplyr::select(id, address)
      }, 
      error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error', id='status_notif', duration=NULL)
        return()}
      )
      saveData(data=data, fileName='dataset_address.csv')
      filetable$data<-data
  })
  
# ======= Geocode data =====================================================
  # show uploaded datafile
  filetable<-reactiveValues(data=data.frame(Message='Upload data to preview'))
  #This previews the CSV data file or geocoded datafile ##
  output$filetable_preview<- DT::renderDataTable({filetable$data}, editable=FALSE, 
                                                 rownames=FALSE,
                                                 options = list(
                                                   searching = FALSE,
                                                   pageLength = 10,
                                                   autowidth=FALSE,
                                                   scrollX=TRUE
                                                 ))
  
## function to geocode uploaded dataset  
  geocode_data<-reactive({
    raw_data<-read.csv('ACMT_Shiny_App/dataset_address.csv')
    filetable$data<-withProgress(geocode_loop(raw_data), message='Geocoding dataset')
  })    
  
## save data after geocoding
  observeEvent(input$geocodeButton, {
    data<-geocode_data()
    showNotification('Geocoding Complete', id='status_notif', duration=NULL)
    data<-data %>% mutate(geocode_notes='')
    saveData(data=data, fileName=paste0('dataset_geocoded.csv'))}
  )
  
## Summary of geocode ratings
  ratingstable<-reactiveValues(data=data.frame(GeocodeRating="Geocode data and click the 'Check geocode ratings' button to view ratings", n=NA, percent=NA))
  
  observeEvent(input$checkratings, {
    data<-loadData('dataset_geocoded.csv')
    ratingstable$data<-ratings_table(data)%>%DT::datatable()%>%DT::formatPercentage('percent', 1)
  })
  
  output$ratingstable<- DT::renderDataTable({ratingstable$data})
  
# ======== Check geocodes & update addresses =====================================
## upload geocoded dataset ##
  upload_geocoded_data<-eventReactive(
    input$refreshgeocode,
    {loadData('dataset_geocoded.csv') %>%#dplyr::select(-X)%>%
        mutate(address=as.character(address)) }
  )
  
##Filtered version of geocoded dataset
  filter_uploaded_data<-reactive({if(input$idNumber=='Select a single id to map' | input$idNumber=='' | is.null(input$idNumber)){upload_geocoded_data()}
    else{upload_geocoded_data()[upload_geocoded_data()$id==input$idNumber,]}
  })
  
## check ratings and maps
  output$dataset_geocoded2<-DT::renderDataTable(
    {filter_uploaded_data()},
    editable=TRUE, 
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = FALSE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    ), 
    class='display'
  )
  
  
## Update dataset_geocoded values
  observeEvent(input$dataset_geocoded2_cell_edit, {
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(address=as.character(address), geocode_notes=as.character(geocode_notes))
    str(input$dataset_geocoded2_cell_edit)
    value<-input$dataset_geocoded2_cell_edit$value
    column<-input$dataset_geocoded2_cell_edit$col
    if(input$idNumber=='Select a single id to map'){
      row<-input$dataset_geocoded2_cell_edit$row
      id<-filter_uploaded_data()$id[row]
    }
    else{id<-input$idNumber
    }
    dataset_geocoded[dataset_geocoded$id==id,][column]<-value
    
## re-geocode updated address values
    if(colnames(filter_uploaded_data()[column])=='address'){
      
      new_lat_long<-tryCatch({geocode(value)}, 
                             error=function(x){
                               list(latitude=NA, longitude=NA, rating=NA) ##imput NA value if geocoding is unsuccessful
                             })
      dataset_geocoded$lat[dataset_geocoded$id==id]<-new_lat_long$lat
      dataset_geocoded$long[dataset_geocoded$id==id]<-new_lat_long$long
      dataset_geocoded$rating[dataset_geocoded$id==id]<-new_lat_long$rating
    }
    saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
    write.csv(dataset_geocoded%>%dplyr::select(id, rating, geocode_notes), 'ACMT_Shiny_App/data_pull_measures/geocode_ratings_notes.csv')
    filter_uploaded_data()
  })
  
## update Selector input with participant IDs 
  observe({
    if(input$refreshgeocode==0)
      return()
    isolate({
      #if(input$refreshgeocode>0){
      updateSelectInput(session, 'idNumber', 
                        label='Choose ID for mapping', 
                        choices=c('Select a single id to map', upload_geocoded_data()$id), 
                        selected=c('Select a single id to map')) 
    })
    })
  
  
## create map
  vals <- reactiveValues(id = 1, lat=NULL, long=NULL, address=NULL, rating=NULL, z = 15, side_len = .007)

# update values based on ID selected  
  observeEvent(list(input$mapButton, input$idNumber), {
    vals$id<-input$idNumber
    vals$z<-input$z
    vals$side_len<-input$side_len
    vals$lat<-filter_uploaded_data()$lat[filter_uploaded_data()$id==input$idNumber]
    vals$long<-filter_uploaded_data()$long[filter_uploaded_data()$id==input$idNumber]
    vals$address<-filter_uploaded_data()$address[filter_uploaded_data()$id==input$idNumber]
    vals$rating<-filter_uploaded_data()$rating[filter_uploaded_data()$id==input$idNumber]
    
  })
  
# function to map lat/long when single ID selected and map button is pushed
  mapdata<-eventReactive(list(input$mapButton, input$idNumber),{
    if(input$idNumber=='Select a single id to map' | input$idNumber=='' | is.null(vals$address)){
      showNotification('Select 1 ID number to map')
      return()
    }
    else{
      check_geocode(lat=vals$lat, long=vals$long, address=vals$address, rate=vals$rating, id=vals$id, z=vals$z, side_len=vals$side_len)
    }
  })
  
# create the map  
  output$map<-renderLeaflet({
    withProgress(mapdata(), message = 'Mapping address for selected id')
  })
  
# create map labels
  output$maplabel<-renderText({
    if(input$mapButton==0){''}
    if(input$mapButton>0){
      paste(paste('ID = ', vals$id, sep=''), paste('address = ', vals$address, sep=''), paste("Rating =", vals$rating, sep = ""), sep=' | ')
    }
  })
  
# Manual lat/long updates 
  proxy_geocode<-reactiveValues(data=data.frame(Message='Click on map to update latitude and longitude'))
  
  output$updated_geocode<-renderTable({proxy_geocode$data})
  
# when you click the map, lat long is saved (proxy_geocode$data)
  observeEvent(input$map_click, {
    click<-input$map_click
    leafletProxy('map')%>%
      clearMarkers()%>%
      addCircleMarkers(lng=click$lng, lat=click$lat)
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
    id<-input$idNumber
    showNotification('Click the "Update Geocode" button to update the latitude and longitude to the clicked location', type='warning', id='status_notif')
    proxy_geocode$data$Message='Lat/Long for clicked location:'
    proxy_geocode$data$lat<-round(as.numeric(click$lat), 4)
    proxy_geocode$data$long<-round(as.numeric(click$lng), 4)
  })  
  
# when you click the update_geocode button, the dataset is updated with new lat/long values
  observeEvent(input$update_geocode, {
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
    id<-input$idNumber
    #calculate distance between old and new points on first udpate:
    if(!('old_lat' %in% colnames(dataset_geocoded))){
      dataset_geocoded$old_lat<-NA
      dataset_geocoded$old_long<-NA
      dataset_geocoded$distance_old_new<-NA
    }
    if(is.na(dataset_geocoded$old_lat[dataset_geocoded$id==id])){
      dist<-round(distm(c( dataset_geocoded$long[dataset_geocoded$id==id],dataset_geocoded$lat[dataset_geocoded$id==id]),
                        c(proxy_geocode$data$long, proxy_geocode$data$lat), fun = distHaversine), 2)
      #save distance, and old lat/long values
      dataset_geocoded$distance_old_new[dataset_geocoded$id==id]<-dist
      dataset_geocoded$old_lat[dataset_geocoded$id==id]<-dataset_geocoded$lat[dataset_geocoded$id==id]
      dataset_geocoded$old_long[dataset_geocoded$id==id]<-dataset_geocoded$long[dataset_geocoded$id==id]
    }
    #re-calculate distance (don't rewrite old_lat or new_lat)
    if(!is.na(dataset_geocoded$old_lat[dataset_geocoded$id==id])){
      dist<-round(distm(c(dataset_geocoded$old_long[dataset_geocoded$id==id],dataset_geocoded$old_lat[dataset_geocoded$id==id]),
                        c(proxy_geocode$data$long, proxy_geocode$data$lat), fun = distHaversine), 2)
    }
    #add new lat/long values to lat/long fields
    dataset_geocoded$lat[dataset_geocoded$id==id]<-proxy_geocode$data$lat
    dataset_geocoded$long[dataset_geocoded$id==id]<-proxy_geocode$data$long
    #dataset_geocoded$rating[dataset_geocoded$id==id]<-NA
    dataset_geocoded$distance_old_new[dataset_geocoded$id==id]<-dist
    if(!(grepl('manually updated lat/long', dataset_geocoded$geocode_notes[dataset_geocoded$id==id]))){
      if(!is.na(dataset_geocoded$geocode_notes[dataset_geocoded$id==id])){
        dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-paste0(dataset_geocoded$geocode_notes[dataset_geocoded$id==id], ', ',
                                                                        'manually updated lat/long on the map')}
      else(dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-'manually updated lat/long on the map')
    }
    proxy_geocode$data<-data.frame(Message='Click on map to update latitude and longitude')
    saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
    write.csv(dataset_geocoded%>%dplyr::select(id, rating, distance_old_new, geocode_notes), 'ACMT_Shiny_App/data_pull_measures/geocode_ratings_notes.csv')
  })
  
# Add / update notes in dataset_geocoded   
  observeEvent(input$geocode_notes,{
    
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%mutate(geocode_notes=as.character(geocode_notes))
    id<-input$idNumber
    dataset_geocoded$geocode_notes[dataset_geocoded$id==id]<-input$geocode_check
    saveData(data=dataset_geocoded, fileName='dataset_geocoded.csv')
    write.csv(dataset_geocoded%>%dplyr::select(id, rating, geocode_notes), 'ACMT_Shiny_App/data_pull_measures/geocode_ratings_notes.csv')
  })
  
  
# ------ Section 3: ACMT ----------------------------------------------------    
   # ======== Upload geocoded data ============================================  
  
  # show example address dataset
  output$example_table_geocoded<-renderTable({example_geocoded})
  
  ## Upload data file action ###
  geofiledata <- reactive({
    req(input$geodatafile)
    tryCatch({
      infile <- input$geodatafile
      read.csv(infile$datapath)
      
    }, error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error',id = 'status_notif', duration=NULL)
      return()}
    )
  })
  
  # save data after uploading file 
  observeEvent(input$geodatafile, {
    ### UPDATE THIS to add address column, only if it doesn't exist ##
    tryCatch({geodata<-geofiledata() %>% dplyr::select(id, lat, long)%>%mutate(address=NA, geocode_notes=NA, rating=NA)
    }, 
    error=function(e) {showNotification('Check dataset formatting and column names before uploading', type='error', id='status_notif', duration=NULL)
      return()}
    )
    geofiletable$data<-geodata
    saveData(data=geodata, fileName='dataset_geocoded.csv')})  
  
  # show uploaded datafile
  geofiletable<-reactiveValues(data=data.frame(Message='Upload data to preview'))
  #This previews the CSV data file or geocoded datafile ##
  output$geofiletable_preview<- DT::renderDataTable({geofiletable$data%>%dplyr::select(id, lat, long)}, editable=FALSE, 
                                                    rownames=FALSE,
                                                    options = list(
                                                      searching = FALSE,
                                                      pageLength = 5,
                                                      autowidth=FALSE,
                                                      scrollX=TRUE
                                                    ))
  
  # ======== WALKABILITY  Server Functions -------------------------------
  # generate desriptive text
  output$walk_description<-renderText(walk_description)
  preview_walk<-reactiveValues(data=data.frame())
  
  #### pull or create walk data (automatically with load data) ####
  observeEvent(input$pull_walk,{
    if(is.null(input$selectradii_walk)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/dataset_geocoded.csv')==FALSE){
      showNotification('Upload geocoded data (or use the ACMT geocoder) to pull environmental measures', type='warning')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')==FALSE){
      showNotification('Creating walk data frame', duration=5, type='message', id='process_notif') 
      dataset_walk<-create_dataset(variable_list=walk_vars)
      write.csv(dataset_walk, 'ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')==TRUE & 
       !is.null(input$selectradii_walk)){
      showNotification('Importing walk data frame', duration=5, type='message', id='status_notif')
    }
  })
  
  load_walk<-reactive({
    dataset_walk<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')%>%dplyr::select(walk_vars[1]:radius)
    dataset_walk
  })
  
  #### Data pull process: Walkability #### 
  observeEvent(input$pull_walk,{
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/walk_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_walk)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling walk data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of walk data, using geocoded data: 
    #loop to pull walk data: 
    years<-as.numeric(walk_years)
    radius_vector <- as.numeric(input$selectradii_walk) #set the radius for the area of interest
    dataset_walk<-
      load_walk()
    
    
    
    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling walk data...")
      # Long Running Task - walk data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
        for(y in 1:length(years)){
          year<-years[y]
          print(year)
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_walk$id) != 0){
                if(id %in% dataset_walk$id[dataset_walk$year==year & dataset_walk$radius==radius]) next #skip the row if the data is already there
              }
              
              dataset_walk<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')%>%dplyr::select(walk_vars[1]:radius)
              suppressMessages(
                suppressWarnings(
                  environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                                  external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
                )
              )
              walk_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_walk<-rbind(dataset_walk, walk_measures)
              
              write.csv(dataset_walk, 'ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')
              
              
              
            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), 'ACMT_Shiny_App/data_pull_summaries/walk_summary.csv')
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), 'ACMT_Shiny_App/data_pull_summaries/walk_missingness.csv')
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #### Udpate status message & data preview ####    
  output$dataset_walk_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_walk,{
    print("Status")
    print('Current ID')
    
    #showNotification(id='status_notif', paste(get_status(), get_current_id()), type='message', duration=8)
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')==TRUE){
      preview_walk$data=read.csv('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_walk$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_walk.csv')==FALSE){
      preview_walk$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_walk<-DT::renderDataTable({preview_walk$data}, editable=FALSE, 
                                           rownames=FALSE,
                                           options = list(
                                             searching = FALSE,
                                             pageLength = 10,
                                             #dom = 't', 
                                             autowidth=FALSE,
                                             scrollX=TRUE
                                             #,
                                             #columnDefs = list(list(targets='_all', width='900px'))
                                           )
  )
  
  # interrupt data pull
  observeEvent(input$stop_walk,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })
  
  
  
  
  
  # ======== MRFEI  Server Functions -------------------------------
  # generate desriptive text
  output$mrfei_description<-renderText(mrfei_description)
  preview_mrfei<-reactiveValues(data=data.frame())
  
  #### pull or create mrfei data (automatically with load data) ####
  observeEvent(input$pull_mrfei,{
    if(is.null(input$selectradii_mrfei)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')==FALSE){
      showNotification('Creating mrfei data frame', duration=5, type='message', id='process_notif') 
      dataset_mrfei<-create_dataset(variable_list=mrfei_vars)
      write.csv(dataset_mrfei, 'ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')==TRUE & 
       !is.null(input$selectradii_mrfei)){
      showNotification('Importing mrfei data frame', duration=5, type='message', id='status_notif')
    }
  })
  
  load_mrfei<-reactive({
    dataset_mrfei<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(mrfei_vars[1]:radius)
    dataset_mrfei
  })
  
  #### Data pull process: MRFEI #### 
  observeEvent(input$pull_mrfei,{
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/mrfei_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_mrfei)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling mrfei data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of mrfei data, using geocoded data: 
    #loop to pull mrfei data: 
    years<-as.numeric(mrfei_years)
    radius_vector <- as.numeric(input$selectradii_mrfei) #set the radius for the area of interest
    dataset_mrfei<-
      load_mrfei()
    
    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling mrfei data...")
      # Long Running Task - mrfei data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
        for(y in 1:length(years)){
          year<-years[y]
          print(year)
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_mrfei$id) != 0){
                if(id %in% dataset_mrfei$id[dataset_mrfei$year==year & dataset_mrfei$radius==radius]) next #skip the row if the data is already there
              }
              
              dataset_mrfei<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(mrfei_vars[1]:radius)
              suppressMessages(
                suppressWarnings(
                  environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                                  external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
                )
              )
              mrfei_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_mrfei<-rbind(dataset_mrfei, mrfei_measures)
              
              write.csv(dataset_mrfei, 'ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')
              
              
              
            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), 'ACMT_Shiny_App/data_pull_summaries/mrfei_summary.csv')
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), 'ACMT_Shiny_App/data_pull_summaries/mrfei_missingness.csv')
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #### Udpate status message & data preview ####    
  output$dataset_mrfei_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_mrfei,{
    print("Status")
    print('Current ID')
    
    #showNotification(id='status_notif', paste(get_status(), get_current_id()), type='message', duration=8)
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')==TRUE){
      preview_mrfei$data=read.csv('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_mrfei$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_mrfei.csv')==FALSE){
      preview_mrfei$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_mrfei<-DT::renderDataTable({preview_mrfei$data}, editable=FALSE, 
                                            rownames=FALSE,
                                            options = list(
                                              searching = FALSE,
                                              pageLength = 10,
                                              #dom = 't', 
                                              autowidth=FALSE,
                                              scrollX=TRUE
                                              #,
                                              #columnDefs = list(list(targets='_all', width='900px'))
                                            )
  )
  
  # interrupt data pull
  observeEvent(input$stop_mrfei,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })
  
  
  
  
  
  # ======== CDC Places Server Functions ============================
  
  # generate desriptive text
  output$cdc_description<-renderText(cdc_description)
  preview_cdc<-reactiveValues(data=data.frame())
  
  #### pull or create cdc data (automatically with load data) ####
  observeEvent(input$pull_cdc,{
    if(is.null(input$selectradii_cdc)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')==FALSE){
      showNotification('Creating cdc data frame', duration=5, type='message', id='process_notif') 
      dataset_cdc<-create_dataset(variable_list=cdc_vars)
      write.csv(dataset_cdc, 'ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')==TRUE & 
       !is.null(input$selectradii_cdc)){
      showNotification('Importing cdc data frame', duration=5, type='message', id='status_notif')
    }
  })
  
  load_cdc<-reactive({
    dataset_cdc<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(cdc_vars[1]:radius)
    dataset_cdc
  })
  
  #### Data pull process: cdc #### 
  observeEvent(input$pull_cdc,{
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/cdc_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_cdc)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling cdc data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of cdc data, using geocoded data: 
    #loop to pull cdc data: 
    years<-as.numeric(input$selectyear_cdc)
    radius_vector <- as.numeric(input$selectradii_cdc) #set the radius for the area of interest
    dataset_cdc<-
      load_cdc()
    
    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling cdc data...")
      # Long Running Task - cdc data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
        for(y in 1:length(years)){
          year<-years[y]
          print(year)
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_cdc$id) != 0){
                if(id %in% dataset_cdc$id[dataset_cdc$year==year & dataset_cdc$radius==radius]) next #skip the row if the data is already there
              }
              
              dataset_cdc<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(cdc_vars[1]:radius)
              suppressMessages(
                suppressWarnings(
                  environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                                  external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
                )
              )
              cdc_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_cdc<-rbind(dataset_cdc, cdc_measures)
              
              write.csv(dataset_cdc, 'ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')
              
              
              
            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), 'ACMT_Shiny_App/data_pull_summaries/cdc_summary.csv')
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), 'ACMT_Shiny_App/data_pull_summaries/cdc_missingness.csv')
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #### Udpate status message & data preview ####    
  output$dataset_cdc_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_cdc,{
    print("Status")
    print('Current ID')
    
    #showNotification(id='status_notif', paste(get_status(), get_current_id()), type='message', duration=8)
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')==TRUE){
      preview_cdc$data=read.csv('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_cdc$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_cdc.csv')==FALSE){
      preview_cdc$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_cdc<-DT::renderDataTable({preview_cdc$data}, editable=FALSE, 
                                            rownames=FALSE,
                                            options = list(
                                              searching = FALSE,
                                              pageLength = 10,
                                              #dom = 't', 
                                              autowidth=FALSE,
                                              scrollX=TRUE
                                              #,
                                              #columnDefs = list(list(targets='_all', width='900px'))
                                            )
  )
  
  # interrupt data pull
  observeEvent(input$stop_cdc,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })
  
  
  
  
  
  
  
  
  
  # ======== ParkServe Server Functions ============================
  
  # generate desriptive text
  output$parks_description<-renderText(parks_description)
  preview_parks<-reactiveValues(data=data.frame())
  
  #### pull or create parks data (automatically with load data) ####
  observeEvent(input$pull_parks,{
    if(is.null(input$selectradii_parks)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')==FALSE){
      showNotification('Creating parks data frame', duration=5, type='message', id='process_notif') 
      dataset_parks<-create_dataset(variable_list=parks_vars)
      write.csv(dataset_parks, 'ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')==TRUE & 
       !is.null(input$selectradii_parks)){
      showNotification('Importing parks data frame', duration=5, type='message', id='status_notif')
    }
  })
  
  load_parks<-reactive({
    dataset_parks<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')%>%dplyr::select(parks_vars[1]:radius)
    dataset_parks
  })
  
  #### Data pull process: parks #### 
  observeEvent(input$pull_parks,{
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/park_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_parks)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling parks data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of parks data, using geocoded data: 
    #loop to pull parks data: 
    years<-as.numeric(input$selectyear_parks)
    radius_vector <- as.numeric(input$selectradii_parks) #set the radius for the area of interest
    dataset_parks<-
      load_parks()
    
    #Download & Process parks data  
    if(exists('park_shp')==TRUE){
      showNotification('Park shapefile has already been created -- ready to pull data', duration=5, id='status_notif')
    }
    
    if(exists('park_shp')==FALSE){
      showNotification('loading parks data (this may take around 20 minutes', duration=10, id='status_notif')
      fire_running("Currently downloading ParkServe data")
      park_shp<-prepare_park_data()
      removeNotification(id='status_notif')
      showNotification('Raw ParkServe data has downloaded and processed, now pulling measures for dataset', duration=10, id='status_notif')
    }
    fire_running()
    
    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling parks data...")
      # Long Running Task - parks data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
        for(y in 1:length(years)){
          year<-years[y]
          print(year)
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_parks$id) != 0){
                if(id %in% dataset_parks$id[dataset_parks$year==year & dataset_parks$radius==radius]) next #skip the row if the data is already there
              }
              
              dataset_parks<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')%>%dplyr::select(parks_vars[1]:radius)
              suppressMessages(
                suppressWarnings(
                  environmental_measures<-data.frame(park_proportion=get_proportion_in_shapefile(long=longitude, lat=latitude, radius_meters=radius, shp_processed = park_shp), 
                                                     distance_park=get_distance_to_shapefile(long=longitude, lat=latitude, radius_meters=radius, shp_processed = park_shp))
                )
              )
              parks_measures<-environmental_measures %>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_parks<-rbind(dataset_parks, parks_measures)
              
              write.csv(dataset_parks, 'ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')

            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), 'ACMT_Shiny_App/data_pull_summaries/parks_summary.csv')
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), 'ACMT_Shiny_App/data_pull_summaries/parks_missingness.csv')
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #### Udpate status message & data preview ####    
  output$dataset_parks_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_parks,{
    print("Status")
    print('Current ID')
    
    #showNotification(id='status_notif', paste(get_status(), get_current_id()), type='message', duration=8)
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')==TRUE){
      preview_parks$data=read.csv('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_parks$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_parks.csv')==FALSE){
      preview_parks$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_parks<-DT::renderDataTable({preview_parks$data}, editable=FALSE, 
                                          rownames=FALSE,
                                          options = list(
                                            searching = FALSE,
                                            pageLength = 10,
                                            #dom = 't', 
                                            autowidth=FALSE,
                                            scrollX=TRUE
                                            #,
                                            #columnDefs = list(list(targets='_all', width='900px'))
                                          )
  )
  
  # interrupt data pull
  observeEvent(input$stop_parks,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })
  
  
  
  
  
  
  
  
  
  
  # ======== NLCD Server Functions ============================
  
  # generate desriptive text
  output$nlcd_description<-renderText(nlcd_description)
  preview_nlcd<-reactiveValues(data=data.frame())
  
  #### pull or create nlcd data (automatically with load data) ####
  observeEvent(input$pull_nlcd,{
    if(is.null(input$selectradii_nlcd)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')==TRUE & 
       !is.null(input$selectradii_nlcd)){
      showNotification('Importing nlcd data frame', duration=5, type='message', id='status_notif')
    }
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')==FALSE){
      showNotification('Creating nlcd data frame', duration=5, type='message', id='process_notif') 
      dataset_nlcd<-create_dataset(variable_list=nlcd_vars)
      write.csv(dataset_nlcd, 'ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')
    }
  })
  
  load_nlcd<-reactive({
    dataset_nlcd<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(nlcd_vars[1]:radius)
    dataset_nlcd
  })
  
  #### Data pull process: nlcd #### 
  observeEvent(input$pull_nlcd,{
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/nlcd_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_nlcd)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling nlcd data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of nlcd data, using geocoded data: 
    #loop to pull nlcd data: 
    years<-as.numeric(input$selectyear_nlcd)
    radius_vector <- as.numeric(input$selectradii_nlcd) #set the radius for the area of interest
    dataset_nlcd<-
      load_nlcd()
    
    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector)*length(years))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling nlcd data...")
      # Long Running Task - nlcd data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
        for(y in 1:length(years)){
          year<-years[y]
          print(year)
          nlcd_data<-pull_nlcd_data(year=year, label='nlcd landcover')
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_nlcd$id) != 0){
                if(id %in% dataset_nlcd$id[dataset_nlcd$year==year & dataset_nlcd$radius==radius]) next #skip the row if the data is already there
              }
              
              dataset_nlcd<-read.csv('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(nlcd_vars[1]:radius)
              suppressMessages(
                suppressWarnings(
                  environmental_measures<-pull_nlcd_measures(longitude, latitude, radius, variable_list=nlcd_vars, nlcd_data=nlcd_data)
                )
              )
              nlcd_measures<-environmental_measures %>% dplyr::select(legend, Freq) %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_nlcd<-rbind(dataset_nlcd, nlcd_measures)
              
              write.csv(dataset_nlcd, 'ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')
              
            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), 'ACMT_Shiny_App/data_pull_summaries/nlcd_summary.csv')
      write.csv(read.csv('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), 'ACMT_Shiny_App/data_pull_summaries/nlcd_missingness.csv')
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #### Udpate status message & data preview ####    
  output$dataset_nlcd_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_nlcd,{
    print("Status")
    print('Current ID')
    
    #showNotification(id='status_notif', paste(get_status(), get_current_id()), type='message', duration=8)
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')==TRUE){
      preview_nlcd$data=read.csv('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_nlcd$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists('ACMT_Shiny_App/data_pull_measures/dataset_nlcd.csv')==FALSE){
      preview_nlcd$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_nlcd<-DT::renderDataTable({preview_nlcd$data}, editable=FALSE, 
                                            rownames=FALSE,
                                            options = list(
                                              searching = FALSE,
                                              pageLength = 10,
                                              #dom = 't', 
                                              autowidth=FALSE,
                                              scrollX=TRUE
                                              #,
                                              #columnDefs = list(list(targets='_all', width='900px'))
                                            )
  )
  
  # interrupt data pull
  observeEvent(input$stop_nlcd,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })

  # ======== American Community Survey  Server Functions -------------------------------
  # generate desriptive text
  output$acs_description<-renderText(acs_description)
  preview_acs<-reactiveValues(data=data.frame())
  
  #### pull or create acs data (automatically with load data) ####
 # observeEvent(input$pull_acs,{
    
    # get list of ACS Variables to pull for given year: 
    # acs_info<-get_acs_standard_columns(year=
    #                                      input$selectyear_acs
    # )
    # acs_columns<-acs_info$acs_columns
    # acs_count_pretty_names <- acs_columns$pretty_name_count  # all have pretty name for count
    # acs_proportion_pretty_names <- acs_columns$pretty_name_proportion[acs_columns$universe_col != ""]  # only non-universal varaibles have pretty name for proportion
    # acs_vars <- c(as.character(acs_columns$acs_col), as.character(acs_columns$universe_col))  #  acs_columns$acs_col is a superset of acs_columns$universe_col
    # 
    # unique_var_cols <- unique(acs_vars)
    # codes_of_acs_variables_to_get=acs_columns$acs_col
    # 
    # ##Set interpolation designations (from acs_columns dataframe) ##
    # acs_variable_name_to_interpolate_by_sum_boolean_mapping<-acs_columns$acs_variable_name_to_interpolate_by_sum_boolean_mapping
    # names(acs_variable_name_to_interpolate_by_sum_boolean_mapping)<-as.character(acs_columns$acs_col)
    # 
    # if(is.null(input$selectradii_acs)){
    #   showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    # }
    # if(file.exists('ACMT_Shiny_App/dataset_geocoded.csv')==FALSE){
    #   showNotification('Upload geocoded data (or use the ACMT geocoder) to pull environmental measures', type='warning')
    # }
    # 
    # if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==FALSE){
    #   showNotification('Creating acs data frame', duration=5, type='message', id='process_notif') 
    #   dataset_acs<-create_dataset(variable_list=acs_vars)
    #   write.csv(dataset_acs, paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))
    # }
    # if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==TRUE & 
    #    !is.null(input$selectradii_acs)){
    #   showNotification('Importing acs data frame', duration=5, type='message', id='status_notif')
    # }
    # head(acs_vars)
    
 # })
  
  load_acs<-reactive({
    dataset_acs<-read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))%>%dplyr::select(acs_vars[1]:radius)
    dataset_acs<-read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_2015.csv'))#%>%dplyr::select(acs_vars[1]:radius)
    
    dataset_acs
  })
  
  #### Data pull process: ACS #### 
  observeEvent(input$pull_acs,{
    
    
    # get list of ACS Variables to pull for given year: 
   acs_vars<-get_default_acs_var_list(year=input$selectyear_acs)

    ##Set interpolation designations (from acs_columns dataframe) ##
    # acs_variable_name_to_interpolate_by_sum_boolean_mapping<-acs_columns$acs_variable_name_to_interpolate_by_sum_boolean_mapping
    # names(acs_variable_name_to_interpolate_by_sum_boolean_mapping)<-as.character(acs_columns$acs_col)
    # print('acs boolean mapping list')
    # print(head(acs_variable_name_to_interpolate_by_sum_boolean_mapping))
    
    if(is.null(input$selectradii_acs)){
      showNotification('Select one or more radius to pull data', duration=5, type='message', id='status_notif')
    }
    if(file.exists('ACMT_Shiny_App/dataset_geocoded.csv')==FALSE){
      showNotification('Upload geocoded data (or use the ACMT geocoder) to pull environmental measures', type='warning')
    }
    
    if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==FALSE){
      showNotification('Creating acs data frame', duration=5, type='message', id='process_notif') 
      dataset_acs<-create_dataset(variable_list=acs_vars)
      write.csv(dataset_acs, paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))
    }
    if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==TRUE & 
       !is.null(input$selectradii_acs)){
      showNotification('Importing acs data frame', duration=5, type='message', id='status_notif')
    }
    print('head of acs_var list')
    print(head(acs_vars))
    
    #load dataset_geocoded from the R environment
    source('ACMT_Shiny_App/data_pull_settings/acs_data_settings.R')
    dataset_geocoded<-loadData('dataset_geocoded.csv')%>%dplyr::select(id, lat, long)%>%filter(!is.na(lat)&!is.na(long))
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already pulling data", type='warning')
      return(NULL)
    }
    
    if(is.null(input$selectradii_acs)){
      showNotification("Select at least one buffer radius", type='warning', id='status_notif')
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    
    result_val(data.frame(Status="Pulling acs data..."))
    
    fire_running()
    
    ##reactive values here:
    #initial pull of acs data, using geocoded data: 
    #loop to pull acs data: 
    year<-as.numeric(input$selectyear_acs)
    radius_vector <- as.numeric(input$selectradii_acs) #set the radius for the area of interest
    dataset_acs<-
      load_acs()

    N<-nrow(dataset_geocoded)
    N2<-nrow(dataset_geocoded)*(length(radius_vector))
    
    #future promise loop here:
    result <- future::future({
      print("Pulling acs data...")
      # Long Running Task - acs data pull
      for(i in 1:N){
        print(paste0("Currently processing ", i, ' out of ', N))
        #check for interrupted data process:
        id<-dataset_geocoded$id[i]
        latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
        longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
        print(id) #print the number to keep track of progress
        print(c(latitude, longitude))
        result_val(data.frame(Status=paste("Pulling data for", id)))
        
         # for(y in 1:length(years)){
        #   year<-years[y]
          print(year)
          
          for(r in 1:length(radius_vector)){
            # Check for user interrupts
            if(interrupted()){ 
              print("Stopping...")
              stop("User Interrupt")
              removeNotification(id='stop_message')
              #set clicks back to 0
              nclicks(0)
            }
            
            radius<-radius_vector[r]
            print(radius)
            
            # Notify status file of progress
            fire_running(round(100*i/N, 2))
            auto_status(id=dataset_geocoded$id[i], radius, year)
            
            #check for existing data in dataset:
            tryCatch({
              if(length(dataset_acs$id) != 0){
                if(id %in% dataset_acs$id[dataset_acs$year==year & dataset_acs$radius==radius]) next #skip the row if the data is already there
              }
              print('load in acs dataset')
              dataset_acs<-read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', year, '.csv'))%>%dplyr::select(-any_of('X'))
              #suppressMessages(
                #suppressWarnings(
                  environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NA, 
                                                                  external_data_name_to_info_list=NULL, fill_missing_GEOID_with_zero = TRUE)
                #)
              #)
              acs_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
              
              #combine 
              dataset_acs<-rbind(dataset_acs, acs_measures)
              
              write.csv(dataset_acs, paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', year, '.csv'))
              
              
              
            },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
            
          }# end of radius loop
        # }# end of year loop
      } #end of ID loop
      
      # export summary tables at the end of the data pull
      write.csv(read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', year, '.csv'))%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_summary(.), paste0('ACMT_Shiny_App/data_pull_summaries/acs_summary_', year, '.csv'))
      write.csv(read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', year, '.csv'))%>%dplyr::select(id, radius, year, everything())%>%
                  dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)%>%table_missingness(.), paste0('ACMT_Shiny_App/data_pull_summaries/acs_missingness_', year, '.csv'))
      
      # Some results
    }) %...>% result_val()
    
    # Catch interrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      removeNotification(id='stop_message')
                      showNotification(e$message, type='warning', id='status_notif')
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for another Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  
  #### Udpate status message & data preview ####    
  output$dataset_acs_message<-renderTable(
    if(is.null(result_val())){data.frame(Status='Ready to pull data')}
    else{
      req(result_val())
    }, 
    colnames=TRUE
  )
  
  # Show status notifications
  observeEvent(input$status_acs,{
    print("Status")
    print('Current ID')
    
    showNotification(id='process_notif', get_current_id(), type='message', duration=8)
    
    if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==TRUE){
      preview_acs$data=read.csv(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', 
                                       input$selectyear_acs, 
                                       #year, 
                                       '.csv'))%>%dplyr::select(id, radius, year, everything())%>%
        dplyr::select(-X)%>%mutate_if(is.numeric, round, digits=3)#%>%dplyr::select(id, radius, year)%>%tail(10)
      #get % complete: 
      fire_running(
        round(100*nrow(preview_acs$data%>%dplyr::select(id)%>%unique())/nrow(loadData('dataset_geocoded.csv')),0))
    }
    if(file.exists(paste0('ACMT_Shiny_App/data_pull_measures/dataset_acs_', input$selectyear_acs, '.csv'))==FALSE){
      preview_acs$data=data.frame(message='No data pulled yet, click the Pull data button to create dataset and begin pulling environmental measures')}
    
    #update status
    result_val(data.frame(Status=paste(get_status())))
    
  })
  
  # render data preview
  output$dataset_acs<-DT::renderDataTable({preview_acs$data}, editable=FALSE, 
                                           rownames=FALSE,
                                           options = list(
                                             searching = FALSE,
                                             pageLength = 10,
                                             #dom = 't', 
                                             autowidth=FALSE,
                                             scrollX=TRUE
                                             #,
                                             #columnDefs = list(list(targets='_all', width='900px'))
                                           )
  )
  
  # interrupt data pull
  observeEvent(input$stop_acs,{
    if(get_status()=='Ready to pull data'){
      showNotification('Data Pull is not currently running', duration=5, type='warning')
    }
    else{
      showNotification('Stopping data pull, please wait', duration=NULL, id='stop_message', type='error')
      print("Cancel")
      fire_interrupt()
    }
  })
  
  
  
# ----- Section 4: Data summaries & correlation analysis #   
  # ======== Data Summary Table ============================================
  # create the progress summmary function #  
  progress_summary<-function(){  
    
    files <- list.files(path="ACMT_Shiny_App/data_pull_measures", pattern="dataset", full.names=TRUE)
    
    if(length(files)==0){
      data_summary<-list(Status=data.frame(total_n='No data pulled'))
    }
    
    if(length(files)>0){
      #get dataset names
      dataset_names<-lapply(files, function(x) {
        toupper(gsub("\\..*", "", str_replace(x, '(.*?)dataset_(.*?)', ''))) # get dataset name
      })
      
      data_summary<-lapply(files, function(x) {
        t <- read.csv(x, header=TRUE) # load file
        # apply function
        t %>% group_by(year, radius) %>% dplyr::summarise(total_n=n())
      })
      
      names(data_summary)<-dataset_names
    }
    lapply(names(data_summary), function(x) {
      table_title<-as.character(x)
      output[[x]] = DT::renderDataTable({data_summary[[x]]}, 
                                    editable=FALSE, rownames=FALSE,
                                    caption=table_title,
                                    options=list(dom = 't'))
    })
    
    output$tables <- renderUI({
      lapply(names(data_summary), dataTableOutput)
    })
  }
  
  #show table when the page loads
  progress_summary()
  
  #refresh table with the button
  observeEvent(input$refreshprogress, {
    progress_summary()})
  
  observeEvent(input$create_report, {
    withProgress(
      source('ACMT_Shiny_App/summary_tables_ACMT.R'), 
      message='Creating final summary report'
    )
    showNotification('Data Summary Report Complete', id='status_notif', duration=NULL)
  })
  
  
  
  # ======== Correlation Analysis =====================================
  # show uploaded datafile
  #corrplot<-reactiveValues(data=data.frame(Message='Select values and hit the correlation plot button'))

  radius_list<-reactiveValues(list=c(500, 1000, 5000))
  year_list<-reactiveValues(list=c(2017))
  
  #update radii & year list based on selected datasets
  observe({
    print(input$selectdatasets_corr)
     if(is.null(input$selectdatasets_corr))
      return()
    isolate({
      updatePickerInput(session, 'selectradii_corr', 
                        label='Select radius to include in correlation analysis',
                        choices=all_radii_of_data(dataset_names_list=input$selectdatasets_corr)$radius)#, ### NOT WORKING -- UPDATE
                        #options = list(`actions-box` = TRUE),multiple = T) 
      updatePickerInput(session, 'selectyear_corr', 
                        label='', 
                        choices=all_years_of_data(dataset_names_list=input$selectdatasets_corr)$year)#, ### NOT WORKING -- UPDATE
                        #options = list(`actions-box` = TRUE),multiple = T)
    })
  })
  
  observeEvent(input$selectdatasets_corr, {
    output$randomCorrPlot<<-NULL
  })
  
  observeEvent(input$run_corr_analysis, {
    output$randomCorrPlot<-renderPlot({
      #random_correlations(corr_number = 5, years=2017, radius_meters=1000, dataset_names=dataset_names_list)
      random_correlations(corr_number = input$selectcorrNumber, years=input$selectyear_corr, radius_meters=input$selectradii_corr, dataset_names=input$selectdatasets_corr)
    })
  })
  
  

   }
shinyApp(ui, server)
