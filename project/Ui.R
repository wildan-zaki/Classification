library("shinythemes") #v
library("leaflet") #v
library("markdown") #v
library("shiny")
library("shinyjs")
library("shinyBS")

shinyUI(
  div(fluidPage(style = 'height:5500px;', theme = shinytheme("journal"),
                headerPanel(h1("Aplikasi Klasifikasi Data Citra", align = "center", theme = shinytheme("slate")),
                windowTitle = "Aplikasi klasifikasi data citra"),
              useShinyjs(),
                #shinythemes::themeSelector(),
                tags$style(type = "text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
      tabsetPanel(type = "pills",
            tabPanel("Haze Trajectory", hr(),
                  tabsetPanel(type = "tab",
                                       tabPanel('Initial point from CSV file',sidebarLayout(fluid = TRUE,
                                                                                            sidebarPanel(h4('Upload Periode I'),
                                                                                                         h5('Initial Point :'),
                                                                                                         fileInput('datafilecsv', 'Upload the file'),
                                                                                                         br(),
                                                                                                         
                                                                                                         actionButton('go_sim3', 'Start Simulation'),width = 3),
                                                                                            
                                                                                            mainPanel(fluid = TRUE,h3(textOutput('caption_traj3')),
                                                                                                      h5('Description and visualization of trajectory simulation results'),
                                                                                                      leafletOutput('showplotleafletcsv', height = 500),
                                                                                                      h4(textOutput('titlecheck')),
                                                                                                      dataTableOutput('checkstatus'),
                                                                                                      h4(textOutput('titleSimulation')),
                                                                                                      dataTableOutput('simulation_trajcsv'))
                                       )),
                                       
                                       
                                       
                                       tabPanel('Haze Cluster',sidebarLayout(fluid = TRUE,
                                                                             sidebarPanel(h4('Clustering ST-DBSCAN'),
                                                                                          sliderInput("eps1_traj", "ST-DBSCAN EPS 1 (0.01 - 0.2)",
                                                                                                      min = 0.01, max = 0.2, value = "", step = 0.01),
                                                                                          sliderInput("eps2_traj", "ST-DBSCAN EPS 2 (1 - 30)",
                                                                                                      min = 1, max = 30, value = "", step = 1),
                                                                                          sliderInput("minPts_traj", 'ST-DBSCAN MinPts (3 - 7)',
                                                                                                      min = 3, max = 7, value = "", step = 1),
                                                                                          br(),
                                                                                          actionButton('go_clusttraj', 'Start Clustering ST-DBSCAN'),width = 3),
                                                                             
                                                                             mainPanel(fluid = TRUE,h3(textOutput('caption_traj_clust')),
                                                                                       h5('Description and visualization of cluster trajectory result from ST-DBSCAN algorithm'),
                                                                                       leafletOutput('showplotleaflet2', height = 500),
                                                                                       dataTableOutput('stat_traj_table'),
                                                                                       dataTableOutput('summary_clust_table'))))
                           )),              
     
  #--------------------------------------------------------------------------------------------------           
    tabPanel("Haze Pollution", hr(),
      tabsetPanel(type = "tab",
      tabPanel('Simulation',
      sidebarLayout(fluid = TRUE,
      sidebarPanel(h4('Simulation HYSPLIT Consentration'),
      selectInput('date_Period2', 'Hotspot Sequence', 
                 choices = c(Choose = '', 
                                           'Sumatra 03 September - 04 September 2015' = 'period215gr7','Sumatra 22 September - 24 September 2015' = 'period215gr8', 'Sumatra 22 September - 26 September 2015' = 'period215gr9', 
                                           'Sumatra 24 September - 26 September 2015' = 'period215gr10','Sumatra 01 October - 03 October 2015' = 'period215gr11',
                                           'Sumatra 05 October - 08 October 2015' = 'period15gr212','Sumatra 19 October - 22 October 2015' = 'period215gr14',
                                           'Sumatra 19 October - 21 October 2015' = 'period215gr13','Sumatra 21 October - 24 October 2015' = 'period215gr16',
                                           'Sumatra 21 October - 22 October 2015' = 'period215gr15','Sumatra 23 October - 24 October 2015' = 'period215gr17',
                                           'Sumatra 24 October - 26 October 2015' = 'period215gr18'), selectize = TRUE),
      
      actionButton('go_cons', 'Start Simulation'),width = 3),
      mainPanel(fluid = TRUE,h3(textOutput('caption_const')),
      h5('Description and visualization of simulated pollutant consentration results'),
      leafletOutput('showplotleaflet3', height = 500),dataTableOutput('simulation_const'))
      )),
                         
      tabPanel('Cluster ST-DBSCAN',sidebarLayout(fluid = TRUE,
        sidebarPanel(h4('Clustering ST-DBSCAN'),
          sliderInput("eps1_cons", "ST-DBSCAN EPS 1 (0.01 - 0.2)",
              min = 0.01, max = 0.2, value = "", step = 0.01),
          sliderInput("eps2_cons", "ST-DBSCAN EPS 2 (1 - 30)",
              min = 1, max = 30, value = "", step = 1),
          sliderInput("minPts_cons", 'ST-DBSCAN MinPts (3 - 7)',
              min = 3, max = 7, value = "", step = 1),
              br(),
          actionButton('go_clustST', 'Start Clustering ST-DBSCAN'),width = 3),
        mainPanel(fluid = TRUE,h3(textOutput('caption_const_clust_stdbscan')),
          h5('Description and visualization of cluster pollutant concentration results from ST-DBSCAN algorithm'),
          leafletOutput('showplotleaflet4', height = 500),
          dataTableOutput('stat_cons_table_stdbscan'),
          dataTableOutput('summary_clust_table_stdbscan'))
                                  )),
                         
      tabPanel('Cluster K-Means',sidebarLayout(fluid = TRUE,
        sidebarPanel(h4('Clustering K-Means'),
          numericInput('Cluster_STDBSCAN', 'Cluster', value = "", min = 0, max = 30, step = 1),
          sliderInput("K_cons", " number of clusters K (2 - 30)",
              min = 2, max = 30, value = "", step = 1),
              br(),
          actionButton('go_clustKM', 'Start Clustering K-Means'),width = 3),
        mainPanel(fluid = TRUE,h3(textOutput('caption_const_clust_kmeans')),
          h5('Description of cluster pollutant concentration results from K-Means algorithm'),
          leafletOutput('showplotleaflet5', height = 500),
          dataTableOutput('stat_cons_table_kmeans'),
          dataTableOutput('summary_clust_table_kmeans'))
                                  )
                         ))),
    #----------------------------------------------------------------------------------------------
    tabPanel("Haze Pollutants Visualization", hr(),
      tabsetPanel(type = "tab",
          tabPanel('Visual Simulation',sidebarLayout(fluid = TRUE,
          sidebarPanel(h4('Visual Simulation HYSPLIT Trajectory and Consentration'),
              selectInput('date_Period3', 'Hotspot Sequence (Initial Point)', choices = c(Choose = '', 'Sumatra 09 Juli - 11 Juli 2015' = 'period315gr1', 'Sumatra 22 Juli - 23 Juli 2015' = 'period315gr2',
                                                                                                                              'Sumatra 26 Juli - 29 Juli 2015' = 'period315gr3', 'Sumatra 30 Agustus - 01 September 2015' = 'period315gr4',
                                                                                                                              'Sumatra 01 September - 02 September 2015' = 'period315gr5', 'Sumatra 21 Oktober - 22 Oktober 2015' = 'period315gr6'), selectize = TRUE),
              actionButton('go_vis', 'Start Visual Simulation'),width = 3),
              mainPanel(fluid = TRUE,h3(textOutput('caption_vis')),
                h5('Visualization of cluster patterns from trajectory and pollutant concentrations on ST-DBSCAN and K-Means algorithms'),
                br(),
                h5('Visualization of HYSPLIT model simulation results from trajectory and pollutant concentration'),
                leafletOutput('showplotleaflet6', height = 600))
                )),
          tabPanel('Visual Cluster ST-DBSCAN',sidebarLayout(fluid = TRUE,
            sidebarPanel(h4('Visual Cluster Trajectory ST-DBSCAN'),
              sliderInput("eps1_traj_vis", "ST-DBSCAN EPS 1 (0.01 - 0.2)",
                min = 0.01, max = 0.2, value = "", step = 0.01),
              sliderInput("eps2_traj_vis", "ST-DBSCAN EPS 2 (1 - 30)",
                min = 1, max = 30, value = "", step = 1),
              sliderInput("minPts_traj_vis", 'ST-DBSCAN MinPts (3 - 7)',
                min = 3, max = 7, value = "", step = 1),
            h4('Visual Cluster Consentration ST-DBSCAN'),
              sliderInput("eps1_cons_vis", "ST-DBSCAN EPS 1 (0.01 - 0.2)",
                  min = 0.01, max = 0.2, value = "", step = 0.01),
              sliderInput("eps2_cons_vis", "ST-DBSCAN EPS 2 (1 - 30)",
                  min = 1, max = 30, value = "", step = 1),
              sliderInput("minPts_cons_vis", 'ST-DBSCAN MinPts (3 - 7)',
                  min = 3, max = 7, value = "", step = 1),
              br(),
           actionButton('go_clustvis_stdbscan', 'Start Visual Clustering'),width = 3),
           mainPanel(fluid = TRUE,h3(textOutput('caption_vis_clust_stdbscan')),
               h5('Visualization of cluster patterns from trajectory and pollutant concentrations on ST-DBSCAN and K-Means algorithms'),
               br(),
               h5('Visualization of cluster patterns from trajectory and pollutant concentrations on ST-DBSCAN algorithm'),
               leafletOutput('showplotleaflet7', height = 600))
                                  )),
                         
          tabPanel('Visual Cluster K-Means',
            sidebarLayout(fluid = TRUE,sidebarPanel(
              h4('Visual Cluster Consentration K-Means'),
              numericInput('Cluster_STDBSCAN_cons_vis', 'Cluster', value = "", min = 0, max = 30, step = 1),
              sliderInput("K_cons_vis", " number of clusters K (2 - 30)",
                 min = 2, max = 30, value = "", step = 1),
              br(),
                actionButton('go_clustvis_kmeans', 'Start Visual Clustering K-Means'),width = 3),
             mainPanel(fluid = TRUE,h3(textOutput('caption_vis_clust_kmeans')),
               h5('Visualization of cluster patterns from trajectory and pollutant concentrations on ST-DBSCAN and K-Means algorithms'),
               br(),
             h5('Visualization of cluster patterns of trajectory using the ST-DBSCAN algorithm and cluster patterns of pollutant concentrations using the K-Means algorithm'),
             leafletOutput('showplotleaflet8', height = 600))
                                  )
                         ))),
  #-----------------------------------------------------------------------------------------------
  tabPanel("Pollutant Visualization GFAS", hr(),
           tabsetPanel(type = "tab",     #calendar plot didasarkan polutan    
                  tabPanel('Summary Plot',sidebarLayout(fluid = TRUE,
                          sidebarPanel(h4('Summary Plot Pollutant Emissions'),
                                  selectInput('datecons', 'Period', choices = c(Choose = '', 'Sumatra, July - November 2015' = 'period415gr1'), selectize = TRUE),
                                  actionButton('go_consPul', 'Start'),width = 3, textOutput("warningSum")),
                                                    mainPanel(fluid = TRUE, bsAlert('alert1') , h3('Summary Plot Pollutant Emissions GFAS'),
                                                              h5('Summary statistics and plots of pollutant emissions in Sumatra'),
                                                              br(), plotOutput("plot1"))
                                                             
                       )),
                  
                  tabPanel('Calendar Plot',sidebarLayout(fluid = TRUE,
                                                         sidebarPanel(h4('Calendar Plot Pollutant Emission'),
                                                                      selectInput('date_Period61', 'Province', choices = c(Choose = '', 'All provinces'='all', 'Aceh' = 'ACEH', 'Bengkulu' = 'BENGKULU', 'Jambi' = 'JAMBI', 'Lampung'='LAMPUNG', 'Riau'='RIAU', 'West Sumatra' = 'SUMATERA BARAT', 'South Sumatra'='SUMATERA SELATAN', 'North Sumatra'='SUMATERA UTARA'), selectize = TRUE),
                                                                      selectInput('date_Period62', 'Pollutant', choices = c(Choose = '', 'CO' = 'co', 'CO2' = 'co2', 'CH4' = 'ch4', 'NH3'='nh3', 'PM2.5'='pm25'), selectize = TRUE),
                                                                      actionButton('go_calPulLoc', 'Start'),width = 3),
                                                         mainPanel(fluid = TRUE, h3('Calendar Plot Pollutant Emission GFAS'),
                                                                   h5('Plot pollutant emission province in Sumatra by month on conventional calendar format'),
                                                                   br(), plotOutput("plot3"))
                                                         
                  )),
                  tabPanel('Simulation Pollutant',sidebarLayout(fluid = TRUE,
                                                                     sidebarPanel(h4('Google Maps Plot Pollutant Emission'),
                                                                                  selectInput('date_Period71', 'Month', choices = c(Choose = '', 'July 2015' = '7', 'August 2015' = '8',
                                                                                                                                       'September 2015' = '9', 'October 2015' = '10', 
                                                                                                                                       'November 2015' = '11'), selectize = TRUE),
                                                                                  selectInput('date_Period72', 'Province', choices = c(Choose = '', 'Aceh' = 'ACEH', 'Bengkulu' = 'BENGKULU', 'Jambi' = 'JAMBI', 'Lampung'='LAMPUNG', 'Riau'='RIAU', 'West Sumatra' = 'SUMATERA BARAT', 'South Sumatra'='SUMATERA SELATAN', 'North Sumatra'='SUMATERA UTARA'), selectize = TRUE),
                                                                                  selectInput('date_Period73', 'Pollutant', choices = c(Choose = '', 'CO' = 'co', 'CO2' = 'co2', 'CH4' = 'ch4', 'NH3'='nh3', 'PM2.5'='pm25'), selectize = TRUE),
                                                                                
                                                                                  actionButton('go_maps', 'Start'),width = 3),
                                                                     mainPanel(fluid = TRUE, h3('Google Maps Plot Pollutant Emission GFAS in Sumatra'),
                                                                               h5('Description and visualization of simulated pollutant emission results'),
                                                                               br(),
                                                                               leafletOutput('plot4', height = 500),
                                                                               dataTableOutput('simulation_pol'))
                                                                     
                  )),
                  tabPanel('Cluster K-Means',sidebarLayout(fluid = TRUE,
                                                                sidebarPanel(h4('Clustering Pollutants Emission using K-Means'),
                                                                             selectInput('date_Period81', 'Month', choices = c(Choose = '', 'July 2015' = 'period815gr1', 'August 2015' = 'period815gr2',
                                                                                                                               'September 2015' = 'period815gr3', 'October 2015' = 'period815gr4', 
                                                                                                                               'November 2015' = 'period815gr5'), selectize = TRUE),
                                                                             selectInput('date_Period82', 'Province', choices = c(Choose = '', 'Aceh' = 'ACEH', 'Bengkulu' = 'BENGKULU', 'Jambi' = 'JAMBI', 'Lampung'='LAMPUNG', 'Riau'='RIAU', ' West Sumatra' = 'SUMATERA BARAT', 'South Sumatra'='SUMATERA SELATAN', 'North Sumatra'='SUMATERA UTARA'), selectize = TRUE),
                                                                              sliderInput("kvalue", " Number of clusters K (2 - 30)",
                                                                                         min = 2, max = 30, value = "", step = 1),
                                                                             br(),
                                                                             actionButton('go_clustpol', 'Start'),width = 3),
                                                                mainPanel(fluid = TRUE, h3('Clustering Pollutant Emissions GFAS in Sumatra using K-Means'),
                                                                          h5('Description and visualization of cluster pollutant emissions results from K-Means algorithm'),
                                                                          br(),
                                                                          leafletOutput('plot5', height = 600),
                                                                          dataTableOutput('summary_clust_pol'))
                                                                
                  )),
                  tabPanel('Summary Cluster K-Means',
                        h3('Summary of Cluster Pollutant Emissions GFAS in Sumatra using K-Means Algorithm'),
                        h5('Table of Summary Cluster Pollutant Emissions'),
                        br(),
                        column(5,dataTableOutput('sum_clust_pol'))
                                                           
                  )
                       
           )),
  tabPanel(" Haze Pollutant Visualization GFAS", hr(),
           tabsetPanel(type = "tab",     #calendar plot didasarkan polutan    
              tabPanel('Haze Pollutant Visualization GFAS',sidebarLayout(fluid = TRUE,
                    sidebarPanel(h4("Visualization Haze Trajectory and Pollutant Emission GFAS"),
                    selectInput('date_Period9', 'Hotspot Sequence', 
                                             choices = c(Choose = '', 
                                                         'Sumatra 03 September - 04 September 2015' = 'period915gr1','Sumatra 22 September - 24 September 2015' = 'period915gr2',  
                                                         'Sumatra 24 September - 26 September 2015' = 'period915gr3'), selectize = TRUE),
                    selectInput('date_Period92', 'Pollutant', choices = c(Choose = '', 'CO' = 'co', 'CO2' = 'co2', 'CH4' = 'ch4', 'NH3'='nh3', 'PM2.5'='pm25'), selectize = TRUE),
                    actionButton('go_vispol', 'Start'),width = 3),
                    mainPanel(fluid = TRUE,   h3('Visualization Haze Trajectory and Pollutant Emission GFAS in Sumatra'),
                    h5('Predict of pollutant emission on haze trajectory from simulated pollutant emission GFAS'),
                    br(), 
                    leafletOutput('plot6', height = 500))
              ))              
                      
           )),
                      
  tabPanel("Help", hr(),
           h3("How to Use The Trajectory Pattern Mining Analysis Application",align = "center"), br(),
           h4("Haze Trajectory : Analysis of simulation and clustering on trajectory data"),
           "Step on simulation trajectory sequance", br(),
           "1. Click the menu tab simulation trajectory sequance.", br(),
           "2. Select the dataset in the 'Hotspot Sequence' box.", br(),
           "3. Click 'Start Simulation' button", br(),
           "4. The simulation result is stated in google map and simulation result table", br(), br(),
           "Step on simulation trajectory input user", br(),
           "5. Click the menu tab simulation trajectory input user.", br(),
           "6. Select the dataset in the 'Hotspot Sequence' box.", br(),
           "7. Insert number of initial point.", br(),
           "8. Insert number of longitude and latitude with 3 digit decimal.", br(),
           "9. Click 'Start Simulation' button", br(),
           "10. If number of longitude and latitude doesn't exist in the database, system will display pop up warning message. Click dismiss button.", br(),
           "11. The simulation result is stated in google map and simulation result table", br(), br(),
           "Step in clustering using the ST-DBSCAN algorithm", br(),
           "11. Click tab menu Cluster.", br(),
           "12. Specify the parameter value of Eps1 for spatial distance. Eps1 is in the range of values between 0.01 to 0.2", br(),
           "13. Specify the parameter value of Eps2 for temporal distance. Eps2 is in the range of values between 0.01 to 0.2", br(),
           "14. Specify the parameter value MinPts to indicate the minimum number of data points in the cluster. Eps1 is in the range of values between 1 to 7", br(),
           "15. Click 'Start Clustering ST-DBSCAN' button", br(),
           "16. The cluster results are expressed in google maps, simulated results tables in clusters, and cluster summary tables", br(), br(),
           
           h4("Haze Pollution : Analysis of simulation and clustering on pollutant concentrations data"),
           "Step on the simulation of the pollutant concentration", br(),
           "1. Click the menu tab simulation.", br(),
           "2. Select the dataset in the 'Sequential Hotspot Data' box.", br(),
           "3. Click 'Start Simulation' button", br(),
           "4. The simulation result is stated in google map and simulation result table", br(), br(),
           "Steps in clustering pollutant concentrations using the ST-DBSCAN algorithm", br(),
           "5. Click tab menu Cluster ST-DBSCAN.", br(),
           "6. Specify the parameter value of Eps1 for spatial distance. Eps1 is in the range of values between 0.01 to 0.2", br(),
           "7. Specify the parameter value of Eps2 for temporal distance. Eps2 is in the range of values between 0.01 to 0.2", br(),
           "8. Specify the parameter value MinPts to indicate the minimum number of data points in the cluster. Eps1 is in the range of values between 1 to 7", br(),
           "9. Click 'Start Clustering ST-DBSCAN' button", br(),
           "10. The cluster results are expressed in google maps, simulated results tables in clusters, and cluster summary tables", br(), br(),
           "Steps in clustering pollutant concentrations using the K-Means algorithm", br(),
           "11. Click the menu Cluster K-Means.", br(),
           "12. Determine the cluster information result from ST-DBSCAN algorithm which will be clustered again using K-Means algorithm.", br(),
           "13. Specify the parameter value of K for the number of cluster centers to be used. K is in the range of values between 2 to 30", br(),
           "14. Click 'Start Clustering K-Means' button", br(),
           "15. The cluster results are expressed in google maps, simulated results tables in clusters, and cluster summary tables", br(), br(),
           
           h4("Haze Pollutants Visualization : Visualization of simulation results and cluster analysis of trajectory and pollutant concentrations"),
           "Step visualization of simulation results from trajectory and pollutant concentration", br(),
           "1. Click the menu tab Visual Simulation.", br(),
           "2. Select the dataset in the 'Sequential Hotspot Data' box.", br(),
           "3. Click 'Start Visual Simulation' button", br(),
           "4. Visualization in google maps from simulation results of trajectory and pollutant concentration", br(), br(),
           "Step visualization of cluster results of ST-DBSCAN algorithm on tarjectory and pollutant concentration", br(),
           "5. Click the menu tab Visual Cluster ST-DBSCAN.", br(),
           "6. Specify the parameter value of Eps1 for spatial distance of trajectory dataset. Eps1 is in the range of values between 0.01 to 0.2", br(),
           "7. Specify the parameter value of Eps2 for temporal distance of trajectory dataset. Eps2 is in the range of values between 0.01 to 0.2", br(),
           "8. Specify the parameter value MinPts to indicate the minimum number of data points in the cluster of trajectory dataset. Eps1 is in the range of values between 1 to 7", br(),
           "9. Specify the parameter value of Eps1 for spatial distance of pollutant concentration dataset. Eps1 is in the range of values between 0.01 to 0.2", br(),
           "10. Specify the parameter value of Eps2 for temporal distance of pollutant concentration dataset. Eps2 is in the range of values between 0.01 to 0.2", br(),
           "11. Specify the parameter value MinPts to indicate the minimum number of data points in the cluster of pollutant concentration dataset. Eps1 is in the range of values between 1 to 7", br(),
           "12. Click 'Start Visual Clustering' button", br(),
           "13. Visualization in google maps of the ST-DBSCAN algorithm cluster results on trajectory and pollutant concentrations", br(), br(),
           "Step visualization of cluster results of K-Means algorithm on tarjectory and pollutant concentration", br(),
           "14. Click the menu tab Visual Cluster K-Means.", br(),
           "15. Determine the cluster information result from ST-DBSCAN algorithm which will be clustered again using K-Means algorithm.", br(),
           "16. Specify the parameter value of K for the number of cluster centers to be used. K is in the range of values between 2 to 30", br(),
           "17. Click 'Start Visual Clustering K-Means' button", br(),
           "18. Visualization on the google map of the ST-DBSCAN algorithm cluster results on the tarjectory dataset and cluster of K-Mean algorithms in the dataset of the pollutant concentration",
  
  
            h4("Pollutant Visualization GFAS : Visualization of summary plot, calendar plot, simulation results and cluster analysis of pollutant emissions"),
            
           "Step visualization of summary plot from pollutant emissions:", br(),
            "1.	Click the menu tab Summary Plot", br(),
            "2. Select the dataset in the 'Period' box", br(),
            "3.	Click 'Start' button", br(),
            "4.	Summary plot pollutant emissions GFAS in Sumatra",br(), br(),
            "Step visualization of calendar plot from pollutant emission:", br(),
            "5.	Click the menu tab Calendar Plot", br(),
            "6.	Select type of pollutant that wants to be visualized in the 'Pollutant' box", br(),
            "7.	Click 'Start' button", br(),
            "8.	Calendar plot pollutant emission GFAS in Sumatra", br(),br(),
           
           "Step visualization of simulation pollutant from pollutant emission:", br(),
            "9. Click the menu tab Simulation Pollutant", br(),
            "10. Select location of provinces in Sumatra in the 'Location' box", br(),
            "11. Select type of pollutant that wants to be visualized in the 'Pollutant' box", br(),
            "12. Click 'Start' button", br(),
            "13. Calendar plot pollutant emission GFAS by Province", br(),br(),
            
           "Step visualization of simulation pollutant from pollutant emission:", br(),
            "14. Click the menu tab Simulation Pollutant", br(),
             "15. Select month from July-November in the 'Month' box.", br(),
            "16. Select location of provinces in Sumatra in the 'Location' box.", br(),
            "17. Select type of pollutant that wants to be visualized in the 'Pollutant' box", br(),
            "18. Click 'Start' button", br(),
            "19. The simulation result is stated in google map and simulation result table", br(),br(),
  
            "Step visualization of cluster results of K-Means algorithm on pollutant emissions:", br(),
            "20. Click the menu tab Cluster K-Means", br(),
            "21. Select month from July-November in the 'Month' box.", br(),
            "22. Select location of provinces in Sumatra in the 'Location' box", br(),
            "23. Specify the parameter value of K for the number of cluster centers to be used. K is in the range of values between 2 to 30", br(),
            "24. Click 'Start'", br(),
            "25.	The cluster results are expressed in google maps, simulated results tables in clusters", br(), br(),
  
            "Step summary cluster result of K-Means algorithm on pollutant emissions:", br(),
            "26.	Click the menu tab Summary Cluster K-Means", br(),
            "27.	The cluster results is expressed in summary table that contains number of objects in each cluster and average of pollutants in each cluster", br(),
             h4("Haze Pollutant Visualization GFAS: Visualization haze trajectory and pollutant emission GFAS in Sumatra"),
            "1.	Click the dataset in the 'Hotspot Sequence' box", br(),
            "2.	Select type of pollutant that wants to be visualized in the 'Pollutant' box", br(),
            "3.	Click 'Start' button", br(),
           "4.	Visualization on the google maps of simulation results on the pollutant emission GFAS  dataset and trajectory dataset"),
  
  
  
          
  
  tabPanel("About", hr(),
           "This application is used for simulation on trajectory, pollutant emission, and pollutant concentration level. 
           ST-DBSCAN and K-Mean algorithms are used to analyze clusters. 
           The simulation pollutant area is located in Riau's peatlands and simulation pollutant emission area is located in provinces of Sumatra ", br(),
           
           h4("Grouping District in Sumatera"),
           tableOutput("tabel_kabupaten"), br(), br(),
           
           h4("Parameter Values of HYSPLIT model of Trajectory Simulation"),
           tableOutput("tabel_parameter_trajectory"), br(),
           
           h4("Parameter Values of HYSPLIT model of Consentration Simulation"),
           tableOutput("tabel_parameter_consentration"), br(), 
           
           h4("The Level of the Emission of Pollutants (mikrogram/m2s)"),
           tableOutput("tabel_parameter_emission"), br(), br()
           
  )
  
  )
  
  
  
  
  
  
  )))
