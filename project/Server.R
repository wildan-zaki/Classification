### 
library("shiny")
library("shinyjs")
library("shinyBS")
library("cluster") 
library("fpc")
library("sp") #klasifikasi spasial data
library("ggplot2") #untuk proses plotting
library("maptools") #Set of tools for manipulating and reading geographic data
library("rgdal") #Provides bindings to the 'Geospatial' Data Abstraction Library 
library("plyr") #split, combine and apply data
library("svglite") 
#library("SplitR") 
#library("magrittr") 
library("doBy") #untuk summary data
library("psych") 
library("rworldmap")
library("ggmap")
#library("readr")  
library("rgeos") 
library("stats") 
#library("qpcR") 
library('RPostgreSQL') 
library("openair")
library("lubridate")


shinyServer(function(input, output, session)
{
  #CONNECT DATABASE
  pg = dbDriver("PostgreSQL")
  con = dbConnect(pg, user="postgres", password="wildan123",
                  host="localhost", port=5432, dbname="dataset_trajectory")
  
  #------------------------------------------------------------------------------------------ 
  #Load database for HYSPLIT Trajectory Menu
  #------------------------------------------------------------------------------------------
  #Tab Simulation
  selectedData <- reactive({
    switch(input$date_Period,
           #Sumatra 2014
           period14gr0 = myobj <- dbGetQuery(con, "select * from sumatra_juli0915"),
           # period14gr1 = myobj <- dbGetQuery(con, "select * from sumatra_februari2114"),
           # period14gr2 = myobj <- dbGetQuery(con, "select * from sumatra_februari2314"),
           # period14gr3 = myobj <- dbGetQuery(con, "select * from sumatra_februari2714"),
           period14gr4 = myobj <- dbGetQuery(con, "select * from sumatra_maret0914"),
           period14gr5 = myobj <- dbGetQuery(con, "select * from sumatra_maret1114"),
           period14gr6 = myobj <- dbGetQuery(con, "select * from sumatra_maret2514"),
           #Sumatra 2015
           period15gr7 = myobj <- dbGetQuery(con, "select * from sumatra_sept0315"),
           period15gr8 = myobj <- dbGetQuery(con, "select * from sumatra_sept22152"),
           period15gr9 = myobj <- dbGetQuery(con, "select * from sumatra_sept2215"),
           period15gr10 = myobj <- dbGetQuery(con, "select * from sumatra_sept2415"),
           period15gr11 = myobj <- dbGetQuery(con, "select * from sumatra_okt0115"),
           period15gr12 = myobj <- dbGetQuery(con, "select * from sumatra_okt0515"),
           period15gr13 = myobj <- dbGetQuery(con, "select * from sumatra_okt1915"),
           period15gr14 = myobj <- dbGetQuery(con, "select * from sumatra_okt19152"),
           period15gr15 = myobj <- dbGetQuery(con, "select * from sumatra_okt2115"),
           period15gr16 = myobj <- dbGetQuery(con, "select * from sumatra_okt21152"),
           period15gr17 = myobj <- dbGetQuery(con, "select * from sumatra_okt2315"),
           period15gr18 = myobj <- dbGetQuery(con, "select * from sumatra_okt2415"),
           #Kalimantan 2014
           period14gr19 = myobj <- dbGetQuery(con, "select * from kalimantan_sept2514"),
           period14gr20 = myobj <- dbGetQuery(con, "select * from kalimantan_okt0814"),
           period14gr21 = myobj <- dbGetQuery(con, "select * from kalimantan_sep3014"),
           period14gr22 = myobj <- dbGetQuery(con, "select * from kalimantan_nov0114"),
           #Kalimantan 2015
           period15gr23 = myobj <- dbGetQuery(con, "select * from kalimantan_sep0815"),
           period15gr24 = myobj <- dbGetQuery(con, "select * from kalimantan_sept1215"),
           period15gr25 = myobj <- dbGetQuery(con, "select * from kalimantan_sept1415"),
           period15gr26 = myobj <- dbGetQuery(con, "select * from kalimantan_sept2115"),
           period15gr27 = myobj <- dbGetQuery(con, "select * from kalimantan_okt0915"),
           period15gr28 = myobj <- dbGetQuery(con, "select * from kalimantan_okt1415"),
           
           period15gr29 = myobj <- dbGetQuery(con, "select * from kalimantan8oct")
    )
  })
  
  selectedDataInput <- reactive({
    switch(input$date_PeriodInput,
           #Sumatra 2014
           period14gr0 = myobj <- dbGetQuery(con, "select * from sumatra_juli0915"),
           # period14gr1 = myobj <- dbGetQuery(con, "select * from sumatra_februari2114"),
           # period14gr2 = myobj <- dbGetQuery(con, "select * from sumatra_februari2314"),
           # period14gr3 = myobj <- dbGetQuery(con, "select * from sumatra_februari2714"),
           period14gr4 = myobj <- dbGetQuery(con, "select * from sumatra_maret0914"),
           period14gr5 = myobj <- dbGetQuery(con, "select * from sumatra_maret1114"),
           period14gr6 = myobj <- dbGetQuery(con, "select * from sumatra_maret2514"),
           #Sumatra 2015
           period15gr7 = myobj <- dbGetQuery(con, "select * from sumatra_sept0315"),
           period15gr8 = myobj <- dbGetQuery(con, "select * from sumatra_sept22152"),
           period15gr9 = myobj <- dbGetQuery(con, "select * from sumatra_sept2215"),
           period15gr10 = myobj <- dbGetQuery(con, "select * from sumatra_sept2415"),
           period15gr11 = myobj <- dbGetQuery(con, "select * from sumatra_okt0115"),
           period15gr12 = myobj <- dbGetQuery(con, "select * from sumatra_okt0515"),
           period15gr13 = myobj <- dbGetQuery(con, "select * from sumatra_okt1915"),
           period15gr14 = myobj <- dbGetQuery(con, "select * from sumatra_okt19152"),
           period15gr15 = myobj <- dbGetQuery(con, "select * from sumatra_okt2115"),
           period15gr16 = myobj <- dbGetQuery(con, "select * from sumatra_okt21152"),
           period15gr17 = myobj <- dbGetQuery(con, "select * from sumatra_okt2315"),
           period15gr18 = myobj <- dbGetQuery(con, "select * from sumatra_okt2415"),
           #Kalimantan 2014
           period14gr19 = myobj <- dbGetQuery(con, "select * from kalimantan_sept2514"),
           period14gr20 = myobj <- dbGetQuery(con, "select * from kalimantan_okt0814"),
           period14gr21 = myobj <- dbGetQuery(con, "select * from kalimantan_sep3014"),
           period14gr22 = myobj <- dbGetQuery(con, "select * from kalimantan_nov0114"),
           #Kalimantan 2015
           period15gr23 = myobj <- dbGetQuery(con, "select * from kalimantan_sep0815"),
           period15gr24 = myobj <- dbGetQuery(con, "select * from kalimantan_sept1215"),
           period15gr25 = myobj <- dbGetQuery(con, "select * from kalimantan_sept1415"),
           period15gr26 = myobj <- dbGetQuery(con, "select * from kalimantan_sept2115"),
           period15gr27 = myobj <- dbGetQuery(con, "select * from kalimantan_okt0915"),
           period15gr28 = myobj <- dbGetQuery(con, "select * from kalimantan_okt1415"),
           
           period15gr29 = myobj <- dbGetQuery(con, "select * from kalimantan8oct")
           
    )
  })
  
  title_periode <- reactive({
    switch(input$date_Period,
           # period14gr1 = capt <- 'Sequential Hotspot Sumatra 21 February - 23 February 2014',
           # period14gr2 = capt <- 'Sequential Hotspot Sumatra 23 February - 28 February 2014',
           # period14gr3 = capt <- 'Sequential Hotspot Sumatra 27 February - 28 February 2014',
           period14gr4 = capt <- 'Sequential Hotspot Sumatra 09 March - 11 March 2014',
           period14gr5 = capt <- 'Sequential Hotspot Sumatra 11 March - 13 March 2014',
           period14gr6 = capt <- 'Sequential Hotspot Sumatra 25 March - 27 March 2014',
           period15gr7 = capt <- 'Sequential Hotspot Sumatra 03 September - 04 September 2015',
           period15gr8 = capt <- 'Sequential Hotspot Sumatra 22 September - 24 September 2015',
           period15gr9 = capt <- 'Sequential Hotspot Sumatra 22 September - 26 September 2015',
           period15gr10 = capt <- 'Sequential Hotspot Sumatra 24 September - 26 September 2015',
           period15gr11 = capt <- 'Sequential Hotspot Sumatra 01 October - 03 October 2015',
           period15gr12 = capt <- 'Sequential Hotspot Sumatra 05 October - 08 October 2015',
           period15gr13 = capt <- 'Sequential Hotspot Sumatra 19 October - 22 October 2015',
           period15gr14 = capt <- 'Sequential Hotspot Sumatra 19 October - 21 October 2015',
           period15gr15 = capt <- 'Sequential Hotspot Sumatra 21 October - 24 October 2015',
           period15gr16 = capt <- 'Sequential Hotspot Sumatra 21 October - 22 October 2015',
           period15gr17 = capt <- 'Sequential Hotspot Sumatra 23 October - 24 October 2015',
           period15gr18 = capt <- 'Sequential Hotspot Sumatra 24 October - 26 October 2015',
           period14gr19 = capt <- 'Sequential Hotspot Kalimantan 25 September - 27 September 2014 ',
           period14gr20 = capt <- 'Sequential Hotspot Kalimantan 08 October - 09 October 2014',
           period14gr21 = capt <- 'Sequential Hotspot Kalimantan 30 September - 02 October 2014',
           period14gr22 = capt <- 'Sequential Hotspot Kalimantan 01 November - 03 November 2014',
           period15gr23 = capt <- 'Sequential Hotspot Kalimantan 08 September - 09 September 2015',
           period15gr24 = capt <- 'Sequential Hotspot Kalimantan 12 September - 14 September 2015',
           period15gr25 = capt <- 'Sequential Hotspot Kalimantan 14 September - 19 September 2015',
           period15gr26 = capt <- 'Sequential Hotspot Kalimantan 21 September - 23 September 2015',
           period15gr27 = capt <- 'Sequential Hotspot Kalimantan 09 October - 14 October 2015',
           period15gr28 = capt <- 'Sequential Hotspot Kalimantan 14 October - 16 October 2015')
    
    return(capt);
  })
  
  title_periodeInput <- reactive({
    switch(input$date_PeriodInput,
           # period14gr1 = capt <- 'Sequential Hotspot Sumatra 21 February - 23 February 2014',
           # period14gr2 = capt <- 'Sequential Hotspot Sumatra 23 February - 28 February 2014',
           # period14gr3 = capt <- 'Sequential Hotspot Sumatra 27 February - 28 February 2014',
           period14gr4 = capt <- 'Sequential Hotspot Sumatra 09 March - 11 March 2014',
           period14gr5 = capt <- 'Sequential Hotspot Sumatra 11 March - 13 March 2014',
           period14gr6 = capt <- 'Sequential Hotspot Sumatra 25 March - 27 March 2014',
           period15gr7 = capt <- 'Sequential Hotspot Sumatra 03 September - 04 September 2015',
           period15gr8 = capt <- 'Sequential Hotspot Sumatra 22 September - 24 September 2015',
           period15gr9 = capt <- 'Sequential Hotspot Sumatra 22 September - 26 September 2015',
           period15gr10 = capt <- 'Sequential Hotspot Sumatra 24 September - 26 September 2015',
           period15gr11 = capt <- 'Sequential Hotspot Sumatra 01 October - 03 October 2015',
           period15gr12 = capt <- 'Sequential Hotspot Sumatra 05 October - 08 October 2015',
           period15gr13 = capt <- 'Sequential Hotspot Sumatra 19 October - 22 October 2015',
           period15gr14 = capt <- 'Sequential Hotspot Sumatra 19 October - 21 October 2015',
           period15gr15 = capt <- 'Sequential Hotspot Sumatra 21 October - 24 October 2015',
           period15gr16 = capt <- 'Sequential Hotspot Sumatra 21 October - 22 October 2015',
           period15gr17 = capt <- 'Sequential Hotspot Sumatra 23 October - 24 October 2015',
           period15gr18 = capt <- 'Sequential Hotspot Sumatra 24 October - 26 October 2015',
           period14gr19 = capt <- 'Sequential Hotspot Kalimantan 25 September - 27 September 2014 ',
           period14gr20 = capt <- 'Sequential Hotspot Kalimantan 08 October - 09 October 2014',
           period14gr21 = capt <- 'Sequential Hotspot Kalimantan 30 September - 02 October 2014',
           period14gr22 = capt <- 'Sequential Hotspot Kalimantan 01 November - 03 November 2014',
           period15gr23 = capt <- 'Sequential Hotspot Kalimantan 08 September - 09 September 2015',
           period15gr24 = capt <- 'Sequential Hotspot Kalimantan 12 September - 14 September 2015',
           period15gr25 = capt <- 'Sequential Hotspot Kalimantan 14 September - 19 September 2015',
           period15gr26 = capt <- 'Sequential Hotspot Kalimantan 21 September - 23 September 2015',
           period15gr27 = capt <- 'Sequential Hotspot Kalimantan 09 October - 14 October 2015',
           period15gr28 = capt <- 'Sequential Hotspot Kalimantan 14 October - 16 October 2015')
    
    return(capt);
  })
  #Tab Cluster
  check_traj <- reactive({
    input$eps1_traj
    input$eps2_traj
    input$minPts_traj
    if (input$eps1_traj > 0.2 || input$eps1_traj < 0.01) {
      return('Eps 1 must in range between 0.01 and 0.2')
    }
    else if (input$eps2_traj < 1 || input$eps2_traj > 30) {
      return('Eps 2 must in range between 1 and 30')
    }
    else if (input$minPts_traj < 3 || input$minPts_traj > 7) {
      return('MinPts must in range between 3 and 7')
    } else tampil_traj()
  })
  
  #------------------------------------------------------------------------------------------ 
  #Load database for HYSPLIT Pollution/Consentration Menu
  #------------------------------------------------------------------------------------------
  #Tab Simulation
  selectedData2 <- reactive({
    switch(input$date_Period2,
           # period215gr1 = myobj <- dbGetQuery(con, "select * from sumatra_juli0915cons"),
           # period215gr2 = myobj <- dbGetQuery(con, "select * from sumatra_juli2215cons"),
           # period215gr3 = myobj <- dbGetQuery(con, "select * from sumatra_juli2615cons"),
           # period215gr4 = myobj <- dbGetQuery(con, "select * from sumatra_agust3015cons"),
           # period215gr5 = myobj <- dbGetQuery(con, "select * from sumatra_sept0115cons"),
           # period215gr6 = myobj <- dbGetQuery(con, "select * from sumatra_okt2115cons"),
           period215gr7 = myobj <- dbGetQuery(con, "select * from sumatra_sept0315cons"),
           period215gr8 = myobj <- dbGetQuery(con, "select * from sumatra_sept22152cons"),
           period215gr9 = myobj <- dbGetQuery(con, "select * from sumatra_sept2215cons"),
           period215gr10 = myobj <- dbGetQuery(con, "select * from sumatra_sept2415cons"),
           period215gr11 = myobj <- dbGetQuery(con, "select * from sumatra_oct0115cons"),
           period215gr12 = myobj <- dbGetQuery(con, "select * from sumatra_oct0515cons"),
           period215gr13 = myobj <- dbGetQuery(con, "select * from sumatra_oct1915cons"),
           period215gr14 = myobj <- dbGetQuery(con, "select * from sumatra_oct19152cons"),
           period215gr15 = myobj <- dbGetQuery(con, "select * from sumatra_oct2115cons"),
           period215gr16 = myobj <- dbGetQuery(con, "select * from sumatra_oct21152cons"),
           period215gr17 = myobj <- dbGetQuery(con, "select * from sumatra_oct2315cons"),
           period215gr18 = myobj <- dbGetQuery(con, "select * from sumatra_oct2415cons")
           
           
    )
  })
  
  title_periode2 <- reactive({
    switch(input$date_Period2,
           # period215gr1 = capt2 <- 'Sequential Hotspot Sumatra 09 July - 11 July 2015',
           # period215gr2 = capt2 <- 'Sequential Hotspot Sumatra 22 July - 23 July 2015',
           # period215gr3 = capt2 <- 'Sequential Hotspot Sumatra 26 July - 29 July 2015',
           # period215gr4 = capt2 <- 'Sequential Hotspot Sumatra 30 August - 01 September 2015',
           # period215gr5 = capt2 <- 'Sequential Hotspot Sumatra 01 September - 02 September 2015',
           # period215gr6 = capt2 <- 'Sequential Hotspot Sumatra 21 October - 22 October 2015',
           
           period215gr7 = capt2 <- 'Sequential Hotspot Sumatra 03 September - 04 September 2015',
           period215gr8 = capt2 <- 'Sequential Hotspot Sumatra 22 September - 24 September 2015',
           period215gr9 = capt2 <- 'Sequential Hotspot Sumatra 22 September - 26 September 2015',
           period215gr10 = capt2 <- 'Sequential Hotspot Sumatra 24 September - 26 September 2015',
           period215gr11 = capt2 <- 'Sequential Hotspot Sumatra 01 October - 03 October 2015',
           period215gr12 = capt2 <- 'Sequential Hotspot Sumatra 05 October - 08 October 2015',
           period215gr13 = capt2 <- 'Sequential Hotspot Sumatra 19 October - 22 October 2015',
           period215gr14 = capt2 <- 'Sequential Hotspot Sumatra 19 October - 21 October 2015',
           period215gr15 = capt2 <- 'Sequential Hotspot Sumatra 21 October - 24 October 2015',
           period215gr16 = capt2 <- 'Sequential Hotspot Sumatra 21 October - 22 October 2015',
           period15gr17 = capt2 <- 'Sequential Hotspot Sumatra 23 October - 24 October 2015',
           period215gr18 = capt2 <- 'Sequential Hotspot Sumatra 24 October - 26 October 2015'
    )
    return(capt2);
  })
  
  #Tab Cluster
  check_cons <- reactive({
    input$eps1_cons
    input$eps2_cons
    input$minPts_cons
    if (input$eps1_cons > 0.2 || input$eps1_cons < 0.01) {
      return('Eps 1 must in range between 0.01 and 0.2')
    }
    else if (input$eps2_cons < 1 || input$eps2_cons > 30) {
      return('Eps 2 must in range between 1 and 30')
    }
    else if (input$minPts_cons < 3 || input$minPts_cons > 7) {
      return('MinPts must in range between 3 and 7')
    } else tampil_cons()
  })
  
  #------------------------------------------------------------------------------------------ 
  #Load database for HYSPLIT Visualization Menu
  #------------------------------------------------------------------------------------------
  selectedData3 <- reactive({
    switch(input$date_Period3,
           period315gr1 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_juli0915"),
                                                   dbGetQuery(con, "select * from sumatra_juli0915cons")),
           period315gr2 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_juli2215"),
                                                   dbGetQuery(con, "select * from sumatra_juli2215cons")),
           period315gr3 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_juli2615"),
                                                   dbGetQuery(con, "select * from sumatra_juli2615cons")),
           period315gr4 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_agustus3015"),
                                                   dbGetQuery(con, "select * from sumatra_agust3015cons")),
           period315gr5 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_september0115"),
                                                   dbGetQuery(con, "select * from sumatra_sept0115cons")),
           period315gr6 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_oktober2115"),
                                                   dbGetQuery(con, "select * from sumatra_okt2115cons"))
    )
  })
  
  title_periode3 <- reactive({
    switch(input$date_Period3,
           period315gr1 = capt3 <- 'Sequential Hotspot Sumatra 09 July - 11 July 2015',
           period315gr2 = capt3 <- 'Sequential Hotspot Sumatra 22 July - 23 July 2015',
           period315gr3 = capt3 <- 'Sequential Hotspot Sumatra 26 July - 29 July 2015',
           period315gr4 = capt3 <- 'Sequential Hotspot Sumatra 30 August - 01 September 2015',
           period315gr5 = capt3 <- 'Sequential Hotspot Sumatra 01 September - 02 September 2015',
           period315gr6 = capt3 <- 'Sequential Hotspot Sumatra 21 October - 22 October 2015')
    return(capt3);
  })
  
  check_vis <- reactive({
    input$eps1_vis
    input$eps2_vis
    input$minPts_vis
    if (input$eps1_vis > 0.2 || input$eps1_vis < 0.01) {
      return('Eps 1 must in range between 0.01 and 0.2')
    }
    else if (input$eps2_vis < 1 || input$eps2_vis > 30) {
      return('Eps 2 must in range between 1 and 30')
    }
    else if (input$minPts_vis < 3 || input$minPts_vis > 7) {
      return('MinPts must in range between 3 and 7')
    } else tampil_vis()
  })
  
  # ------------------------------------------------------------------------------------------
  #Pollutant Visualization ECMFW
  #-----------------------------------------------------------------------------------------
  #summary plot
  selectedData4 <- reactive({
    period415gr1 = myobj <- dbGetQuery(con, "select * from dbpolutan")
    
  })
  
  title_periode4 <- reactive({
    period415gr1 = capt4 <- 'Summary Plot Emission Pollutants Sumatra July - Nov 2015'
    return(capt4);
  })
  
  #calendar plot
  
  #selectedData5 <- reactive({
    #switch(input$date_Period5, 
          # period515gr1 = myobj <- dbGetQuery(con, "select * from dbpolutan1"),
           #period515gr2 = myobj <- dbGetQuery(con, "select * from dbpolutan1"),
           #period515gr3 = myobj <- dbGetQuery(con, "select * from dbpolutan1"),
          # period515gr4 = myobj <- dbGetQuery(con, "select * from dbpolutan1"),
          # period515gr5 = myobj <- dbGetQuery(con, "select * from dbpolutan1")
   # )
 #})
  
  #title_period5 <- reactive({
    #switch(input$date_Period5,
           
           #period515gr1 = capt5 <- 'Calender Plot Pollutant Emission CO',
           #period515gr2 = capt5 <- 'Calender Plot Pollutant Emission CO2',
           #period515gr3 = capt5 <- 'Calender Plot Pollutant Emission CH4',
           #period515gr4 = capt5 <- 'Calender Plot Pollutant Emission NH3',
           #period515gr5 = capt5 <- 'Calender Plot Pollutant Emission PM25'
    #)
    ##return (capt5)
 # })
  
  #calender pollutant by location
  
  selectedData6 <- reactive({
  if (input$date_Period61=='all') {
      calpoljul <- dbGetQuery(con, paste0("select * from dbpolutan"))
  } else {
    calpoljul <- dbGetQuery(con, paste0("select * from dbpolutan where provinsi='", input$date_Period61, "'"))
  }
    calpoljul
  })
  
  
  selectedData7 <- reactive({
    polutanjulnov <- dbGetQuery(con, paste0("select * from dbpolutan where provinsi='", input$date_Period72, "'"))
    polutanjulnov
  })
  
  
  selectedData8 <- reactive({
    clustpoljul <- dbGetQuery(con, paste0("select * from dbpolutan where provinsi='", input$date_Period82, "'"))
    clustpoljul
  })
  
  
  selectedData9 <- reactive({
    switch(input$date_Period9,
           
           period915gr1 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_sept0315"),
                                                   dbGetQuery(con, "select * from polsept")),
           period915gr2 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_sept2215"),
                                                   dbGetQuery(con, "select * from polsept")),
           period915gr3 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_sept2415"),
                                                   dbGetQuery(con, "select * from polsept")),
           period915gr4 = myobj <- qpcR:::cbind.na(dbGetQuery(con, "select * from sumatra_oktober2115"),
                                                   dbGetQuery(con, "select * from polokt"))
           
           
           
    )
  })
  #------------------------------------------------------------------------------------------ 
  #Algoritme ST-DBSCAN
  #------------------------------------------------------------------------------------------     
  #Penamaan fungsi dan parameter inputan
  stdbscan = function(hasil_sim, eps1, eps2, minpts, seeds = TRUE, countmode = 1:nrow(hasil_sim)) {
    #Perhitungan jarak euclid pada aspek spasial dan aspek temporal
    data_spasial <- dist(cbind(hasil_sim$lat, hasil_sim$lon))
    data_temporal <- dist(hasil_sim$hour.inc)
    #Pengubahan bentuk dist menjadi matriks
    data_spasial <- as.matrix(data_spasial)
    data_temporal <- as.matrix(data_temporal)
    #Menghitung jumlah baris pada data
    n <- nrow(data_spasial)
    #Membuat vector bertipe integer dan logical
    classn <- cv <- integer(n)
    isseed <- logical(n)
    cn <- integer(1)
    
    for (i in 1:n) {
      #Jika countmode = n maka akan keluar titik mana yang sedang diproses
      if (i %in% countmode)
        #Membuat vector titik mana saja yang belum ditandai
        unclass <- (1:n)[cv < 1]
      if (cv[i] == 0) {
        
        #Mencari tetangga secara spasial dan temporal dari titik ke-i
        ddreachables <- intersect(unclass[data_spasial[i, unclass] <= eps1],
                                  unclass[data_temporal[i, unclass] <= eps2])
        #Proses penandaan titik dan tetangga dari titik tersebut jika ada
        if (length(ddreachables) + classn[i] < minpts)
          cv[i] <- (-1)
        else {
          cn <- cn + 1
          cv[i] <- cn
          isseed[i] <- TRUE
          ddreachables <- setdiff(ddreachables, i) # mencari data ddrreachables
          unclass <- setdiff(unclass, i)
          classn[ddreachables] <- classn[ddreachables] + 1
          while (length(ddreachables)) {
            cv[ddreachables] <- cn #Mencari tetangga dari tetangga titik ke-i
            ap <- ddreachables
            ddreachables <- integer()
            for (i2 in seq(along = ap)) {
              j <- ap[i2] #Proses penandaan titik tetangga dari tetangga titik ke-i
              jreachables <- intersect(unclass[data_spasial[j, unclass] <= eps1],
                                       unclass[data_temporal[j, unclass] <= eps2])
              if (length(jreachables) + classn[j] >= minpts) {
                isseed[j] <- TRUE
                cv[jreachables[cv[jreachables] < 0]] <- cn
                ddreachables <- union(ddreachables, jreachables[cv[jreachables] == 0])
              }
              classn[jreachables] <- classn[jreachables] + 1
              unclass <- setdiff(unclass, j)
            }
          } #Mencari titik yang belum ditandai
        } 
      }
      if (!length(unclass))
        break
    }#Proses selesai jika panjang kelas unclass (kelas titik mana saja yang belum ditandai) = NULL
    
    rm(classn) #Mengganti penamaan label titik noise
    if (any(cv == (-1))) {
      cv[cv == (-1)] <- 0
    }
    
    out <- list(cluster = cv, eps = eps1, minpts = minpts)
    class(out) <- "stdbscan"
    out
  }
  
  
  #------------------------------------------------------------------------------------------ 
  #Progress Bar
  #------------------------------------------------------------------------------------------ 
  compute_data <- function(updateProgress = NULL) {
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    for (i in 1:10) {
      Sys.sleep(0.25)
      
      # Compute new row of data
      new_row <- data.frame(x = rnorm(1), y = rnorm(1))
      
      # If we were passed a progress update function, call it
      if (is.function(updateProgress)) {
        text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
        updateProgress(detail = text)
      }
      
      # Add the new row of data
      dat <- rbind(dat, new_row)
    }
    dat
  }
  
  #------------------------------------------------------------------------------------------ 
  #                                     MENU HYSPLIT TRAJECTORY
  #------------------------------------------------------------------------------------------ 
  
  
  # #------------------------------------------------------------------------------------------ 
  #                                  #HYSPLIT Trajectory Package SplitR
  # #------------------------------------------------------------------------------------------  
  HYSPLIT_Traj <- function(datafile) {
    trajectory <- NULL
    N <- nrow(datafile)
    for (i in 1:N) {
      trajectory1 <-
        hysplit_trajectory(
          lat = datafile$latitude[i],
          lon = datafile$longitude[i],
          height = 10,
          duration = 72,
          run_period = as.character(as.Date(datafile$date[1], "%m/%d/%Y")),
          #run_period = "2015-07-09",
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = "E:/Ristiyana Sari/G64140102/Data Meteorologi/") #directory data GDAS
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:N, each = 73) #each = duration + 1
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }
  
  #Sub menu Simulation
  #------------------------------------------------------------------------
  #bikin inputan longitude latitude
  #------------------------------------------------------------------------
  
  global <- reactiveValues(numVal = 1, numMin = 0, numMax = 3)
  
  numVal <- reactive({
    if(!is.null(input$jum_init)){
      if(input$jum_init < global$numMin) return(global$numMin)
      if(input$jum_init > global$numMax) return(global$numMax)
      return(input$jum_init)
    }else {#if (input$jum_init==0){
      return(global$numVal)
    }
  })
  
  output$numInput <- renderUI(numericInput("jum_init","Initial Point (Hotspot) max 3 points", min = global$numMin, max = global$numMax, value = numVal()))
  

  
  #-----------------------------------------------------------------------
  create_ui <- reactive({
    input$jum_init
    longitude <- vector("list", input$jum_init)
    for(i in 1:(input$jum_init)){
      longitude[[i]] <- list(br(), 
                             numericInput(paste0("latitude[",i, "]"),
                                          label=paste0("insert latitude number ", i, " (-3.950 - 2.125)"),
                                          value = "",min = -3.950, max=2.125, step=0.001),
                             numericInput(paste0("longitude[",i,"]"),
                                          label=paste0("insert longitude number ", i," (98.910 - 116.915)"),
                                          value = "", min=98.910, max=116.915, step=0.001))
    }
    return(longitude)
  })
  
  output$Dynamic <- renderUI({
    validate(
      need(input$jum_init!=0, "number of point that entered must be greater than 0")
    )
    
    create_ui()
    
    
  })
  
  
  #---------------------- FUNGSI HYSPLIT2
  
  HYSPLIT_Traj2 <- function(datafile2) {
    trajectory <- NULL
    for (i in 1:input$jum_init) { 
      trajectory1 <-
        hysplit_trajectory(
          lat = input[[paste0("latitude[", i, "]")]],
          lon = input[[paste0("longitude[", i, "]")]],
          height = 10,
          duration = 72,
          run_period = as.character(as.Date(datafile2$date[1],"%m/%d/%Y")),
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = "E:/Ristiyana Sari/G64140102/Data Meteorologi/") #directory data GDAS
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:input$jum_init, each = 73) #each = duration + 1
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }
  
  #-------------------------------------------------------------------------
  #POPUP WARNING
  
  popupWarning <- reactive ({
    
    my_vector <- vector("list",input$jum_init)
    my_vector2 <- vector("list",input$jum_init)
    
    for (i in 1:input$jum_init) {
      my_vector[[i]] <- list(
        lat2 <- input[[paste0("latitude[", i, "]")]]
      )
      my_vector2[[i]] <- list(
        lon2 <- input[[paste0("longitude[", i, "]")]]
      )
      
    }
    
    new_vector <- as.numeric(unlist(my_vector))
    new_vector2 <- as.numeric(unlist(my_vector2))
    
    datafilebaru <- selectedDataInput()
    dateSelect <- as.character(datafilebaru$date)
    
    for (i in 1:input$jum_init) {
      datanew <- dbGetQuery(con,  paste("SELECT * FROM join_table WHERE latitude IN  (", paste(new_vector, collapse = ", "), ") and longitude IN  (", paste(new_vector2, collapse = ", "),") and date='",dateSelect,"'"))
    }
    datanew2 <- nrow(as.data.frame(datanew))
    valueSelect <- (as.data.frame(datanew))
    valuelat <- valueSelect$latitude
    valuelon <- valueSelect$longitude
    
    if(datanew2==0){
      observeEvent(input$go_sim2, {
        showModal(modalDialog(
          title = "Important message",
          "Position is not included in the hotspot sequential pattern",
          easyClose = TRUE
        ))
      })
    }else{
      observeEvent(input$go_sim2, {
        showModal(modalDialog(
          title = "Important message",
          "Position longitude ", paste(valuelon, collapse = ", ")," and latitude ", paste(valuelat, collapse = ", "), " 
          included in the hotspot sequential pattern",
          easyClose = TRUE
        ))
      })
    }
    
    
    })
  
  output$Dynamic2 <- renderPrint({
    input$go_sim2
    isolate(popupWarning())
    
  })
  #----------------------------- Input fail CSV------------------------------
  #--------------------------------------------------------------------------
  observeEvent(input$input_action, {
    
    infile <- input$datafilecsv
    
    datacsv <- as.data.frame(read.csv(infile$datapath,header=TRUE,sep=","))
    
    progress <- shiny::Progress$new(session, min=1, max=5)
    on.exit(progress$close())
    progress$set(message = 'Pre-Process in progress')
    
    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(1)
    }
  })
  
  selectedDataCsv <- reactive({
    infile <- input$datafilecsv
    
    datacsv <- as.data.frame(read.csv(infile$datapath,header=TRUE,sep=","))
    
  })
  
  checkCsv <- reactive({
    datacheckstatus <- selectedDataCsv()
    N3 <- nrow(as.data.frame(datacheckstatus))
    
    loncheck <- datacheckstatus$longitude
    latcheck <- datacheckstatus$latitude
    datecheck <- datacheckstatus$date
    
    for (i in 1:N3) {
      datacheck <- dbGetQuery(con,  paste("SELECT * FROM join_table2 WHERE latitude IN  (", paste(latcheck, collapse = ", "), ") and longitude IN  (", paste(loncheck, collapse = ", "),") and date='",datecheck,"'"))
    }
    
    datacsvexist<- as.data.frame(datacheck)
    
    for (i in 1:N3){
      
      if (any(datacsvexist$longitude == datacheckstatus[i,]$longitude & datacsvexist$latitude == datacheckstatus[i,]$latitude)){
        datacheckstatus[i,4] <- as.character("exist")
      }
      else{
        datacheckstatus[i,4] <- as.character("doesn't exist")
      }
      
    }
    names(datacheckstatus)[4]<- paste("status_on_sekuens_data")
    
    datacheckstatusnew <- as.data.frame(datacheckstatus)
    
  })
  
  tabelcheck <- reactive({
    datatablecheck <- checkCsv()
    
    colnames(datatablecheck) <- c('date','logitude','latitude','status_on_sekuens_data')
    list(datatablecheck = datatablecheck)
  })
  
  
  output$checkstatus <- reactive({
    
    input$go_sim3
    isolate(output$checkstatus <- renderDataTable(
      tabelcheck()$datatablecheck, options = list(paging = TRUE,
                                                  searching = FALSE,
                                                  searchable = FALSE,
                                                  pageLength = 10,
                                                  sort = TRUE)
    ))
    
  })
  
  #---------------------- FUNGSI HYSPLIT3
  
  HYSPLIT_Traj3 <- function(datafile3) {
    trajectory <- NULL
    N2 <- nrow(datafile3)
    for (i in 1:N2) { 
      trajectory1 <-
        hysplit_trajectory(
          lat = datafile3$latitude[i],
          lon = datafile3$longitude[i],
          height = 10,
          duration = 72,
          run_period = as.character(as.Date(datafile3$date[1],"%m/%d/%Y")),
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = "F:/Skripsi/Skripsi Ristiyana Sari/Data Meteorologi/") #directory data GDAS
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:N2, each = 73) #each = duration + 1
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }
  #--------------------- #Membuat tabel hasil simulasi-------------------
  #sequence
  
  tabel_traj <- reactive({
    datafile <- selectedData()
    hasil_sim_traj = HYSPLIT_Traj(datafile)
    colnames(hasil_sim_traj) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    list(hasil_sim_traj = hasil_sim_traj)
  })
  
  #--user input
  tabel_traj2 <- reactive({
    datafile2 <- selectedDataInput()
    hasil_sim_traj2 = HYSPLIT_Traj2(datafile2)
    colnames(hasil_sim_traj2) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    list(hasil_sim_traj2 = hasil_sim_traj2)
  })
  
  #--file csv
  tabel_traj3 <- reactive({
    datafile3 <- selectedDataCsv()
    hasil_sim_traj3 = HYSPLIT_Traj3(datafile3)
    colnames(hasil_sim_traj3) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height (m AGL)', 'pressure (hPa)', 'date_hour', 'date')
    list(hasil_sim_traj3 = hasil_sim_traj3)
  })
  
  
  #------------------------output------------------------------
  
  
  output$caption_traj <- renderText({
    if (input$date_Period == "")
      paste("Trajectory Simulation using HYSPLIT Model")
    else
      paste("Trajectory Simulation HYSPLIT Model on Data", title_periode());
  })
  
  output$caption_traj2 <- renderText({
    if (input$date_PeriodInput == "")
      paste("Trajectory Simulation using HYSPLIT Model")
    else
      paste("Trajectory Simulation HYSPLIT Model on Data", title_periodeInput());
  })
  
  output$caption_traj3 <- renderText({
    paste("Haze Trajectory Simulation using HYSPLIT Model, on Initial Point from CSV File");
  })
  
  output$titlecheck <- renderText({
    input$go_sim3
    paste("Data from CSV File");
  })
  output$titleSimulation <- renderText({
    input$go_sim3
    paste("Result of Simulation");
  })
  
  #Title in Tab Panel Clustering Trajectory
  output$caption_traj_clust <- renderText({
    paste("Cluster Trajectory using ST-DBSCAN algorithm on Data", title_periode());
  })
  
  #Output of Tab Panel Simulation in data Simulation Trajectory
  output$simulation_traj <- renderPrint({
    input$go_sim
    if (input$go_sim == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_traj <- renderDataTable(
      tabel_traj()$hasil_sim_traj, options = list(paging = TRUE,
                                                  searching = FALSE,
                                                  searchable = FALSE,
                                                  pageLength = 10,
                                                  sort = TRUE)
    ))
    
  })
  
  output$simulation_trajinput <- renderPrint({
    input$go_sim2
    if (input$go_sim2 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_trajinput <- renderDataTable(
      tabel_traj2()$hasil_sim_traj2, options = list(paging = TRUE,
                                                    searching = FALSE,
                                                    searchable = FALSE,
                                                    pageLength = 10,
                                                    sort = TRUE)
    ))
    
  })
  
  output$simulation_trajcsv <- renderPrint({
    input$go_sim3
    if (input$go_sim3 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_trajcsv <- renderDataTable(
      tabel_traj3()$hasil_sim_traj3, options = list(paging = TRUE,
                                                    searching = FALSE,
                                                    searchable = FALSE,
                                                    pageLength = 10,
                                                    sort = TRUE)
    ))
    
  })
  
  output$simulation_trajinput <- renderPrint({
    input$go_sim2
    if (input$go_sim2 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_trajinput <- renderDataTable(
      tabel_traj2()$hasil_sim_traj2, options = list(paging = TRUE,
                                                    searching = FALSE,
                                                    searchable = FALSE,
                                                    pageLength = 10,
                                                    sort = TRUE)
    ))
    
  })
  #------------------------ PLOTTING
  
  plot_leaflet_traj <- reactive({
    dataplot <- tabel_traj()$hasil_sim_traj
    dataplot$color <- as.character(dataplot$receptor)
    dataplot$color[dataplot$color == "1"] <- "#F44336" #"red"
    dataplot$color[dataplot$color == "2"] <- "#4CAF50" #green
    dataplot$color[dataplot$color == "3"] <- "aqua"
    dataplot$color[dataplot$color == "4"] <- "#2196F3" #blue
    dataplot$color[dataplot$color == "5"] <- "#FF9800" #orange
    dataplot$color[dataplot$color == "6"] <- "#FFEB3B" #yellow
    dataplot$color[dataplot$color == "7"] <- "#673AB7" #deep purple
    dataplot$color[dataplot$color == "8"] <- "#f1c40f" #sunflower
    dataplot$color[dataplot$color == "9"] <- "#795548" #brown
    dataplot$color[dataplot$color == "10"] <- "#8BC34A" #light green
    dataplot$color[dataplot$color == "11"] <- "#BA68C8" #purple
    dataplot$color[dataplot$color == "12"] <- "#607D8B" #blue grey
    dataplot$color[dataplot$color == "13"] <- "#009688" #teal
    dataplot$color[dataplot$color == "14"] <- "#E91E63" #pink
    dataplot$color[dataplot$color == "15"] <- "#3F51B5" #indigo
    dataplot$color[dataplot$color == "16"] <- "#CDDC39" #lime
    dataplot$color[dataplot$color == "17"] <- "#FFC107" #amber
    dataplot$color[dataplot$color == "18"] <- "#FF5722" #deeporange
    dataplot$color[dataplot$color == "19"] <- "#c0392b" #pomegranade
    dataplot$color[dataplot$color == "20"] <- "#3498db" #peterriver
    dataplot$color[dataplot$color == "21"] <- "#e74c3c" #alizarin
    dataplot$color[dataplot$color == "22"] <- "#2980b9" #belizehole
    dataplot$color[dataplot$color == "23"] <- "#16a085" #greensea
    dataplot$color[dataplot$color == "24"] <- "#2c3e50" #midnightblue
    dataplot$color[dataplot$color == "25"] <- "#95a5a6" #concreate
    dataplot$color[dataplot$color == "26"] <- "#69F0AE" #greenA200
    dataplot$color[dataplot$color == "27"] <- "#76FF03" #lightgreenA400
    dataplot$color[dataplot$color == "28"] <- "#827717" #lime900
    dataplot$color[dataplot$color == "29"] <- "#FFFF00" #yellowA200
    dataplot$color[dataplot$color == "30"] <- "#3E2723" #brown900
    dataplot$color[dataplot$color == "31"] <- "#FF6E40" #deeporangeA200
    dataplot$color[dataplot$color == "32"] <- "#FF9E80" #deeporangeA100
    dataplot$color[dataplot$color == "33"] <- "#FF1744" #redA400
    dataplot$color[dataplot$color == "34"] <- "#D50000" #redA700
    dataplot$color[dataplot$color == "35"] <- "#D500F9" #purpleA400
    dataplot$color[dataplot$color == "36"] <- "#E57373" #red300
    dataplot$color[dataplot$color == "37"] <- "#F06292" #pink300
    
    basedreceptor <- (paste("Receptor : ", dataplot$receptor, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$date, "<br/>"))
    popup1 <- (paste(basedreceptor,longitudepaste, latitudepaste, heightpaste, pressurepaste, datepaste, sep = ""))
    
    mymap = leaflet()
    
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    
    
    mymap=addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, color = dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)
    
    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour.inc==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })
  
  plot_leaflet_trajinput <- reactive({
    dataplot <- tabel_traj2()$hasil_sim_traj2
    dataplot$color <- as.character(dataplot$receptor)
    dataplot$color[dataplot$color == "1"] <- "#F44336" #"red"
    dataplot$color[dataplot$color == "2"] <- "#4CAF50" #green
    dataplot$color[dataplot$color == "3"] <- "aqua"
    dataplot$color[dataplot$color == "4"] <- "#2196F3" #blue
    dataplot$color[dataplot$color == "5"] <- "#FF9800" #orange
    dataplot$color[dataplot$color == "6"] <- "#FFEB3B" #yellow
    dataplot$color[dataplot$color == "7"] <- "#673AB7" #deep purple
    dataplot$color[dataplot$color == "8"] <- "black"
    dataplot$color[dataplot$color == "9"] <- "#795548" #brown
    dataplot$color[dataplot$color == "10"] <- "#8BC34A" #light green
    dataplot$color[dataplot$color == "11"] <- "#BA68C8" #purple
    dataplot$color[dataplot$color == "12"] <- "#607D8B" #blue grey
    dataplot$color[dataplot$color == "13"] <- "#009688" #teal
    dataplot$color[dataplot$color == "14"] <- "#E91E63" #pink
    dataplot$color[dataplot$color == "15"] <- "#3F51B5" #indigo
    dataplot$color[dataplot$color == "16"] <- "#CDDC39" #lime
    dataplot$color[dataplot$color == "17"] <- "#FFC107" #amber
    dataplot$color[dataplot$color == "18"] <- "#FF5722" #deeporange
    
    basedreceptor <- (paste("Receptor : ", dataplot$receptor, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$date, "<br/>"))
    popup1 <- (paste(basedreceptor,longitudepaste, latitudepaste, heightpaste, pressurepaste, datepaste, sep = ""))
    
    mymap = leaflet()
    
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    
    
    mymap=addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, color = dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)
    
    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour.inc==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })
  
  
  output$showplotleaflet <- renderLeaflet({
    input$go_sim
    if (input$go_sim == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leaflet_traj())
  })
  
  output$showplotleafletinput <- renderLeaflet({
    input$go_sim2
    if (input$go_sim2 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leaflet_trajinput())
  })
  
  #----------------------- #sub Menu Cluster
  
  #Summary Statistics Cluster ST-DBSCAN Simulation Trajectory
  Statistics_clustering <- reactive({
    datafile <- selectedData()
    hasil_sim = HYSPLIT_Traj(datafile)
    result = stdbscan(hasil_sim, eps1 = input$eps1_traj, eps2 = input$eps2_traj, minpts = input$minPts_traj)
    dataclust <- data.frame(receptor = hasil_sim$receptor, year = hasil_sim$year, month = hasil_sim$month, day = hasil_sim$day, hour = hasil_sim$hour, hour.inc = hasil_sim$hour.inc,
                            lat = hasil_sim$lat, lon = hasil_sim$lon, height = hasil_sim$height, pressure = hasil_sim$pressure, date2 = hasil_sim$date2, date = hasil_sim$date, cluster = result$cluster)
    height_traj <- dataclust$height
    pressure_traj <- dataclust$pressure
    tablestat <- describeBy(data.frame(height_traj, pressure_traj), dataclust$cluster, mat = T)
    tablestat$vars[tablestat$vars == 1] <- "height"
    tablestat$vars[tablestat$vars == 2] <- "pressure"
    tablestat2 <- data.frame(tablestat$group1, tablestat$vars, tablestat$n,
                             tablestat$mean, tablestat$sd, tablestat$median,
                             tablestat$min, tablestat$max, tablestat$range, tablestat$se)
    colnames(tablestat2) <- c('Cluster', 'Trajectory', 'n', 'mean', 'standard deviation', 'median', 'min', 'max', 'range', 'standard error')
    list(tablestat2 = tablestat2)
  })
  
  #Tabel Hasil Clustering dan Simulation model HYSPLIT
  stat_cluster_traj <- reactive({
    datafile <- selectedData()
    hasil_sim = HYSPLIT_Traj(datafile)
    result = stdbscan(hasil_sim, eps1 = input$eps1_traj, eps2 = input$eps2_traj, minpts = input$minPts_traj)
    tabel_clust_traj <- data.frame(receptor = hasil_sim$receptor, year = hasil_sim$year, month = hasil_sim$month, day = hasil_sim$day, hour = hasil_sim$hour, hour.inc = hasil_sim$hour.inc,
                                   lat = hasil_sim$lat, lon = hasil_sim$lon, height = hasil_sim$height, pressure = hasil_sim$pressure, date2 = hasil_sim$date2, date = hasil_sim$date, cluster = result$cluster)
    colnames(tabel_clust_traj) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date', 'cluster')
    list(tabel_clust_traj = tabel_clust_traj)
  })
  
  #Plotting
  plot_leaflet_clusttraj <- reactive({
    dataplot <- stat_cluster_traj()$tabel_clust_traj
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$date, "<br/>"))
    clusterpaste <- (paste("Cluster : ", dataplot$cluster, "<br/>"))
    popup1 <- (paste(longitudepaste, latitudepaste, clusterpaste, heightpaste, pressurepaste, datepaste, sep = ""))
    mymap = leaflet()
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    factpal <- colorFactor(topo.colors(1000), dataplot$cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, fillColor = factpal(dataplot$cluster), fillOpacity = 2, stroke = FALSE, color = "#808080", weight = 50, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)
    
    leafIcons <- icons(
      iconUrl = 
        "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour.inc==0)
    
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    lablegend <- unique(data.frame(factpal(dataplot$cluster),dataplot$cluster))
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot.cluster.,
                      labels= lablegend$dataplot.cluster,
                      title= "Cluster")
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
    
  })
  
  #-----------------------Output------------------------------------
  
  #Output for Descriptivie  Statistics Table of Trajectory Clustering
  output$stat_traj_table <- renderPrint({
    input$go_clusttraj
    if (input$go_clusttraj == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Cluster Trajectory Results using ST-DBSCAN algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$stat_traj_table <- renderDataTable(
      Statistics_clustering()$tablestat2, options = list(paging = TRUE,
                                                         searching = FALSE,
                                                         searchable = FALSE,
                                                         pageLength = 15,
                                                         sort = TRUE)
    ))
  })
  
  #Output for Summary Table of Trajectory Clustering
  output$summary_clust_table <- renderPrint({
    input$go_clusttraj
    if (input$go_clusttraj == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Summary of Cluster trajectory using ST-DBSCAN algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$summary_clust_table <- renderDataTable(
      stat_cluster_traj()$tabel_clust_traj, options = list(paging = TRUE,
                                                           searching = FALSE,
                                                           searchable = FALSE,
                                                           pageLength = 5,
                                                           sort = TRUE)
    ))
  })
  
  #Output render plot  
  output$showplotleaflet2 <- renderLeaflet({
    input$go_clusttraj
    if (input$go_clusttraj == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leaflet_clusttraj())
  })
  
  #------------------------------------------------------------------------------------------ 
  #                                     MENU HYSPLIT POLLUTAN
  #------------------------------------------------------------------------------------------ 
  
  #Sub menu Simulation
  
  #Tabel Hasil Simulation Consentration model HYSPLIT
  tabel_cons <- reactive({
    datafile2 <- selectedData2()
    hasil_sim_cons <- data.frame(Day = datafile2$Day, Hour = datafile2$Temporal2, Latitude = datafile2$Latitude, Longitude = datafile2$Longitude, CO2 = datafile2$CO2, CO = datafile2$CO)
    list(hasil_sim_cons = hasil_sim_cons)
  })
  
  #Summary Statistics Cluster ST-DBSCAN Simulation Consentration
  Statistics_clustering2 <- reactive({
    dataclust <- stat_cluster_cons_stdbscan()$tabel_clust_cons
    Carbon_dioksida <- dataclust$CO2
    Carbon_monoksida <- dataclust$CO
    tablestat2 <- describeBy(data.frame(Carbon_dioksida, Carbon_monoksida), dataclust$Cluster, mat = T)
    tablestat2$vars[tablestat2$vars == 1] <- "Carbon dioxide"
    tablestat2$vars[tablestat2$vars == 2] <- "Carbon monoxide"
    tablestat3 <- data.frame(tablestat2$group1, tablestat2$vars, tablestat2$n,
                             tablestat2$mean, tablestat2$sd, tablestat2$median,
                             tablestat2$min, tablestat2$max, tablestat2$range, tablestat2$se)
    colnames(tablestat3) <- c('Cluster', 'Consentration', 'n', 'mean', 'standard deviation', 'median', 'min', 'max', 'range', 'standard error')
    list(tablestat3 = tablestat3)
  })
  
  #Stat Clustering ST-BDSCAN and Simulation data model HYSPLIT
  stat_cluster_cons_stdbscan <- reactive({
    datafile2 <- selectedData2()
    hasil_sim2 = data.frame(lat = datafile2$Latitude, lon = datafile2$Longitude, hour.inc = datafile2$Temporal2)
    result2 = stdbscan(hasil_sim2, eps1 = input$eps1_cons, eps2 = input$eps2_cons, minpts = input$minPts_cons)
    hasil2 = result2$cluster
    tabel_clust_cons = data.frame(Latitude = datafile2$Latitude, Longitude = datafile2$Longitude, Hour = datafile2$Temporal2, CO2 = datafile2$CO2, CO = datafile2$CO, Cluster = hasil2)
    list(tabel_clust_cons = tabel_clust_cons)
  })
  
  #Summary Statistics Cluster ST-DBSCAN Simulation Consentration
  Statistics_clustering3 <- reactive({
    dataclust <- stat_cluster_cons_kmeans()$tabel_clust_cons_kmeans
    Carbon_dioksida <- dataclust$CO2
    Carbon_monoksida <- dataclust$CO
    tablestat2 <- describeBy(data.frame(Carbon_dioksida, Carbon_monoksida), dataclust$Cluster, mat = T)
    tablestat2$vars[tablestat2$vars == 1] <- "Carbon dioxide"
    tablestat2$vars[tablestat2$vars == 2] <- "Carbon monoxide"
    tablestat4 <- data.frame(tablestat2$group1, tablestat2$vars, tablestat2$n,
                             tablestat2$mean, tablestat2$sd, tablestat2$median,
                             tablestat2$min, tablestat2$max, tablestat2$range, tablestat2$se)
    colnames(tablestat4) <- c('Cluster', 'Consentration', 'n', 'mean', 'standard deviation', 'median', 'min', 'max', 'range', 'standard error')
    list(tablestat4 = tablestat4)
  })
  
  #Stat Clustering K-Means and Simulation data model HYSPLIT
  stat_cluster_cons_kmeans <- reactive({
    data_clust_stdbscan <- stat_cluster_cons_stdbscan()$tabel_clust_cons
    data_by_clust <- subset(data_clust_stdbscan, data_clust_stdbscan$Cluster == input$Cluster_STDBSCAN)
    dataclustkmeans <- data.frame(data_by_clust$CO2, data_by_clust$CO)
    clust_kmeans <- kmeans(dataclustkmeans, centers = input$K_cons)
    tabel_clust_cons_kmeans <- data.frame(Latitude = data_by_clust$Latitude, Longitude = data_by_clust$Longitude, CO2 = data_by_clust$CO2, CO = data_by_clust$CO, Cluster = clust_kmeans$cluster)
    list(tabel_clust_cons_kmeans = tabel_clust_cons_kmeans)
  })
  
  #Plot Simulation consentration
  plot_leaflet_cons <- reactive({
    dataplot <- tabel_cons()$hasil_sim_cons
    longitudepaste <- (paste("Longitude : ", dataplot$Longitude, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$Latitude, "<br/>"))
    CO2paste <- (paste("CO2: ", dataplot$CO2, "<br/>"))
    COpaste <- (paste("CO : ", dataplot$CO, "<br/>"))
    Hourpaste <- (paste("Hour : ", dataplot$Hour, "<br/>"))
    popup1 <- (paste(longitudepaste, latitudepaste, CO2paste, COpaste, Hourpaste, sep = ""))
    mymap = leaflet()
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    factpal <- colorFactor(topo.colors(1000), dataplot$Hour)
    mymap = addCircleMarkers(mymap, lng = dataplot$Longitude, lat = dataplot$Latitude, fillColor = factpal(dataplot$Hour), fillOpacity = 1, stroke = FALSE, color = "#808080", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup1)
    lablegend <- unique(data.frame(factpal(dataplot$Hour),dataplot$Hour))
    lablegend$factpal.dataplot.Hour.
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot.Hour.,
                      labels= lablegend$dataplot.Hour,
                      title= "Hours",
                      opacity = 1)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #Plot clustering consentration ST-DBSCAN
  plot_leaflet_cluststdbscan <- reactive({
    dataplot <- stat_cluster_cons_stdbscan()$tabel_clust_cons
    longitudepaste <- (paste("Longitude : ", dataplot$Longitude, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$Latitude, "<br/>"))
    CO2paste <- (paste("CO2: ", dataplot$CO2, "<br/>"))
    COpaste <- (paste("CO : ", dataplot$CO, "<br/>"))
    Hourpaste <- (paste("Hour : ", dataplot$Hour, "<br/>"))
    clusterpaste <- (paste("Cluster : ", dataplot$Cluster, "<br/>"))
    popup1 <- (paste(longitudepaste, latitudepaste, CO2paste, COpaste, Hourpaste, clusterpaste, sep = ""))
    mymap = leaflet()
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    factpal <- colorFactor(topo.colors(1000), dataplot$Cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot$Longitude, lat = dataplot$Latitude, fillColor = factpal(dataplot$Cluster), fillOpacity = 1, stroke = FALSE, color = "#808080", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup1)
    lablegend <- unique(data.frame(factpal(dataplot$Cluster),dataplot$Cluster))
    lablegend$dataplot.Cluster
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot.Cluster.,
                      labels= lablegend$dataplot.Cluster,
                      title= "Cluster",
                      opacity = 1)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #Plot clustering consentration K-Means
  plot_leafletclustkmeans <- reactive({
    dataplot <- stat_cluster_cons_kmeans()$tabel_clust_cons_kmeans
    longitudepaste <- (paste("Longitude : ", dataplot$Longitude, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$Latitude, "<br/>"))
    CO2paste <- (paste("CO2: ", dataplot$CO2, "<br/>"))
    COpaste <- (paste("CO : ", dataplot$CO, "<br/>"))
    clusterpaste <- (paste("Cluster : ", dataplot$Cluster, "<br/>"))
    popup1 <- (paste(longitudepaste, latitudepaste, CO2paste, COpaste, clusterpaste, sep = ""))
    mymap = leaflet()
    
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    
    factpal <- colorFactor(topo.colors(1000), dataplot$Cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot$Longitude, lat = dataplot$Latitude, fillColor = factpal(dataplot$Cluster), fillOpacity = 1, stroke = FALSE, color = "blue", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup1)
    lablegend <- unique(data.frame(factpal(dataplot$Cluster),dataplot$Cluster))
    lablegend$dataplot.Cluster
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot.Cluster.,
                      labels= lablegend$dataplot.Cluster,
                      title= "Cluster",
                      opacity = 1)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #-------------------------------Output----------------------------
  
  #Title in Tab Panel Simulation Consentration
  output$caption_const <- renderText({
    if (input$date_Period2 == "")
      paste("Simulate the Level of Pollutant Concentration using HYSPLIT Model")
    else
      paste("Simulate the Level of Pollutant Concentration HYSPLIT Model on Data", title_periode2());
  })
  
  #Title in Tab Panel Clustering Consentration ST-DBSCAN
  output$caption_const_clust_stdbscan <- renderText({
    paste("Cluster Consentration of Pollutants using ST-DBSCAN algorithm on Simulation Result Data", title_periode2());
  })
  
  #Title in Tab Panel Clustering Consentration K-Means
  output$caption_const_clust_kmeans <- renderText({
    paste("Cluster Consentration of Pollutants using K-Means algorithm on Simulation Result Data", title_periode2());
  })
  
  #Output of Tab Panel Simulation in data Simulation Consentration
  output$simulation_const <- renderPrint({
    input$go_cons
    if (input$go_cons == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Pollutant Consentration Level in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_const <- renderDataTable(
      tabel_cons()$hasil_sim_cons, options = list(paging = TRUE,
                                                  searching = FALSE,
                                                  searchable = FALSE,
                                                  pageLength = 10,
                                                  sort = TRUE)
    ))
  })
  
  #Output for Table Cluster of Consentration Clustering
  output$stat_cons_table_stdbscan <- renderPrint({
    input$go_clustST
    if (input$go_clustST == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Cluster Consentration Result of Pollutant using ST-DBSCAN algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$stat_cons_table_stdbscan <- renderDataTable(
      Statistics_clustering2()$tablestat3, options = list(paging = TRUE,
                                                          searching = FALSE,
                                                          searchable = FALSE,
                                                          pageLength = 15,
                                                          sort = TRUE)
    ))
  })
  
  #Output for Table Cluster of Consentration Clustering K-Means
  output$stat_cons_table_kmeans <- renderPrint({
    input$go_clustKM
    if (input$go_clustKM == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Cluster Consentration Level of Pollutant using K-Means algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$stat_cons_table_kmeans <- renderDataTable(
      Statistics_clustering3()$tablestat4, options = list(paging = TRUE,
                                                          searching = FALSE,
                                                          searchable = FALSE,
                                                          pageLength = 15,
                                                          sort = TRUE)
    )) 
  })
  
  #Output for Summary Table of Consentration Clustering
  output$summary_clust_table_stdbscan <- renderPrint({
    input$go_clustST
    if (input$go_clustST == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Summary of Cluster Consentration Level of Pollutant using ST-DBSCAN algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$summary_clust_table_stdbscan <- renderDataTable(
      stat_cluster_cons_stdbscan()$tabel_clust_cons, options = list(paging = TRUE,
                                                                    searching = FALSE,
                                                                    searchable = FALSE,
                                                                    pageLength = 5,
                                                                    sort = TRUE)
    ))
  })
  
  #Output for Summary Table of Consentration Clustering
  output$summary_clust_table_kmeans <- renderPrint({
    input$go_clustKM
    if (input$go_clustKM == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Summary of Cluster Consentration Level of Pollutant using K-Means algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$summary_clust_table_kmeans <- renderDataTable(
      stat_cluster_cons_kmeans()$tabel_clust_cons_kmeans, options = list(paging = TRUE,
                                                                         searching = FALSE,
                                                                         searchable = FALSE,
                                                                         pageLength = 5,
                                                                         sort = TRUE)
    ))
  })
  
  #Output render plot
  output$showplotleaflet3 <- renderLeaflet({
    input$go_cons
    if (input$go_cons == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leaflet_cons())
  })
  
  #Output render plot  
  output$showplotleaflet4 <- renderLeaflet({
    input$go_clustST
    if (input$go_clustST == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leaflet_cluststdbscan())
  })
  
  #Output render plot  
  output$showplotleaflet5 <- renderLeaflet({
    input$go_clustKM
    if (input$go_clustKM == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leafletclustkmeans())
  })
  
  # if (interactive()) {
  #   # Display an important message that can be dismissed only by clicking the
  #   # dismiss button.
  #   shinyApp(
  #     ui = basicPage(
  #       actionButton("show", "Show modal dialog")
  #     ),
  #     server = function(input, output) {
  #       observeEvent(input$show, {
  #         showModal(modalDialog(
  #           title = "Important message",
  #           "This is an important message!"
  #         ))
  #       })
  #     }
  #   )
  
  
  #---------------------------------------------- SUB-MENU VISUALIZATION --------------------------------------------------#
  
  #Plot simulation trajectory and consentration
  plot_leafletviz_sim <- reactive({
    datafile3 <- selectedData3()
    #simulation trajectory 
    sekuens <- data.frame(na.omit(datafile3[, 1:5]))
    hasil_sim_traj_vis = HYSPLIT_Traj(sekuens)
    colnames(hasil_sim_traj_vis) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    #simulation consentration
    datacons <- datafile3[, - (1:5)]
    hasil_sim_cons_vis <- data.frame(Day = datacons$Day, Hour = datacons$Temporal2, Latitude = datacons$Latitude, Longitude = datacons$Longitude, CO2 = datacons$CO2, CO = datacons$CO)
    #dataset for visualization
    dataplot_sim_traj <- hasil_sim_traj_vis
    dataplot_sim_cons <- hasil_sim_cons_vis
    
    #warna
    dataplot_sim_traj$color <- as.character(dataplot_sim_traj$receptor)
    dataplot_sim_traj$color[dataplot_sim_traj$color == "1"] <- "#F44336" #"red"
    dataplot_sim_traj$color[dataplot_sim_traj$color == "2"] <- "#4CAF50" #green
    dataplot_sim_traj$color[dataplot_sim_traj$color == "3"] <- "aqua"
    dataplot_sim_traj$color[dataplot_sim_traj$color == "4"] <- "#2196F3" #blue
    dataplot_sim_traj$color[dataplot_sim_traj$color == "5"] <- "#FF9800" #orange
    dataplot_sim_traj$color[dataplot_sim_traj$color == "6"] <- "#FFEB3B" #yellow
    dataplot_sim_traj$color[dataplot_sim_traj$color == "7"] <- "#673AB7" #deep purple
    dataplot_sim_traj$color[dataplot_sim_traj$color == "8"] <- "black"
    dataplot_sim_traj$color[dataplot_sim_traj$color == "9"] <- "#795548" #brown
    dataplot_sim_traj$color[dataplot_sim_traj$color == "10"] <- "#8BC34A" #light green
    dataplot_sim_traj$color[dataplot_sim_traj$color == "11"] <- "#BA68C8" #purple
    dataplot_sim_traj$color[dataplot_sim_traj$color == "12"] <- "#607D8B" #blue grey
    dataplot_sim_traj$color[dataplot_sim_traj$color == "13"] <- "#009688" #teal
    dataplot_sim_traj$color[dataplot_sim_traj$color == "14"] <- "#E91E63" #pink
    dataplot_sim_traj$color[dataplot_sim_traj$color == "15"] <- "#3F51B5" #indigo
    dataplot_sim_traj$color[dataplot_sim_traj$color == "16"] <- "#CDDC39" #lime
    dataplot_sim_traj$color[dataplot_sim_traj$color == "17"] <- "#FFC107" #amber
    dataplot_sim_traj$color[dataplot_sim_traj$color == "18"] <- "#FF5722" #deeporange
    
    #popup simulation trajectory
    longitudepaste_sim_traj <- (paste("Longitude : ", dataplot_sim_traj$lon, "<br/>"))
    latitudepaste_sim_traj <- (paste("Latitude : ", dataplot_sim_traj$lat, "<br/>"))
    heightpaste_sim_traj <- (paste("Height: ", dataplot_sim_traj$height, "<br/>"))
    pressurepaste_sim_traj <- (paste("Pressure : ", dataplot_sim_traj$pressure, "<br/>"))
    datepaste_sim_traj <- (paste("Date : ", dataplot_sim_traj$date, "<br/>"))
    popup1 <- (paste(longitudepaste_sim_traj, latitudepaste_sim_traj, heightpaste_sim_traj, pressurepaste_sim_traj, datepaste_sim_traj, sep = ""))
    
    #popup simulation consentration
    longitudepaste_sim_cons <- (paste("Longitude : ", dataplot_sim_cons$Longitude, "<br/>"))
    latitudepaste_sim_cons <- (paste("Latitude : ", dataplot_sim_cons$Latitude, "<br/>"))
    CO2paste_sim_cons <- (paste("CO2: ", dataplot_sim_cons$CO2, "<br/>"))
    COpaste_sim_cons <- (paste("CO : ", dataplot_sim_cons$CO, "<br/>"))
    clusterpaste_sim_cons <- (paste("Cluster : ", dataplot_sim_cons$Cluster, "<br/>"))
    popup2 <- (paste(longitudepaste_sim_cons, latitudepaste_sim_cons, CO2paste_sim_cons, COpaste_sim_cons, clusterpaste_sim_cons, sep = ""))
    
    mymap = leaflet()
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    mymap = addCircleMarkers(mymap, lng = dataplot_sim_traj$lon, lat = dataplot_sim_traj$lat, color =  dataplot_sim_traj$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)
    
    factpal <- colorFactor(topo.colors(1000), dataplot_sim_cons$Hour)
    mymap = addCircleMarkers(mymap, lng = dataplot_sim_cons$Longitude, lat = dataplot_sim_cons$Latitude, fillColor = factpal(dataplot_sim_cons$Hour), fillOpacity = 1, stroke = FALSE, color = "#808080", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup2)
    lablegend <- unique(data.frame(factpal(dataplot_sim_cons$Hour),dataplot_sim_cons$Hour))
    lablegend$factpal.dataplot.Hour.
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot_sim_cons.Hour.,
                      labels= lablegend$dataplot_sim_cons.Hour,
                      title= "Hours",
                      opacity = 1)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #Plot cluster trajectory and consentration stdbscan
  plot_leafletviz_cons <- reactive({
    datafile3 <- selectedData3()
    #simulation trajectory 
    sekuens <- data.frame(na.omit(datafile3[, 1:5]))
    hasil_sim_traj_vis = HYSPLIT_Traj(sekuens)
    colnames(hasil_sim_traj_vis) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    result = stdbscan(hasil_sim_traj_vis, eps1 = input$eps1_traj_vis, eps2 = input$eps2_traj_vis, minpts = input$minPts_traj_vis)
    tabel_clust_traj_vis <- data.frame(receptor = hasil_sim_traj_vis$receptor, year = hasil_sim_traj_vis$year, month = hasil_sim_traj_vis$month, day = hasil_sim_traj_vis$day, hour = hasil_sim_traj_vis$hour, hour.inc = hasil_sim_traj_vis$hour.inc,
                                       lat = hasil_sim_traj_vis$lat, lon = hasil_sim_traj_vis$lon, height = hasil_sim_traj_vis$height, pressure = hasil_sim_traj_vis$pressure, date2 = hasil_sim_traj_vis$date2, date = hasil_sim_traj_vis$date, cluster = result$cluster)
    colnames(tabel_clust_traj_vis) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date', 'cluster')
    #simulation consentration
    datacons <- datafile3[, - (1:5)]
    hasil_sim_cons_vis <- data.frame(Day = datacons$Day, Hour = datacons$Temporal2, Latitude = datacons$Latitude, Longitude = datacons$Longitude, CO2 = datacons$CO2, CO = datacons$CO)
    hasil_sim3 = data.frame(lat = datacons$Latitude, lon = datacons$Longitude, hour.inc = datacons$Temporal2)
    result3 = stdbscan(hasil_sim3, eps1 = input$eps1_cons_vis, eps2 = input$eps2_cons_vis, minpts = input$minPts_cons_vis)
    hasil3 = result3$cluster
    tabel_clust_cons_vis = data.frame(Latitude = datacons$Latitude, Longitude = datacons$Longitude, Hour = datacons$Temporal2, CO2 = datacons$CO2, CO = datacons$CO, Cluster = hasil3)
    
    #dataset for visualization
    dataplot_clust_traj <- tabel_clust_traj_vis
    dataplot_clust_stdbscan_cons <- tabel_clust_cons_vis
    
    #plot dataset simulation and cluster result for visualization
    #popup cluster trajectory ST-DBSCAN
    longitudepaste_clust_traj <- (paste("Longitude : ", dataplot_clust_traj$lon, "<br/>"))
    latitudepaste_clust_traj <- (paste("Latitude : ", dataplot_clust_traj$lat, "<br/>"))
    heightpaste_clust_traj <- (paste("Height: ", dataplot_clust_traj$height, "<br/>"))
    pressurepaste_clust_traj <- (paste("Pressure : ", dataplot_clust_traj$pressure, "<br/>"))
    datepaste_clust_traj <- (paste("Date : ", dataplot_clust_traj$date, "<br/>"))
    clusterpaste_clust_traj <- (paste("Cluster : ", dataplot_clust_traj$cluster, "<br/>"))
    popup3 <- (paste(longitudepaste_clust_traj, latitudepaste_clust_traj, clusterpaste_clust_traj, heightpaste_clust_traj, pressurepaste_clust_traj, datepaste_clust_traj, sep = ""))
    #popup cluster consentration ST-DBSCAN
    longitudepaste_clust_stdbscan_cons <- (paste("Longitude : ", dataplot_clust_stdbscan_cons$Longitude, "<br/>"))
    latitudepaste_clust_stdbscan_cons <- (paste("Latitude : ", dataplot_clust_stdbscan_cons$Latitude, "<br/>"))
    CO2paste_clust_stdbscan_cons <- (paste("CO2: ", dataplot_clust_stdbscan_cons$CO2, "<br/>"))
    COpaste_clust_stdbscan_cons <- (paste("CO : ", dataplot_clust_stdbscan_cons$CO, "<br/>"))
    Hourpaste_clust_stdbscan_cons <- (paste("Hour : ", dataplot_clust_stdbscan_cons$Hour, "<br/>"))
    clusterpaste_clust_stdbscan_cons <- (paste("Cluster : ", dataplot_clust_stdbscan_cons$Cluster, "<br/>"))
    popup4 <- (paste(longitudepaste_clust_stdbscan_cons, latitudepaste_clust_stdbscan_cons, CO2paste_clust_stdbscan_cons, COpaste_clust_stdbscan_cons, Hourpaste_clust_stdbscan_cons, clusterpaste_clust_stdbscan_cons, sep = ""))
    
    mymap = leaflet()
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    factpal3 <- colorFactor(topo.colors(1000), dataplot_clust_traj$cluster)
    factpal4 <- colorFactor(topo.colors(1000), dataplot_clust_stdbscan_cons$Cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot_clust_traj$lon, lat = dataplot_clust_traj$lat, fillColor = factpal3(dataplot_clust_traj$Cluster), fillOpacity = 1, stroke = FALSE, color = "darkorange", weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup3)
    lablegend1 <- unique(data.frame(factpal3(dataplot_clust_traj$cluster),dataplot_clust_traj$cluster))
    lablegend1$dataplot.Cluster
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend1$factpal3.dataplot_clust_traj.cluster.,
                      labels= lablegend1$dataplot_clust_traj.cluster,
                      title= "Cluster Traj",
                      opacity = 1)
    mymap = addCircleMarkers(mymap, lng = dataplot_clust_stdbscan_cons$Longitude, lat = dataplot_clust_stdbscan_cons$Latitude, fillColor = factpal4(dataplot_clust_stdbscan_cons$Cluster), fillOpacity = 1, stroke = FALSE, color = "blue", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup4)
    lablegend2 <- unique(data.frame(factpal4(dataplot_clust_stdbscan_cons$Cluster),dataplot_clust_stdbscan_cons$Cluster))
    lablegend2$dataplot.Cluster
    mymap = addLegend(mymap, "bottomleft", 
                      colors =lablegend2$factpal4.dataplot_clust_stdbscan_cons.Cluster.,
                      labels= lablegend2$dataplot_clust_stdbscan_cons.Cluster,
                      title= "Cluster Cons",
                      opacity = 1)
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #Plot cluster trajectory and consentration 
  plot_leafletviz_cons_kmeans <- reactive({
    datafile3 <- selectedData3()
    #simulation trajectory 
    sekuens <- data.frame(na.omit(datafile3[, 1:5]))
    hasil_sim_traj_vis = HYSPLIT_Traj(sekuens)
    colnames(hasil_sim_traj_vis) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    result = stdbscan(hasil_sim_traj_vis, eps1 = input$eps1_traj_vis, eps2 = input$eps2_traj_vis, minpts = input$minPts_traj_vis)
    tabel_clust_traj_vis <- data.frame(receptor = hasil_sim_traj_vis$receptor, year = hasil_sim_traj_vis$year, month = hasil_sim_traj_vis$month, day = hasil_sim_traj_vis$day, hour = hasil_sim_traj_vis$hour, hour.inc = hasil_sim_traj_vis$hour.inc,
                                       lat = hasil_sim_traj_vis$lat, lon = hasil_sim_traj_vis$lon, height = hasil_sim_traj_vis$height, pressure = hasil_sim_traj_vis$pressure, date2 = hasil_sim_traj_vis$date2, date = hasil_sim_traj_vis$date, cluster = result$cluster)
    colnames(tabel_clust_traj_vis) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date', 'cluster')
    #simulation consentration
    datacons <- datafile3[, - (1:5)]
    hasil_sim_cons_vis <- data.frame(Day = datacons$Day, Hour = datacons$Temporal2, Latitude = datacons$Latitude, Longitude = datacons$Longitude, CO2 = datacons$CO2, CO = datacons$CO)
    hasil_sim3 = data.frame(lat = datacons$Latitude, lon = datacons$Longitude, hour.inc = datacons$Temporal2)
    result3 = stdbscan(hasil_sim3, eps1 = input$eps1_cons_vis, eps2 = input$eps2_cons_vis, minpts = input$minPts_cons_vis)
    hasil3 = result3$cluster
    tabel_clust_cons_vis = data.frame(Latitude = datacons$Latitude, Longitude = datacons$Longitude, Hour = datacons$Temporal2, CO2 = datacons$CO2, CO = datacons$CO, Cluster = hasil3)
    data_clust_stdbscan_vis <- tabel_clust_cons_vis
    data_by_clust_vis <- subset(data_clust_stdbscan_vis, data_clust_stdbscan_vis$Cluster == input$Cluster_STDBSCAN_cons_vis)
    data_clust_kmeans_vis <- data.frame(data_by_clust_vis$CO2, data_by_clust_vis$CO)
    clust_kmeans_vis <- kmeans(data_clust_kmeans_vis, centers = input$K_cons_vis)
    tabel_clust_cons_kmeans_vis <- data.frame(Latitude = data_by_clust_vis$Latitude, Longitude = data_by_clust_vis$Longitude, CO2 = data_by_clust_vis$CO2, CO = data_by_clust_vis$CO, Cluster = clust_kmeans_vis$cluster)
    
    #dataset for visualization
    dataplot_clust_traj <- tabel_clust_traj_vis
    dataplot_clust_kmeans_cons <- tabel_clust_cons_kmeans_vis
    
    #plot dataset simulation and cluster result for visualization
    #popup cluster trajectory ST-DBSCAN
    longitudepaste_clust_traj <- (paste("Longitude : ", dataplot_clust_traj$lon, "<br/>"))
    latitudepaste_clust_traj <- (paste("Latitude : ", dataplot_clust_traj$lat, "<br/>"))
    heightpaste_clust_traj <- (paste("Height: ", dataplot_clust_traj$height, "<br/>"))
    pressurepaste_clust_traj <- (paste("Pressure : ", dataplot_clust_traj$pressure, "<br/>"))
    datepaste_clust_traj <- (paste("Date : ", dataplot_clust_traj$date, "<br/>"))
    clusterpaste_clust_traj <- (paste("Cluster : ", dataplot_clust_traj$cluster, "<br/>"))
    popup3 <- (paste(longitudepaste_clust_traj, latitudepaste_clust_traj, clusterpaste_clust_traj, heightpaste_clust_traj, pressurepaste_clust_traj, datepaste_clust_traj, sep = ""))
    #popup cluster consentration K-Means
    longitudepaste_clust_kmeans_cons <- (paste("Longitude : ", dataplot_clust_kmeans_cons$Longitude, "<br/>"))
    latitudepaste_clust_kmeans_cons <- (paste("Latitude : ", dataplot_clust_kmeans_cons$Latitude, "<br/>"))
    CO2paste_clust_kmeans_cons <- (paste("CO2: ", dataplot_clust_kmeans_cons$CO2, "<br/>"))
    COpaste_clust_kmeans_cons <- (paste("CO : ", dataplot_clust_kmeans_cons$CO, "<br/>"))
    clusterpaste_clust_kmeans_cons <- (paste("Cluster : ", dataplot_clust_kmeans_cons$Cluster, "<br/>"))
    popup5 <- (paste(longitudepaste_clust_kmeans_cons, latitudepaste_clust_kmeans_cons, CO2paste_clust_kmeans_cons, COpaste_clust_kmeans_cons, clusterpaste_clust_kmeans_cons, sep = ""))
    
    mymap = leaflet()
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    factpal3 <- colorFactor(topo.colors(1000), dataplot_clust_traj$cluster)
    factpal5 <- colorFactor(topo.colors(1000), dataplot_clust_kmeans_cons$Cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot_clust_traj$lon, lat = dataplot_clust_traj$lat, fillColor = factpal3(dataplot_clust_traj$cluster), fillOpacity = 1, stroke = FALSE, color = "blue", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup3)
    lablegend1 <- unique(data.frame(factpal3(dataplot_clust_traj$cluster),dataplot_clust_traj$cluster))
    lablegend1$dataplot.Cluster
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend1$factpal3.dataplot_clust_traj.cluster.,
                      labels= lablegend1$dataplot_clust_traj.cluster,
                      title= "Cluster Traj",
                      opacity = 1)
    mymap = addCircleMarkers(mymap, lng = dataplot_clust_kmeans_cons$Longitude, lat = dataplot_clust_kmeans_cons$Latitude, fillColor = factpal5(dataplot_clust_kmeans_cons$Cluster), fillOpacity = 1, stroke = FALSE, color = "blue", weight = 7, fill = TRUE, opacity = 1, radius = 1, popup = popup5)
    lablegend2 <- unique(data.frame(factpal5(dataplot_clust_kmeans_cons$Cluster),dataplot_clust_kmeans_cons$Cluster))
    lablegend2$dataplot.Cluster
    mymap = addLegend(mymap, "bottomleft", 
                      colors =lablegend2$factpal5.dataplot_clust_kmeans_cons.Cluster.,
                      labels= lablegend2$dataplot_clust_kmeans_cons.Cluster,
                      title= "Cluster Cons",
                      opacity = 1)    
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #--------------------------------------- OUTPUT FUNCTION PANEL VISUALIZATION -----------------------------------------#
  
  #Title in Tab Panel Visual Simulation Trajectory and Consentration
  output$caption_vis <- renderText({
    if (input$date_Period3 == "")
      paste("Visualization of Simulation Results from Trajectory and Pollutant Consentration using HYSPLIT Model")
    else
      paste("Visualization of Simulation Results from Trajectory and Pollutant Consentration using HYSPLIT Model on Data", title_periode3());
  })
  
  #Title in Tab Panel Visual Cluster Trajectory and Consentration ST-DBSCAN
  output$caption_vis_clust_stdbscan <- renderText({
    paste("Visual Cluster Algoritme ST-DBSCAN of Trajectory and Consentration Model HYSPLIT on Data", title_periode3());
  })
  
  #Title in Tab Panel Clustering Consentration K-Means
  output$caption_vis_clust_kmeans <- renderText({
    paste("Visual Cluster Algoritme K-Means of Consentration Model HYSPLIT in Cluster Algoritme ST-DBSCAN Result on Data", title_periode3());
  })
  
  #Output render plot
  output$showplotleaflet6 <- renderLeaflet({
    input$go_vis
    if (input$go_vis == 0)
      return("Please click 'Start Visual Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leafletviz_sim())
  })
  
  #Output render plot
  output$showplotleaflet7 <- renderLeaflet({
    input$go_clustvis_stdbscan
    if (input$go_clustvis_stdbscan == 0)
      return("Please click 'Start Visual Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leafletviz_cons())
  })
  
  #Output render plot
  output$showplotleaflet8 <- renderLeaflet({
    input$go_clustvis_kmeans
    if (input$go_clustvis_kmeans == 0)
      return("Please click 'Start Visual Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(plot_leafletviz_cons_kmeans())
  })
  
  #----------------------------------------- MENU POLLUTANT VISUALIZARION ECMFW ----------------------------------------
  #-------------------------------------- Sub Menu Summary Plot--------------------------------------------------------
  summaryPol <- reactive({
    datafile4 <- selectedData4()
    datafile4$co <- abs(datafile4$co)
    datafile4$co2 <- abs(datafile4$co2)
    datafile4$nh3 <- abs(datafile4$nh3)
    datafile4$ch4 <- abs(datafile4$ch4)
    datafile4$pm25 <- abs(datafile4$pm25)
    datafile4$date <- as.POSIXct(strptime(datafile4$date, format = "%d/%m/%Y"))
    plotBaru <- summaryPlot(subset(datafile4, select = c(date,co, co2, nh3, ch4, pm25)), type = FALSE, period = "months", main = "Summary Plot Pollutants Emission July-November 2015")
    
  })
 
 
  #---------------------------------- Sub Menu Calender Plot by Location ------------------------------------
  calenderPolbyLoc <- reactive({
    datafile6 <- selectedData6()
    datafile6$co <- abs(datafile6$co)
    datafile6$co2 <- abs(datafile6$co2)
    datafile6$nh3 <- abs(datafile6$nh3)
    datafile6$ch4 <- abs(datafile6$ch4)
    datafile6$pm25 <- abs(datafile6$pm25)    
    datafile6$date <- as.POSIXct(strptime(datafile6$date, format = "%d/%m/%Y"))
    loc <- datafile6
    if(input$date_Period61 == 'JAMBI') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on Jambi in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on Jambi in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on Jambi in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on Jambi in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on Jambi in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission of",input$date_Period62 , "on", "Jambi", "in", 2015))
    } else if (input$date_Period61 == 'RIAU') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on Riau in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on Riau in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on Riau in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on Riau in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on Riau in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Riau", "in", 2015))
    } else if (input$date_Period61 == 'SUMATERA SELATAN') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on South Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on South Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on South Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on South Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on South Sumatra in 2015 ", "( ", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Sumatra Selatan", "in", 2015))
    } else if (input$date_Period61 == 'ACEH') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on Aceh in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on Aceh in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on Aceh in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on Aceh in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on Aceh in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Aceh", "in", 2015))
    } else if (input$date_Period61 == 'SUMATERA UTARA') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on North Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on North Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on North Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on North Sumatra in 2015 ", "( ", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on North Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Sumatra Utara", "in", 2015))
    } else if (input$date_Period61 == 'BENGKULU') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on Bengkulu in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on Bengkulu in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on Bengkulu in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on Bengkulu in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on Bengkulu in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Bengkulu", "in", 2015))
    } else if (input$date_Period61 == 'LAMPUNG') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on Lampung in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on Lampung in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on Lampung in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on Lampung in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on Lampung in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      #plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, statistic = "max",  main = paste("Pollutant Emission",input$date_Period62 , "on", "Lampung", "in", 2015))
    } else if (input$date_Period61 == 'SUMATERA BARAT') {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " on West Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " on West Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " on West Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " on West Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " on West Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
    }
    else {
      if(input$date_Period62=="co") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 140000), statistic = "max", main = expression(paste("Pollutant Emission of ",CO, " in Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="co2") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 1000000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CO[2], " in Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      else if(input$date_Period62=="ch4") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 14000), statistic = "max",  main = expression(paste("Pollutant Emission of ",CH[4], " in Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
      
      else if(input$date_Period62=="nh3") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 12000), statistic = "max",  main = expression(paste("Pollutant Emission of ", NH[3], " in Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
        
      }
      else if(input$date_Period62=="pm25") {
        plotBaru2 <- calendarPlot(loc, pollutant = input$date_Period62, limits = c(0, 6000), statistic = "max",  main = expression(paste("Pollutant Emission of ",PM[2.5], " in Sumatra in 2015 ", "(", mu, gram,"/", "(", m^{2}, sekon, "))")))
      }
    }

    
  })
  
  #-----------------------------------Sub Menu Simulation --------------------------------------
  tabel_simPol <- reactive({
    datafile7 <- selectedData7()
    datafile7$co <- abs(datafile7$co)
    datafile7$co2 <- abs(datafile7$co2)
    datafile7$nh3 <- abs(datafile7$nh3)
    datafile7$ch4 <- abs(datafile7$ch4)
    datafile7$pm25 <- abs(datafile7$pm25)
    mon <- month(as.POSIXct(datafile7$date))
    results <- datafile7[which(mon==input$date_Period71),]
    result <- datafile7[which(mon==input$date_Period71), 1:4]
    if(input$date_Period73=="co") {
      polutan <- results$co
    } else if (input$date_Period73=="co2") {
      polutan <- results$co2
    } else if (input$date_Period73=="nh3") {
      polutan <- results$nh3
    } else if(input$date_Period73=="ch4") {
      polutan <- results$ch4
    } else if(input$date_Period73=="pm25") {
      polutan <- results$pm25
    }
    datafile7 <- cbind(result,polutan)
    hasil_sim_pol <- data.frame(Date = datafile7$date, Lon = datafile7$longitude, Lat= datafile7$latitude, Polutan= datafile7$polutan)
    
    list(hasil_sim_pol = hasil_sim_pol)
    
  })
  
  
  #---------------------------plot simulation emission--------------------------------------
  plot_leaflet_consPul <- reactive({
    dataplot <- tabel_simPol()$hasil_sim_pol
    
    #ambil data untuk legend 
    input$date_Period71
    ambilpol <- selectedData7()
    mon <- month(as.POSIXct(ambilpol$date))
    result <- ambilpol[which(mon==input$date_Period71),]
    dataplot$color <- as.character(dataplot$Polutan) 
    
    
    datepaste <- (paste("Date : ", dataplot$Date, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$Lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$Lat, "<br/>"))
    polpaste <- (paste("Kadar Polutan: ", dataplot$Polutan, "<br/>"))
    popup1 <- (paste(datepaste, longitudepaste, latitudepaste, polpaste, sep= ""))
    mymap = leaflet()
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
   
    
    if(input$date_Period73== "co") {
      dataplot$color[dataplot$Polutan >= 0 & dataplot$Polutan<= 20000] <- "#3cd4ef" #"bright blue"
      dataplot$color[dataplot$Polutan > 20000 & dataplot$Polutan <=60000] <- "#fdfa2d" #yellow
      dataplot$color[dataplot$Polutan > 60000 & dataplot$Polutan <= 100000] <- "#e4951c" #dark orange
      dataplot$color[dataplot$Polutan > 100000] <- "#a50000" #red
      
      dataplot$size <- as.character(dataplot$Polutan)
      dataplot$size[dataplot$Polutan >= 0 & dataplot$Polutan <= 60000] <- "1.5"
      dataplot$size[dataplot$Polutan > 60000 & dataplot$Polutan <= 100000] <- "4"
      dataplot$size[dataplot$Polutan >100000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, color= dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot$size, popup = popup1)
      
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-20000","20001-60000", "60001-100000", ">100000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CO",
                        opacity = 1)
      
      
    }
    if(input$date_Period73=="co2") {
      dataplot$color[dataplot$Polutan >= 0 & dataplot$Polutan <= 200000] <- "#3cd4ef" #" blue"
      dataplot$color[dataplot$Polutan > 200000 & dataplot$Polutan <=400000] <- "#fdfa2d" #yellow
      dataplot$color[dataplot$Polutan > 400000 & dataplot$Polutan <= 800000] <- "#e4951c" #darkorange
      dataplot$color[dataplot$Polutan > 800000] <- "#a50000" #red
      
      dataplot$size <- as.character(dataplot$Polutan)
      dataplot$size[dataplot$Polutan>= 0 & dataplot$Polutan <= 400000] <- "1.5"
      dataplot$size[dataplot$Polutan > 400000 & dataplot$Polutan <= 800000] <- "4"
      dataplot$size[dataplot$Polutan > 800000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, color= dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot$size, popup = popup1)
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-200000", "200001-400000", "400001-800000", ">800000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CO2 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    
    if(input$date_Period73=="pm25") {
      dataplot$color[dataplot$Polutan >= 0 & dataplot$Polutan <= 1000] <- "#3cd4ef" #"bright blue"
      dataplot$color[dataplot$Polutan > 1000 & dataplot$Polutan <= 3000] <- "#fdfa2d" #yellow
      dataplot$color[dataplot$Polutan > 3000 & dataplot$Polutan <= 5000] <- "#e4951c" #orange
      dataplot$color[dataplot$Polutan > 5000] <- "#a50000" #red
      
      dataplot$size <- as.character(dataplot$Polutan)
      dataplot$size[dataplot$Polutan >= 0 & dataplot$Polutan <= 3000] <- "1.5"
      dataplot$size[dataplot$Polutan > 3000 & dataplot$Polutan <= 5000] <- "4"
      dataplot$size[dataplot$Polutan > 5000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, color= dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot$size, popup = popup1)
    
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-1000", "1001-3000", "3001-5000", ">5000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission PM2.5 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    if(input$date_Period73=="ch4") {
      dataplot$color[dataplot$Polutan >= 0 & dataplot$Polutan <= 2000] <- "#3cd4ef" #"bright blue"
      dataplot$color[dataplot$Polutan > 2000 & dataplot$Polutan <= 6000] <- "#fdfa2d" #yellow
      dataplot$color[dataplot$Polutan > 6000 & dataplot$Polutan <= 10000] <- "#e4951c" #orange
      dataplot$color[dataplot$Polutan > 10000] <- "#a50000" #red
      
      dataplot$size <- as.character(dataplot$Polutan)
      dataplot$size[dataplot$Polutan >= 0 &dataplot$Polutan <= 6000] <- "1.5"
      dataplot$size[dataplot$Polutan > 6000 & dataplot$Polutan <= 10000] <- "4"
      dataplot$size[dataplot$Polutan > 10000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, color= dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot$size, popup = popup1)
    
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-2000", "2001-6000", "6001-10000", ">10000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CH4 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    
    if(input$date_Period73=="nh3") {
      dataplot$color[dataplot$Polutan >= 0 & dataplot$Polutan <= 2000] <- "#3cd4ef" #"bright blue"
      dataplot$color[dataplot$Polutan > 2000 & dataplot$Polutan <=6000] <- "#fdfa2d" #yellow
      dataplot$color[dataplot$Polutan > 6000 & dataplot$Polutan <= 10000] <- "#e4951c" #orange
      dataplot$color[dataplot$Polutan > 10000] <- "#a50000" #red
      
      dataplot$size <- as.character(dataplot$Polutan)
      dataplot$size[dataplot$Polutan >= 0 & dataplot$Polutan <= 6000] <- "1.5"
      dataplot$size[dataplot$Polutan > 6000 & dataplot$Polutan <= 10000] <- "4"
      dataplot$size[dataplot$Polutan > 10000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, color= dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot$size, popup = popup1)
      
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-2000", "2001-6000", "6001-10000", ">10000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission NH3 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })                                                 
  
  output$simulation_pol <- renderPrint({
    input$go_maps
    if (input$go_maps == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Pollutant Emission in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      disable("go_consPul")
      disable("go_calPul")
      disable("go_calPulLoc")
      disable("go_maps")
      disable("go_clustpol")
      disable("go_vispol")
      
    }
    enable("go_consPul")
    enable("go_calPul")
    enable("go_calPulLoc")
    enable("go_maps")
    enable("go_clustpol")
    enable("go_vispol")
    
    resultSimul <- tabel_simPol()$hasil_sim_pol
    colnames(resultSimul) <- c('Date', 'Longitude', 'Latitude', 'Polutant (mikrogram/m2s)')
    isolate(output$simulation_pol  <- renderDataTable(
     resultSimul , options = list(paging = TRUE,
                                                   searching = FALSE,
                                                   searchable = FALSE,
                                                   pageLength = 10,
                                                   sort = TRUE)
     
      
    ))
    
  })
  
  #------------------------Sub Menu Cluster K-Means-------------------------------------
  stat_cluster_pol <- reactive({
    datafile8 <- selectedData8()
    datafile8$co <- abs(datafile8$co)
    datafile8$co2 <- abs(datafile8$co2)
    datafile8$nh3 <- abs(datafile8$nh3)
    datafile8$ch4 <- abs(datafile8$ch4)
    datafile8$pm25 <- abs(datafile8$pm25)
    datafile8new <-datafile8
    datafile8$co <- ((datafile8$co-min(datafile8$co))/(max(datafile8$co)-min(datafile8$co)))
    datafile8$co2 <- ((datafile8$co2-min(datafile8$co2))/(max(datafile8$co2)-min(datafile8$co2)))
    datafile8$ch4 <- ((datafile8$ch4-min(datafile8$ch4))/(max(datafile8$ch4)-min(datafile8$ch4)))
    datafile8$nh3 <- ((datafile8$nh3-min(datafile8$nh3))/(max(datafile8$nh3)-min(datafile8$nh3)))
    datafile8$pm25 <- ((datafile8$pm25-min(datafile8$pm25))/(max(datafile8$pm25)-min(datafile8$pm25)))
    dataclustpol <- data.frame(datafile8$co, datafile8$co2, datafile8$nh3, datafile8$ch4, datafile8$pm25)
    inputvalue <- input$kvalue
    clustpol <- kmeans(dataclustpol, centers=input$kvalue)
    tabel_clustpol <- data.frame(Date = datafile8$date, Lon = datafile8$longitude, Lat= datafile8$latitude, CO2 = datafile8new$co2, CO = datafile8new$co, CH4= datafile8new$ch4, NH3=datafile8new$nh3, PM25=datafile8new$pm25,  Cluster = clustpol$cluster)
    list(tabel_clustpol = tabel_clustpol)
    
  })
  
  
  #-----------------------Plot Clustering K-Means------------------------------------------
  plot_leaflet_clustPol <- reactive({
    dataplot <- stat_cluster_pol()$tabel_clustpol
    datepaste <- (paste("Date : ", dataplot$Date, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$Lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$Lat, "<br/>"))
    CO2paste <- (paste("CO2: ", dataplot$CO2, "<br/>"))
    COpaste <- (paste("CO: ", dataplot$CO, "<br/>"))
    NH3paste <- (paste("NH3: ", dataplot$NH3, "<br/>"))
    CH4paste <- (paste("CH4: ", dataplot$CH4, "<br/>"))
    PM25paste <- (paste("CH4: ", dataplot$PM25, "<br/>"))
    popup1 <- (paste(longitudepaste, latitudepaste, CO2paste, COpaste, NH3paste, CH4paste, PM25paste, sep= ""))
    mymap = leaflet()
    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    factpal <- colorFactor(topo.colors(1000), dataplot$Cluster)
    mymap = addCircleMarkers(mymap, lng = dataplot$Lon, lat = dataplot$Lat, fillColor = factpal(dataplot$Cluster), fillOpacity = 1, stroke = FALSE, color = "blue", weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)
    lablegend <- unique(data.frame(factpal(dataplot$Cluster),dataplot$Cluster))
    lablegend$dataplot.Cluster
    mymap = addLegend(mymap, "bottomright", 
                      colors =lablegend$factpal.dataplot.Cluster.,
                      labels= lablegend$dataplot.Cluster,
                      title= "Cluster",
                      opacity = 1)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
  })
  
  #Output for Summary Table of Emissions Clustering
  output$summary_clust_pol <- renderPrint({
    input$go_clustpol
    if (input$go_clustpol == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Summary of Cluster Pollutants Emission using K-Means algorithm in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      disable("go_consPul")
      disable("go_calPul")
      disable("go_calPulLoc")
      disable("go_maps")
      disable("go_clustpol")
      disable("go_vispol")
    }
    
    enable("go_consPul")
    enable("go_calPul")
    enable("go_calPulLoc")
    enable("go_maps")
    enable("go_clustpol")
    enable("go_vispol")
    
    resultClust <- stat_cluster_pol()$tabel_clustpol
    colnames(resultClust) <- c('Date', 'Longitude', 'Latitude', 'CO', 'CO2', 'CH4', 'NH3', 'PM2.5', 'Cluster')
    isolate(output$summary_clust_pol <- renderDataTable(
      resultClust, options = list(paging = TRUE,
                                                        searching = FALSE,
                                                        searchable = FALSE,
                                                        pageLength = 10,
                                                        sort = TRUE)
      
    ))
  })
  
  #---------------------------------------Menu Visualization Pollutant -----------------------------------------------
  
  plot_leafletviz_pol <- reactive({
    datafile9 <- selectedData9()
    datafile9$co <- abs(datafile9$co)
    datafile9$co2 <- abs(datafile9$co2)
    datafile9$nh3 <- abs(datafile9$nh3)
    datafile9$ch4 <- abs(datafile9$ch4)
    datafile9$pm25 <- abs(datafile9$pm25)
    #simulation trajectory 
    sekuens <- data.frame(na.omit(datafile9[, 1:5]))
    hasil_sim_traj9 = HYSPLIT_Traj(sekuens)
    colnames(hasil_sim_traj9) <- c('receptor', 'year', 'month', 'day', 'hour', 'hour.inc', 'lat', 'lon', 'height', 'pressure', 'date2', 'date')
    
    hasil_sim_traj9$date4 <- format(as.Date(hasil_sim_traj9$date2), "%d/%m/%Y")
    
    #simulation consentration
    datacons <- datafile9[, - (1:5)]
    datacons1 <- datacons[which(datacons$date==hasil_sim_traj9$date4), ]
    if(input$date_Period92=="co") {
      polutan <- datacons1$co
    } else if (input$date_Period92=="co2") {
      polutan <- datacons1$co2
    } else if (input$date_Period92=="nh3") {
      polutan <- datacons1$nh3
    } else if(input$date_Period92=="ch4") {
      polutan <- datacons1$ch4
    } else if(input$date_Period92=="pm25") {
      polutan <- datacons1$pm25
    }
    datacons <- cbind(datacons1, polutan)
    hasil_sim_cons9 <- data.frame(Date = datacons$date, Lon = datacons$longitude, Lat= datacons$latitude, Polutan= datacons$polutan)
    
    #dataset for visualization
    dataplot_sim_traj <- hasil_sim_traj9
    dataplot_sim_cons <- hasil_sim_cons9
  
    #warna
    dataplot_sim_traj$color <- as.character(dataplot_sim_traj$receptor)
    dataplot_sim_traj$color[dataplot_sim_traj$color == "1"] <- "#F44336" #"red"
    dataplot_sim_traj$color[dataplot_sim_traj$color == "2"] <- "#4CAF50" #green
    dataplot_sim_traj$color[dataplot_sim_traj$color == "3"] <- "#e44727" #pink salem
    dataplot_sim_traj$color[dataplot_sim_traj$color == "4"] <- "#0001ff" #blue
    dataplot_sim_traj$color[dataplot_sim_traj$color == "5"] <- "#FF9800" #orange
    dataplot_sim_traj$color[dataplot_sim_traj$color == "6"] <- "#FFEB3B" #yellow
    dataplot_sim_traj$color[dataplot_sim_traj$color == "7"] <- "#673AB7" #deep purple
    dataplot_sim_traj$color[dataplot_sim_traj$color == "8"] <- "black"
    dataplot_sim_traj$color[dataplot_sim_traj$color == "9"] <- "#795548" #brown
    dataplot_sim_traj$color[dataplot_sim_traj$color == "10"] <- "#8BC34A" #light green
    dataplot_sim_traj$color[dataplot_sim_traj$color == "11"] <- "#BA68C8" #purple
    dataplot_sim_traj$color[dataplot_sim_traj$color == "12"] <- "#766d00" 
    dataplot_sim_traj$color[dataplot_sim_traj$color == "13"] <- "#009688" #teal
    dataplot_sim_traj$color[dataplot_sim_traj$color == "14"] <- "#E91E63" #pink
    dataplot_sim_traj$color[dataplot_sim_traj$color == "15"] <- "#3F51B5" #indigo
    dataplot_sim_traj$color[dataplot_sim_traj$color == "16"] <- "#CDDC39" #lime
    dataplot_sim_traj$color[dataplot_sim_traj$color == "17"] <- "#FFC107" #amber
    dataplot_sim_traj$color[dataplot_sim_traj$color == "18"] <- "#FF5722" #deeporange
    
    #popup simulation trajectory
    longitudepaste_sim_traj <- (paste("Longitude : ", dataplot_sim_traj$lon, "<br/>"))
    latitudepaste_sim_traj <- (paste("Latitude : ", dataplot_sim_traj$lat, "<br/>"))
    heightpaste_sim_traj <- (paste("Height: ", dataplot_sim_traj$height, "<br/>"))
    pressurepaste_sim_traj <- (paste("Pressure : ", dataplot_sim_traj$pressure, "<br/>"))
    datepaste_sim_traj <- (paste("Date : ", dataplot_sim_traj$date, "<br/>"))
    hourpaste_sim_traj <- (paste("Hour : ", dataplot_sim_traj$hour, "<br/>"))
    popup2 <- (paste(longitudepaste_sim_traj, latitudepaste_sim_traj, heightpaste_sim_traj, pressurepaste_sim_traj, datepaste_sim_traj,hourpaste_sim_traj, sep = ""))
    
    #popup simulation emissions
    longitudepaste_sim_cons <- (paste("Longitude : ", dataplot_sim_cons$Lon, "<br/>"))
    latitudepaste_sim_cons <- (paste("Latitude : ", dataplot_sim_cons$Lat, "<br/>"))
    polpaste_sim_cons <- (paste("Kadar Polutan: ", dataplot_sim_cons$Polutan, "<br/>"))
    popup1 <- (paste(longitudepaste_sim_cons, latitudepaste_sim_cons,  polpaste_sim_cons, sep = ""))
    
    mymap = leaflet()
    addProviderTiles(
      mymap,
      "OpenStreetMap",
      group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))
    dataplot_sim_cons$color <- as.character(dataplot_sim_cons$Polutan) 
    
    if(input$date_Period92== "co") {
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 20000] <- "#3cd4ef" #"soft blue"
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 20000 & dataplot_sim_cons$Polutan <=60000] <- "#fdfa2d" #yellow
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 60000 & dataplot_sim_cons$Polutan <= 100000] <- "#e4951c" #orange
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 100000] <- "#a50000" #red
      
      dataplot_sim_cons$size <- as.character(dataplot_sim_cons$Polutan)
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 60000] <- "1.5"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 60000 & dataplot_sim_cons$Polutan<= 100000] <- "4"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 100000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot_sim_cons$Lon, lat = dataplot_sim_cons$Lat, color= dataplot_sim_cons$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot_sim_cons$size, popup = popup1)
      
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-20000","20001-60000", "60001-100000", ">100000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CO (mikrogram/m2s)",
                        opacity = 1)
      
      
    }
    if(input$date_Period92=="co2") {
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 200000] <- "#3cd4ef" #"soft blue"
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 200000 & dataplot_sim_cons$Polutan <=400000] <- "#fdfa2d" #yellow
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 400000 & dataplot_sim_cons$Polutan <= 800000] <- "#e4951c" #orange
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 800000] <- "#a50000" #red
      
      dataplot_sim_cons$size <- as.character(dataplot_sim_cons$Polutan)
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 400000] <- "1.5"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 400000 & dataplot_sim_cons$Polutan <= 800000] <- "4"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 800000] <- "6"
     
      mymap=addCircleMarkers(mymap, lng = dataplot_sim_cons$Lon, lat = dataplot_sim_cons$Lat, color= dataplot_sim_cons$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot_sim_cons$size, popup = popup1)
      
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-200000", "200001-400000", "400001-800000", ">800000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CO2 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    
    if(input$date_Period92=="pm25") {
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 1000] <- "#3cd4ef" #" soft blue" 
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 1000 & dataplot_sim_cons$Polutan <= 3000] <- "#fdfa2d" #yellow
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 3000 & dataplot_sim_cons$Polutan <= 5000] <- "#e4951c" #orange
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 5000] <- "#a50000" #red
      
      dataplot_sim_cons$size <- as.character(dataplot_sim_cons$Polutan)
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 3000] <- "1.5"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 3000 & dataplot_sim_cons$Polutan <= 5000] <- "4"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 5000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot_sim_cons$Lon, lat = dataplot_sim_cons$Lat, color= dataplot_sim_cons$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot_sim_cons$size, popup = popup1)
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-1000", "1001-3000", "3001-5000", ">5000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission PM2.5 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    if(input$date_Period92=="ch4") {
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 2000] <- "#3cd4ef" #"bright blue"
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 2000 & dataplot_sim_cons$Polutan <= 6000] <- "#fdfa2d" #yellow
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 6000 & dataplot_sim_cons$Polutan <= 10000] <- "#e4951c" #orange
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 10000] <- "#a50000" #red
      
      dataplot_sim_cons$size <- as.character(dataplot_sim_cons$Polutan)
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 6000] <- "1.5"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 6000 & dataplot_sim_cons$Polutan <= 10000] <- "4"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 10000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot_sim_cons$Lon, lat = dataplot_sim_cons$Lat, color= dataplot_sim_cons$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot_sim_cons$size, popup = popup1)
      
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-2000", "2001-6000", "6001-10000", ">10000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission CH4 (mikrogram/m2s)",
                        opacity = 1)
      
    }
    
    if(input$date_Period92=="nh3") {
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 2000] <- "#3cd4ef" #"bright blue"
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 2000 & dataplot_sim_cons$Polutan <= 6000] <- "#fdfa2d" #yellow
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 6000 & dataplot_sim_cons$Polutan <= 10000] <- "#e4951c" #orange
      dataplot_sim_cons$color[dataplot_sim_cons$Polutan > 10000] <- "#a50000" #red
      
      dataplot_sim_cons$size <- as.character(dataplot_sim_cons$Polutan)
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan >= 0 & dataplot_sim_cons$Polutan <= 6000] <- "1.5"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 6000 & dataplot_sim_cons$Polutan <= 10000] <- "3"
      dataplot_sim_cons$size[dataplot_sim_cons$Polutan > 10000] <- "6"
      
      mymap=addCircleMarkers(mymap, lng = dataplot_sim_cons$Lon, lat = dataplot_sim_cons$Lat, color= dataplot_sim_cons$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = dataplot_sim_cons$size, popup = popup1)
      
      mymap = addLegend(mymap, "bottomright", 
                        labels = c("0-2000", "2001-6000", "6001-10000", ">10000"),
                        colors= c("#3cd4ef", "#fdfa2d", "#e4951c", "#a50000"),
                        title= "Emission NH3 (mikrogram/m2s)",
                        opacity = 1)
      
    }
   
    
    mymap = addCircleMarkers(mymap, lng = dataplot_sim_traj$lon, lat = dataplot_sim_traj$lat, color =  dataplot_sim_traj$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup2)
    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot_sim_traj,dataplot_sim_traj$hour.inc==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    
    
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })
  
  #-----------------------------------------Summary Cluster------------------------------------------------------------

  sum_cluster_pol <- reactive({
    datafile8 <- selectedData8()
    datafile8$co <- abs(datafile8$co)
    datafile8$co2 <- abs(datafile8$co2)
    datafile8$nh3 <- abs(datafile8$nh3)
    datafile8$ch4 <- abs(datafile8$ch4)
    datafile8$pm25 <- abs(datafile8$pm25)
    datafile8new1 <- datafile8
    datafile8$co <- ((datafile8$co-min(datafile8$co))/(max(datafile8$co)-min(datafile8$co)))
    datafile8$co2 <- ((datafile8$co2-min(datafile8$co2))/(max(datafile8$co2)-min(datafile8$co2)))
    datafile8$ch4 <- ((datafile8$ch4-min(datafile8$ch4))/(max(datafile8$ch4)-min(datafile8$ch4)))
    datafile8$nh3 <- ((datafile8$nh3-min(datafile8$nh3))/(max(datafile8$nh3)-min(datafile8$nh3)))
    datafile8$pm25 <- ((datafile8$pm25-min(datafile8$pm25))/(max(datafile8$pm25)-min(datafile8$pm25)))
    dataclustpol <- data.frame(datafile8$co, datafile8$co2, datafile8$nh3, datafile8$ch4, datafile8$pm25)
    inputvalue <- input$kvalue
    clustpol <- kmeans(dataclustpol, centers=input$kvalue)
    tabel_clustpol <- data.frame(Date = datafile8$date, Lon = datafile8$longitude, Lat= datafile8$latitude, CO2 = datafile8new1$co2, CO = datafile8new1$co, CH4= datafile8new1$ch4, NH3=datafile8new1$nh3, PM25=datafile8new1$pm25,  Cluster = clustpol$cluster)
    sum_cluster <- tabel_clustpol
    
    
    inputv <- length(unique(sum_cluster$Cluster))
    rataCO <- array(1:inputv)
    rataCO2 <- array(1:inputv)
    rataNH3 <- array(1:inputv)
    rataCH4 <- array(1:inputv)
    rataPM25 <- array(1:inputv)
    
    for(i in 1:inputv) {
      ambildata <- sum_cluster[which(sum_cluster$Cluster == i),]
      rataCO[i]<- mean(ambildata$CO)
      rataCO2[i] <- mean(ambildata$CO2)
      rataNH3[i]<- mean(ambildata$NH3)
      rataCH4[i] <- mean(ambildata$CH4)
      rataPM25[i] <- mean(ambildata$PM25)
    }
    jmlhClust <- count(sum_cluster$Cluster)
    tabel_sum_clust <- data.frame(Cluster = jmlhClust$x, Total= jmlhClust$freq, AverageCO = as.factor(round(rataCO, 6)), AverageCO2 = as.factor(round(rataCO2,6)), AverageNH3= as.factor(round(rataNH3,6)), AverageCH4= as.factor(round(rataCH4,6)), AveragePM25=as.factor(round(rataPM25,6)))
    colnames(tabel_sum_clust) <- c('Cluster', 'Number of Objects', 'CO (mikrogram/m2s)', 'CO2 (mikrogram/m2s)', 'NH3 (mikrogram/m2s)', 'CH4 (mikrogram/m2s)','PM2.5 (mikrogram/m2s)')
    list(tabel_sum_clust = tabel_sum_clust)
  })
  
  output$sum_clust_pol <- renderPrint({
    input$go_clustpol
    if (input$go_clustpol == 0)
      return("Please choose K value and start clustering on tab 'Cluster K-Means'")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Summary of Cluster Pollutants Emission using K-Means Algorithm in Progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      disable("go_consPul")
      disable("go_calPul")
      disable("go_calPulLoc")
      disable("go_maps")
      disable("go_clustpol")
      disable("go_vispol")
      
    }
    
    enable("go_consPul")
    enable("go_calPul")
    enable("go_calPulLoc")
    enable("go_maps")
    enable("go_clustpol")
    enable("go_vispol")
    isolate(output$sum_clust_pol <- renderDataTable(
      sum_cluster_pol()$tabel_sum_clust, options = list(paging = TRUE,
                                                        searching = FALSE,
                                                        searchable = FALSE,
                                                        pageLength = 10,
                                                        sort = TRUE)
    ))
  })
  
  #--------------------------------------------Warning----------------------------------------------------------------
  output$warningSum <- renderText({
    
    if(is.null(input$datecons)) {
      createAlert(session, "alert1",  "warningSum", title = "Oops",
                  content = "Please choose the period", append = FALSE)
    } 
    else {
      closeAlert(session, "warningSum")
      
    }
    
  })
  #-------------------------------------------Output ------------------------------------------------------------------
  output$plot1 <- renderPlot({
    input$go_consPul
    if (input$go_consPul == 0)
      return("Please click 'Start'")
    else 
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      disable("go_consPul")
      disable("go_calPul")
      disable("go_calPulLoc")
      disable("go_maps")
      disable("go_clustpol")
      disable("go_vispol")
      
      
    }
    isolate(summaryPol())
    enable("go_consPul")
    enable("go_calPul")
    enable("go_calPulLoc")
    enable("go_maps")
    enable("go_clustpol")
    enable("go_vispol")
    
      
   
    
  })
  
  output$plot2 <- renderPlot({
    input$go_calPul
    if (input$go_calPul == 0)
      return("Please click 'Start'")
    else 
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
      on.exit(progress$close())
      progress$set(message = 'Plotting in progress')
      
      for (i in 1:2) {
        progress$set(value = i)
        Sys.sleep(1)
        disable("go_consPul")
        disable("go_calPul")
        disable("go_calPulLoc")
        disable("go_maps")
        disable("go_clustpol")
        disable("go_vispol")
        
      }
      isolate(calenderPol())
      enable("go_consPul")
      enable("go_calPul")
      enable("go_calPulLoc")
      enable("go_maps")
      enable("go_clustpol")
      enable("go_vispol")
     
  })
  
  
  output$plot3 <- renderPlot({

    
    if (input$go_calPulLoc == 0) {
      return("Please click 'Start'")
    }else  {
      
      
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
      on.exit(progress$close())
      progress$set(message = 'Plotting in progress')
      
      for (i in 1:2) {
        progress$set(value = i)
        Sys.sleep(1)
        disable("go_consPul")
        disable("go_calPul")
        disable("go_calPulLoc")
        disable("go_maps")
        disable("go_clustpol")
        disable("go_vispol")
        
      }
      
      isolate(calenderPolbyLoc())
      
      enable("go_consPul")
      enable("go_calPul")
      enable("go_calPulLoc")
      enable("go_maps")
      enable("go_clustpol")
      enable("go_vispol")
      
      
    
    }
  })
  
  output$plot4 <- renderLeaflet({
    input$go_maps
    if (input$go_maps == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
     
      
      
    }
    isolate(plot_leaflet_consPul())
    
    
  })
  
  output$plot5 <- renderLeaflet({
    input$go_clustpol
    if (input$go_clustpol == 0)
      return("Please click 'Start  Clustering' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      
      
    }
    isolate(plot_leaflet_clustPol())
    
    
  })
  
  output$plot6 <- renderLeaflet({
    input$go_vispol
    if (input$go_vispol == 0)
      return("Please click 'Start' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Plotting in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
      
      
    }
    isolate(plot_leafletviz_pol())
    
  })
  
  #--------------------------------------- OUTPUT FUNCTION PANEL ABOUT -----------------------------------------#
  
  #Table Kabupaten of 'About' Menu
  output$tabel_kabupaten <- renderTable({
    dbGetQuery(con, "select * from sumatra_kabupaten15")
  })
  
  #Table Parameter Trajectory of 'About' Menu
  output$tabel_parameter_trajectory <- renderTable({
    dbGetQuery(con, "select * from parameter_trajectory")
  })
  
  #Table Parameter Consentration of 'About' Menu
  output$tabel_parameter_consentration <- renderTable({
    dbGetQuery(con, "select * from parameter_consentration")
  })
  
  #Table Parameter Emission of 'About' Menu
  output$tabel_parameter_emission <- renderTable({
    dbGetQuery(con, "select * from parameter_emissions")
  })
  
  }
)