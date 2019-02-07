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
library("C50")
library("caret")
#convertToRaster
library(raster)
library(rgdal)
library(sp)


shinyServer(function(input, output, session)
{
  #CONNECT DATABASE
  # pg = dbDriver("PostgreSQL")
  # con = dbConnect(pg, user="postgres", password="wildan123",
  #                 host="localhost", port=5432, dbname="dataset_trajectory")
  options(shiny.maxRequestSize=30*1024^1000) 
  #------------------------------------------------------------------------------------------ 
  # OUtput
  #------------------------------------------------------------------------------------------
  
  # output$summary <- renderPrint({
  #   input$dofold
    
  #   if(input$dofold == 0){
  #     hide(summary)
  #   }else{
  #     summary(Fold_k())
  #   }
  # })

  #------------------------------------------------------------------------------------------ 
  # CONVERT TO CSV
  #------------------------------------------------------------------------------------------
  converttocsv <- reactive({
    infile <- input$datafileimage
    x <- new("GDALReadOnlyDataset",infile$datapath)
    imagedata <- data.frame(getRasterTable(x))
    databaru <- imagedata[with(imagedata,order(-band1)),]
    hapusdata <- na.omit(databaru)
    names(hapusdata) <- c("x","y","band7", "band5", "band4")
    databaru <- hapusdata
    # rm(outputfile)
    # write.table(imagedata,file=outputfile,sep=",",row.names = F,col.names = T)
    if (input$Classname != '' && input$addClass != 0){
      databaru$Class <- input$Classname 
    }
    # rm(input$Classname,x,imagedata,databaru)
    output$download3 <- renderUI({
      if(!is.null(databaru)) {
        downloadButton('downloadData', 'Download')
      }
    })
    return(databaru);
  })

  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      filename1 <- "data"
      if (input$filename1 != '' && input$addName1 != 0){
        filename1 <- input$filename1 
      }
      paste(filename1, ".csv", sep = "")
    },
    content = function(file) {
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Generating Data', value = 0, {
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("Doing part", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
        data = converttocsv()
        write.table(data, file,row.names=FALSE, na="",col.names=TRUE, sep=",")
    }
  )
  
  

  ### DOWNLOAD BINDING DATA ###
  output$downloadDbind <- downloadHandler(
    filename = function() {
      filename1 <- "data"
      if (input$filename1 != '' && input$addName1 != 0){
        filename1 <- input$filename1 
      }
      paste(filename1, ".csv", sep = "")
    },
    content = function(file) {
      write.table(getData(), file, row.names=FALSE, na="",col.names=TRUE, sep=",")
    }
  )
  
  # This example uses the withProgress, which is a simple-to-use wrapper around
  # the progress API.
  output$mytable <- DT::renderDataTable({
    input$do # Re-run when button is clicked
    
    style <- isolate(input$style)
    
    
    input$do # Re-run when button is clicked
    
    if (input$do == 0){
      
    }else{
      
      withProgress(message = 'Creating plot', style = style, value = 0.1, {
        Sys.sleep(0.25)
        
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        # withProgress calls can be nested, in which case the nested text appears
        # below, and a second bar is shown.
        withProgress(message = 'Generating data', style = style, detail = "part 0", value = 0, {
          for (i in 1:10) {
            # Each time through the loop, add another row of data. This a stand-in
            # for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            
            # Increment the progress bar, and update the detail text.
            incProgress(0.1, detail = paste("part", i))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
        
        # Increment the top-level progress indicator
        incProgress(0.5)
        
        # Another nested progress indicator.
        # When value=NULL, progress text is displayed, but not a progress bar.
        withProgress(message = 'And this also', detail = "This other thing",
                     style = style, value = NULL, {
                       
                       Sys.sleep(0.75)
                     })
        
        # We could also increment the progress indicator like so:
        # incProgress(0.5)
        # but it's also possible to set the progress bar value directly to a
        # specific value:
        setProgress(1)
        
        isolate(datatable(converttocsv()[(1:10),])) 
      })
    }
  },
  extensions = c('Buttons'), 
  options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
  #------------------------------------------------------------------------------------------ 
  # INPUT
  #------------------------------------------------------------------------------------------
  # observeEvent(input$datacsv, {
  #   infile <- read.csv("D:/Kuliah_Ilkom/Dewi shofiana/G651170466/Data Penelitian/Sampel/Sample_Lahan.csv", header=TRUE)
  #   output$cumdata <- renderPrint({
  #     data
  #     })
  # })


  observeEvent(input$doclass, {
    classifica()
  })

  classifica <- function(){
    
    #CombinePredictedData
    inFile <- input$datacsv
    datacheck <- read.csv(inFile$datapath, header = TRUE)
    colnames(datacheck) <- c("x", "y", "band7", "band5", "band4")
    datacheck <<- datacheck[1:100000,]
    namepredict <- input$filename2
    labelname <- input$labelname
    myrule <- switch('input$k_rule', 'rulemodel1' = ruleModel1 ,'rulemodel2' = ruleModel2, 'rulemodel3' = ruleModel3, 'rulemodel4' = ruleModel4, 'rulemodel5' = ruleModel5, 'rulemodel6' = ruleModel6, 'rulemodel7' = ruleModel7, 'rulemodel8' = ruleModel8, 'rulemodel9' = ruleModel9, 'rulemodel10' = ruleModel10)
    predictdata <- predict(ruleModel1,datacheck)
    datacheck$Class <- predictdata
    citra_noband <- datacheck[,-(3:5)]
    citra_noband[,4] <- citra_noband[,3]
    colnames(citra_noband) <- c("x", "y", "band7", "Class")
    write.table(citra_noband, "D:/Downloads/Projek/citra_noband.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
    colorHutan = 234
    colorSawah = 216
    colorPerkebunan = 198
    colorLahanTerbuka = 180
    colorLahan = 162
    colorUrban = 144
    colorAwan = 126
    colorAir = 108
    colorKabutAsap = 90
    colorSebelumTerbakar = 72   
    colorTerbakar = 54
    colorSetelahTerbakar = 36
    colorSemakBelukar = 18
    colorUnclassified = 0

    #coloringband
    for (i in 1:length(input$variable)) {
      switch(input$variable[i], 
        'Hutan' = {
          mysubset = subset(citra_noband, Class=="Hutan")
          mysubset[,3] = colorHutan
         }, 
        'Sawah' = {
          mysubset = subset(citra_noband, Class=="Sawah")
          mysubset[,3] = colorSawah
        }, 
        'Perkebunan' = {
          mysubset = subset(citra_noband, Class=="Perkebunan")
          mysubset[,3] = colorPerkebunan
          }, 
        'LahanTerbuka' = {
          mysubset = subset(citra_noband, Class=="LahanTerbuka")
          mysubset[,3] = colorLahanTerbuka
          }, 
        'Lahan' = {
          mysubset = subset(citra_noband, Class=="Lahan")
          mysubset[,3] = colorLahan
          }, 
        'Urban' = {
          mysubset = subset(citra_noband, Class=="Urban")
          mysubset[,3] = colorUrban
          }, 
        'Awan' = {
          mysubset = subset(citra_noband, Class=="Awan")
          mysubset[,3] = colorAwan
          }, 
        'Air' = {
          mysubset = subset(citra_noband, Class=="Air")
          mysubset[,3] = colorAir
          }, 
        'KabutAsap' = {
          mysubset = subset(citra_noband, Class=="KabutAsap")
          mysubset[,3] = colorKabutAsap
          },
        'SebelumTerbakar' = {
          mysubset = subset(citra_noband, Class=="SebelumTerbakar")
          mysubset[,3] = colorSebelumTerbakar
          }, 
        'Terbakar' = {
          mysubset = subset(citra_noband, Class=="Terbakar")
          mysubset[,3] = colorTerbakar
          }, 
        'SetelahTerbakar' = {
          mysubset = subset(citra_noband, Class=="SetelahTerbakar")
          mysubset[,3] = colorSetelahTerbakar
          }, 
        'SemakBelukar' = {
          mysubset = subset(citra_noband, Class=="SemakBelukarTegalan")
          mysubset[,3] = colorSemakBelukar
          },
        'Unclassified' = {
          mysubset = subset(citra_noband, Class=="Unclassified")
          mysubset[,3] = colorUnclassified
          })
      if(!exists('dataclass')){
        dataclass <- mysubset
      }else{
        dataclass <- rbind(dataclass,mysubset)  
      }
    }

    daio$dList <- dataclass
    return(dataclass)
  }


  output$downloadNoband <- downloadHandler(
    filename = function() {
      filename3 <- "data"
      if (input$filename3 != '' && input$addName3 != 0){
        filename3 <- input$filename3 
      }
      paste(filename3, ".csv", sep = "")
    },
    content = function(file) {
      write.table(classifica(), file, row.names=FALSE, na="",col.names=TRUE, sep=",")
    }
  )
  
  daio <- reactiveValues()

  output$labelname <- renderPrint({
    input$doclass
    summary(daio$dList)
  })
  
  getData <- reactive({
    inFile <- input$databindcsv
    if (is.null(inFile)){

    }else {   
      files3 <- lapply(inFile$datapath, function(y){
        JSON_csv = read.csv(y, header = TRUE)
      })
      do.call(rbind, files3)
      # output$download3 <- renderUI({
      #   if(!is.null(files3)) {
      #     downloadButton('downloadData', 'Download')
      #   }
      # })
    }
  })
  
  #------------------------------------------------------------------------------------------ 
  # CREATE TREE
  #------------------------------------------------------------------------------------------
  observeEvent(input$dofold, {
    grow()
  })


  grow <- function(){
     
    inFilexyz <- input$datasamplecsv
    # dataxyz <- read.csv(inFilexyz$datapath, header = TRUE)

    # files4 <- lapply(inFile$datapath, function(y){
    #     JSON_csv = read.csv(y, header = TRUE)
    # })

    # data <- files4
    # dataxyz <- read.csv("D:/Kuliah_Ilkom/Dewi shofiana/G651170466/Data Penelitian/Sampel/Sample_Lahan.csv", header=TRUE)
    dataxyz <- read.csv(inFilexyz$datapath, header = TRUE)
    
    dataxyz$Class <- as.factor(dataxyz$Class)
    dataxyz <- dataxyz[,3:6]
    #   #Classification for 10 fold

      # k-cross validation (10)
      fold_data<-createFolds(dataxyz$Class, k = 10, list = TRUE, returnTrain = FALSE)

      test1 <- dataxyz[fold_data$Fold01, ]
      train1 <- dataxyz[-fold_data$Fold01, ]

      #tree
      treeModel1 <<- C5.0 (x = train1[,-4], y = train1$Class)
      # #rule
      ruleModel1 <<- C5.0(Class ~., data = train1, rules = TRUE)
      predict1 <- predict(ruleModel1, test1)
      confusionMatrix(test1$Class,predict1)
      sum1 <- postResample(predict(ruleModel1, test1), test1$Class)


      # #rule2
      # test2 <- data[fold_data$Fold02, ]
      # train2 <- data[-fold_data$Fold02, ]

      # #tree
      # treeModel2 <- C5.0 (x = train2[,-4], y = train2$Class)
      # # #rule
      # ruleModel2 <<- C5.0(Class ~., data = train2, rules = TRUE)
      # predict2 <- predict(ruleModel2, test2)
      # confusionMatrix(test2$Class,predict2)
      # sum2 <- postResample(predict(ruleModel2, test2), test2$Class)
      
      # str(sum2)

      # #rule3
      # test3 <- data[fold_data$Fold03, ]
      # train3 <- data[-fold_data$Fold03, ]

      # #tree
      # treeModel3 <- C5.0 (x = train3[,-4], y = train3$Class)
      # # #rule
      # ruleModel3 <<- C5.0(Class ~., data = train3, rules = TRUE)
      # predict3 <- predict(ruleModel3, test3)
      # confusionMatrix(test3$Class,predict3)
      # sum3 <- postResample(predict(ruleModel3, test3), test3$Class)
      
      # str(sum3)

      # #rule4
      # test4 <- data[fold_data$Fold04, ]
      # train4 <- data[-fold_data$Fold04, ]

      # #tree
      # treeModel4 <- C5.0 (x = train4[,-4], y = train4$Class)
      # # #rule
      # ruleModel4 <<- C5.0(Class ~., data = train4, rules = TRUE)
      # predict4 <- predict(ruleModel4, test4)
      # confusionMatrix(test4$Class,predict4)
      # sum4 <- postResample(predict(ruleModel4, test4), test4$Class)
      
      # str(sum4)

      # #rule5
      # test5 <- data[fold_data$Fold05, ]
      # train5 <- data[-fold_data$Fold05, ]

      # #tree
      # treeModel5 <- C5.0 (x = train5[,-4], y = train5$Class)
      # # #rule
      # ruleModel5 <<- C5.0(Class ~., data = train5, rules = TRUE)
      # predict5 <- predict(ruleModel5, test5)
      # confusionMatrix(test5$Class,predict5)
      # sum5 <- postResample(predict(ruleModel5, test5), test5$Class)
      
      # str(sum5)

      # #rule6
      # test6 <- data[fold_data$Fold06, ]
      # train6 <- data[-fold_data$Fold06, ]

      # #tree
      # treeModel6 <- C5.0 (x = train6[,-4], y = train6$Class)
      # # #rule
      # ruleModel6 <<- C5.0(Class ~., data = train6, rules = TRUE)
      # predict6 <- predict(ruleModel6, test6)
      # confusionMatrix(test6$Class,predict6)
      # sum6 <- postResample(predict(ruleModel6, test6), test6$Class)
      
      # str(sum6)

      # #rule7
      # test7 <- data[fold_data$Fold07, ]
      # train7 <- data[-fold_data$Fold07, ]

      # #tree
      # treeModel7 <- C5.0 (x = train7[,-4], y = train7$Class)
      # # #rule
      # ruleModel7 <<- C5.0(Class ~., data = train7, rules = TRUE)
      # predict7 <- predict(ruleModel7, test7)
      # confusionMatrix(test7$Class,predict7)
      # sum7 <- postResample(predict(ruleModel7, test7), test7$Class)
      
      # str(sum7)

      # #rule8
      # test8 <- data[fold_data$Fold08, ]
      # train8 <- data[-fold_data$Fold08, ]

      # #tree
      # treeModel8 <- C5.0 (x = train8[,-4], y = train8$Class)
      # # #rule
      # ruleModel8 <<- C5.0(Class ~., data = train8, rules = TRUE)
      # predict8 <- predict(ruleModel8, test8)
      # confusionMatrix(test8$Class,predict8)
      # sum8 <- postResample(predict(ruleModel8, test8), test8$Class)
      
      # str(sum8)

      # #rule9
      # test9 <- data[fold_data$Fold09, ]
      # train9 <- data[-fold_data$Fold09, ]

      # #tree
      # treeModel9 <- C5.0 (x = train9[,-4], y = train9$Class)
      # # #rule
      # ruleModel9 <<- C5.0(Class ~., data = train9, rules = TRUE)
      # predict9 <- predict(ruleModel9, test9)
      # confusionMatrix(test9$Class,predict9)
      # sum9 <- postResample(predict(ruleModel9, test9), test9$Class)
      
      # str(sum9)

      # #rule10
      # test10 <- data[fold_data$Fold10, ]
      # train10 <- data[-fold_data$Fold10, ]

      # #tree
      # treeModel10 <- C5.0 (x = train10[,-4], y = train10$Class)
      # # #rule
      # ruleModel10 <<- C5.0(Class ~., data = train10, rules = TRUE)
      # predict10 <- predict(ruleModel10, test10)
      # confusionMatrix(test10$Class,predict10)
      # sum10 <- postResample(predict(ruleModel10, test10), test10$Class)
      
      # str(sum10)

      # press <- predict1
      
  }
  

  output$growpl1 <- renderPrint({
    input$dofold
    if(exists("ruleModel1")){
        summary(ruleModel1)
      }else{
        return(NULL)
      }
      
  })

  # output$growpl2 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel2)
  # })

  # output$growpl3 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel3)
  # })

  # output$growpl4 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel4)
  # })

  # output$growpl5 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel5)
  # })

  # output$growpl6 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel6)
  # })

  # output$growpl7 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel7)
  # })

  # output$growpl8 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel8)
  # })

  # output$growpl9 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel9)
  # })

  # output$growpl10 <- renderPrint({
  #   input$dofold
  #   summary(ruleModel10)
  # })

  #------------------------------------------------------------------------------------------ 
  # CONVERT TO RASTER
  #------------------------------------------------------------------------------------------
  observeEvent(input$doraster, {
    converttoraster()
  })



  converttoraster <- function(){
    inFile3 <- input$databandcsv
    data4 <- read.csv(inFile3$datapath, header = TRUE)

    # data4 <- data4[1:1000000,]
    #konversi ke spatial points
    coordinates(data4) = ~x+y
    proj4string(data4)=CRS("+init=epsg:32647")
    #pixels2 <- SpatialPixelsDataFrame(data4, tolerance = 0.916421, data4@data)
    pixels2 <- SpatialPixelsDataFrame(data4,data4@data)
    #gridify sp
    gridded(pixels2) <- TRUE
    #konversi ke raster
    r2014 <<- raster(pixels2)
    projection(r2014) = CRS("+init=epsg:32647")
    r2014
  }

  if(!exists('r2014')){
    hide('plotimage1')
  }
  

  output$plotimage1 <- renderPlot({
    input$doraster
    plot(converttoraster(),legend = FALSE)
    legend("topright", legend = c("Absence", "Presence"))
  })

  output$downloadRaster <- downloadHandler(
    
    filename = function() {
      paste('raster', ".tif", sep = "")
    },
    content = function(file) {
      writeRaster(r2014,file)
    }
  )
})