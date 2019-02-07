library("shinythemes") #v
library("leaflet") #v
library("markdown") #v
library(shiny)
library(shinyjs)
library("shinyBS")
library("DT")
library("shinyjqui")

shinyUI(
  div(fluidPage(theme = shinytheme("lumen")),
      headerPanel(h1("Aplikasi Klasifikasi Data Citra", align = "center"),
                  windowTitle = "Aplikasi klasifikasi data citra"),
      tags$style(),tags$head(tags$script(src = "message_handler.js"),tags$script(src = "my_script.js")),
      mainPanel( style ="width:100%",
                 tabsetPanel(type = "pills",
                             tabPanel("Extraction", hr(),
                                      tabsetPanel(type = "tab",
                                                  tabPanel('Convert Image to CSV file',sidebarLayout(fluid = TRUE,
                                                           sidebarPanel(h4('Upload Image Data'),
                                                                        fileInput('datafileimage', 'Upload the file',accept = c('image/tif', 'image/TIF')),
                                                                        checkboxInput("addName1", "Insert Filename", FALSE),
                                                                        div(id = "divname1", class = "hidden", textInput("filename1", "Filename")),
                                                                        checkboxInput("addClass", "Add Class", FALSE),
                                                                        div(id = "divclass", class = "hidden", textInput("Classname", "Class Name")),
                                                                        actionButton("do", "Convert"),width = 3),
                                                           
                                                           mainPanel(fluid = TRUE,
                                                                     h3(textOutput("cverr")),
                                                                     DT::dataTableOutput("mytable"),
                                                                     uiOutput("download3")
                                                           )
                                                  )),
                                                  tabPanel('Binding CSV file',sidebarLayout(fluid = TRUE,
                                                            sidebarPanel(h4('Upload Multiple CSV file'),
                                                                         fileInput('databindcsv', "Choose CSV files from a directory",
                                                                                   multiple = TRUE,
                                                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                                                                        checkboxInput("addName2", "Insert Filename", FALSE),
                                                                        div(id = "divname2", class = "hidden", textInput("filename2", "Filename")),
                                                                        actionButton("dobind", "Go Binding"),width = 3),
                                                            
                                                            mainPanel(fluid = TRUE,
                                                                      h5('Download binding results'),
                                                                      tableOutput('contents'),
                                                                      downloadButton("downloadDbind", "Download")
                                                            )
                                                  ))
                                      )
                             ),
                             tabPanel("Classification", hr(),
                                      tabsetPanel(type = "tab",
                                                  tabPanel('Create Tree',sidebarLayout(fluid = TRUE,
                                                           sidebarPanel(h4('Upload CSV file'),
                                                                        fileInput('datasamplecsv', "Choose CSV files for sample",
                                                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                                                                        actionButton("dofold", "Create Model"),width = 3),
                                                           
                                                           mainPanel(fluid = TRUE,
                                                                     h3('Description Rule Results'),
                                                                     # Output: Verbatim text for data summary ----
                                                                     h5('Rule 1'),
                                                                     verbatimTextOutput("growpl1"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 2'),
                                                                     verbatimTextOutput("growpl2"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 3'),
                                                                     verbatimTextOutput("growpl3"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 4'),
                                                                     verbatimTextOutput("growpl4"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 5'),
                                                                     verbatimTextOutput("growpl5"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 6'),
                                                                     verbatimTextOutput("growpl6"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 7'),
                                                                     verbatimTextOutput("growpl7"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 8'),
                                                                     verbatimTextOutput("growpl8"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 9'),
                                                                     verbatimTextOutput("growpl9"),
                                                                     tags$hr(style="border-color: light-gray;"),
                                                                     h5('Rule 10'),
                                                                     verbatimTextOutput("growpl10")
                                                           )
                                                  )),
                                                  tabPanel('Classification C5.0',sidebarLayout(fluid = TRUE,
                                                           sidebarPanel(h4('Upload CSV file'),
                                                                        fileInput('datacsv', "Choose CSV files for train data",
                                                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                                                                        checkboxInput("addName3", "Insert Filename", FALSE),
                                                                        div(id = "divname3", class = "hidden", textInput("filename3", "Filename")),
                                                                        selectInput('k_rule', 'Choose Rule', 
                                                                                    choices = c('1'= 'rulemodel1','2'= 'rulemodel2','3'= 'rulemodel3','4'= 'rulemodel4','5'= 'rulemodel5','6'= 'rulemodel6','7'= 'rulemodel7','8'= 'rulemodel8','9'= 'rulemodel9', '10'= 'rulemodel10'), selected = ""),
                                                                        checkboxGroupInput("variable", "Variables to show:",
                                                                          c("Hutan" = "Hutan",
                                                                            "Sawah" = "Sawah",
                                                                            "Perkebunan" = "Perkebunan",
                                                                            "Lahan Terbuka" = "LahanTerbuka",
                                                                            "Urban" = "Urban",
                                                                            "Awan" = "Awan",
                                                                            "Air" = "Air",
                                                                            "Kabut Asap" = "KabutAsap",
                                                                            "Sebelum Terbakar" = "SebelumTerbakar",
                                                                            "Terbakar" = "Terbakar",
                                                                            "Setelah Terbakar" = "SetelahTerbakar",
                                                                            "Semak Belukar" = "SemakBelukar",
                                                                            "Unclassified" = "Unclassified")),
                                                                        tags$hr(),
                                                                        actionButton("doclass", "Create Classification"),width = 3),
                                                           
                                                                        mainPanel(fluid = TRUE,
                                                                                 h5('Description and summary Classification data'),
                                                                                 verbatimTextOutput("cumdata"),
                                                                                 verbatimTextOutput("labelname"),
                                                                                 downloadButton("downloadNoband", "Download")
                                                           )
                                                  )
                                            ),
                                            tabPanel('Create Raster',sidebarLayout(fluid = TRUE,
                                                     sidebarPanel(h4('Upload CSV file'),
                                                                  fileInput('databandcsv', "Choose CSV file",
                                                                            accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                                                                  actionButton("doraster", "Create Raster"),width = 3),
                                                     
                                                     mainPanel(fluid = TRUE,
                                                              h5('Clik button below to download result'),
                                                              # Output: Verbatim text for data summary ----
                                                              downloadButton("downloadRaster", "Download"),
                                                              plotOutput("plotimage1", click = "plot_click")
                                                     )
                                            ))
                                      )
                             )
                 )
      )
  )
)