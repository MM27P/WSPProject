library(shiny)
library(DT)
library(d3heatmap)


useShinyjs()


selectionPanel1<- panel (headerPanel("Nie wczytano Esetu"))
selectionPanel2<-  panel(
                          headerPanel("Selekcja"), 
                                             selectInput(
                                               "method", "Metoda:",
                                               c(
                                                 "holm" = "holm",
                                                 "hochberg" = "hochberg",
                                                 "hommel" = "hommel",
                                                 "bonferroni"="bonferroni",
                                                 "BH"="BH",
                                                 "BY"="BY",
                                                 "fdr"="fdr",
                                                 "none"="none"
                                               )
                                             ),
                                             uiOutput('selectClas1'),
                                             uiOutput('selectClas2'),
                                             selectInput(
                                               "criterion", "Kryterium sortowania:",
                                               c(
                                                 "fold change" = "FoldChange",
                                                 "p value"="p_val",
                                                 "P value po korekcji_FDR"="p_val_adjusted"
                                               )
                                             ),
                                             radioButtons(
                                               "chooseMode", "Filtracja:",
                                               c(
                                                 "Zakres" = "number",
                                                 "Granica" = "treshold"
                                               )
                                             ),
                                             conditionalPanel(
                                               id = 'ModeConditionalPanel',condition="input.chooseMode=='number' || input.chooseMode=='treshold'",
                                               numericInput("obs", "Wartosc", 10, min = 1, max = 100),
                                               verbatimTextOutput("value")
                                             ),
                                             actionButton("buttonSelection", "Selekcja")
                                  )
pathPanel1<- panel(headerPanel("Nie wczytano p wartości"))
pathPanel2<-panel(
                      checkboxGroupInput("variable", "Geny:",
                                         c(
                                           "H","C1","C2","CGP","CP","CP:BIOCARTA","CP:KEGG","CP:REACTOME", 
                                           "C3","MIR","TFT","C4","CGN","CM","C5","BP","CC","MF","c6","C7"
                                         )
                      ),
                      selectInput(
                        "method2", "Metoda:",
                        c(
                          "Set testowy" = "geneSetTest",
                          "Camera" = "CAMERA"
                        )
                      ),
                      selectInput(
                        "FDR_Correction", "FDR Correction:",
                        c(
                          "holm" = "holm",
                          "hochberg" = "hochberg",
                          "hommel" = "hommel",
                          "bonferroni"="bonferroni",
                          "BH"="BH",
                          "BY"="BY",
                          "fdr"="fdr",
                          "none"="none"
                        )
                      ),
                      actionButton("buttonPath", "Analiza")
                    )
specialPanelSelect<-panel(
                            headerPanel("Opcje"), 
                            actionButton("buttonSelectionHeatmap", "Generuj Heatmapa"),
                            shinySaveButton("saveExcelSelection", "Zapisz", "Save file as ...", filetype=list(xlsx="xlsx"))
                          )

specialPanelPath<-panel(
                          headerPanel("Opcje"), 
                          actionButton("buttonPathHeatmap", "Generuj Heatmapa"),
                          textInput( "pathTextBox", "Nazwa ścieżki:", value = "", width = NULL,
                                    placeholder = NULL),
                          shinySaveButton("saveExcelPath", "Zapisz", "Save file as ...", filetype=list(xlsx="xlsx"))
                        )


ui <- fluidPage(

  useShinyalert(),
  shinyjs::useShinyjs(),
  
  title = "Examples of DataTables",
  sidebarLayout(
      sidebarPanel(
          conditionalPanel(
                           id = 'ExpresSetConditionalPanel',condition="input.conditionPanel==1",
                           panel(
                                   headerPanel("Wczytywanie"), 
                                   fileInput(
                                              "file1", "Wybierz plik ze zbiorem",
                                               accept = c(".RDS")
                                   ),
                                   uiOutput('file2'),
                                   uiOutput('buttonTag'),
                                   #to do wywalenia potem, testy
                                   h6('kitku'),
                                   tags$img(src='kitku.png', height=50, width=50)
                                )
                         ),
          conditionalPanel(
                            id = 'GenConditionalPanel',condition="input.conditionPanel==2",
                            uiOutput('selectPanel'),
                            uiOutput('specialPanelSelect')
                         ),
       conditionalPanel(
                          id = 'ExcelConditionalPanel',condition="input.conditionPanel==3",
                          panel(
                                  headerPanel("Wczytanie z pliku"), 
                                  fileInput(
                                    "loadPValue", "Wybierz plik z t wartosciami",
                                    accept = c(".xlsx")
                                  )
                           ),
                          uiOutput('chooseSource2'),
                          uiOutput('pathPanel'),
                          uiOutput('specialPanelPath')

                        )
      ),
      
      mainPanel(
          tabsetPanel(
                         id = 'conditionPanel',
                         tabPanel("Annotacja danych",value=1, DT::dataTableOutput("exprSetTable")),
                         tabPanel("Selekcja cech roznicujacych",value=2,   d3heatmapOutput("heatmap", width = "100%", height="600px")),
                         tabPanel("Analiza sciezek sygnalowych",value=3, d3heatmapOutput("heatmap1", width = "100%", height="600px"))
                         
                         
                      )
                ),
      
      )
  )

