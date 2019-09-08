library(shiny)
library(DT)
library(d3heatmap)


useShinyjs()


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
                                   actionButton("buttonAdd", "Dodaj"),
                                   actionButton("buttonDelete", "Usuń"),
                                   uiOutput('Test')
                                )
                         ),
          conditionalPanel(
                            id = 'GenConditionalPanel',condition="input.conditionPanel==2",
                            uiOutput('chooseSource'),
                            conditionalPanel(
                                                id = 'SourceConditionalPanel1',condition="input.chooseSource=='File'",
                                                panel(
                                                        headerPanel("Wczytanie pliku"),
                                                        fileInput(
                                                                    "fileGen", "Wybierz plik z genami",
                                                                     accept = NULL
                                                                  )
                                                    )
                                            ),
                            conditionalPanel(id = 'SourceConditionalPanel2',condition="input.chooseSource=='Eset'"),
                            panel(
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
                                                    "fold_change" = "0",
                                                    "p_value"="1",
                                                    "P_value_po_korekcji_FDR"="2"
                                                   )
                                              ),
                                   radioButtons(
                                                "chooseMode", "Filtracja:",
                                                c(
                                                     "Nic" = "none",
                                                     "Zakres" = "number",
                                                     "Granica" = "treshold"
                                                  )
                                               ),
                                  conditionalPanel(
                                                    id = 'ModeConditionalPanel',condition="input.chooseMode=='number' || input.chooseMode=='treshold'",
                                                    numericInput("obs", "Wartość", 10, min = 1, max = 100),
                                                    verbatimTextOutput("value")
                                                   ),
                                  actionButton("buttonSelection", "Selekcja")
                                 ),
                                 panel(
                                        actionButton("buttonSelectionHeatmap", "Generuj Heatmapę"),
                                        shinySaveButton("saveExcelSelection", "Save file", "Save file as ...", filetype=list(xlsx="xlsx"))
                                        
                                 )
                         ),
       conditionalPanel(
                          id = 'ExcelConditionalPanel',condition="input.conditionPanel==3",
                          panel(
                                  uiOutput('chooseSource2'),
                                  conditionalPanel(
                                                    id = 'LoadPValueFile',condition="input.chooseSource2=='File'",
                                                    panel(
                                                          headerPanel("Wczytanie z pliku"), 
                                                          fileInput(
                                                                      "loadPValue", "Wybierz plik z p wartościami",
                                                                      accept = NULL
                                                                    )
                                                    )
                                  ),
                                  checkboxGroupInput("variable", "Geny:",
                                                     c(
                                                         "H","C1","C2","CGP","CP","CP:BIOCARTA","CP:KEGG","CP:REACTOME", 
                                                         "C3","MIR","TFT","C4","CGN","CM","C5","BP","CC","MF","c6","C7"
                                                       )
                                  ),
                                  actionButton("buttonPath", "Analiza")
                               ),
                          panel(
                                  actionButton("buttonPathHeatmap", "Generuj Heatmapę"),
                                  shinySaveButton("saveExcelPath", "Save file", "Save file as ...", filetype=list(xlsx="xlsx"))
                                )
                        ),
       conditionalPanel(
         id = 'HeatmapConditionalPanel',condition="input.conditionPanel==4",
         panel(
           d3heatmapOutput("heatmap", width = "100%", height="500px")
         )
         
       )
      ),
      
      mainPanel(
          tabsetPanel(
                         id = 'conditionPanel',
                         tabPanel("Annotacja danych",value=1, DT::dataTableOutput("exprSetTable")),
                         tabPanel("Selekcja cech różnicujących",value=2, DT::dataTableOutput("mytable1")),
                         tabPanel("Analiza ścieżek sygnałowych",value=3),
                         tabPanel("Heatmaps",value=4)
                         
                         
                      )
                ),
      
      )
  )

