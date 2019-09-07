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
                                   uiOutput('buttonTag')
                                )
                         ),
          conditionalPanel(
                            id = 'GenConditionalPanel',condition="input.conditionPanel==2",
                            conditionalPanel(
                                                id = 'SourceConditionalPanel1',condition="input.conditionPanel==2",
                                                headerPanel("Nie wczytano ExprSet")
                                            ),
                            conditionalPanel(id = 'SourceConditionalPanel2',condition="input.conditionPanel==2",
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
                                  ),
                                 panel(
                                        actionButton("buttonSelectionHeatmap", "Generuj Heatmapa"),
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
                                                                      "loadPValue", "Wybierz plik z p wartosciami",
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
                                  actionButton("buttonPathHeatmap", "Generuj Heatmapa"),
                                  shinySaveButton("saveExcelPath", "Save file", "Save file as ...", filetype=list(xlsx="xlsx"))
                                )
                        )
      ),
      
      mainPanel(
          tabsetPanel(
                         id = 'conditionPanel',
                         tabPanel("Annotacja danych",value=1, DT::dataTableOutput("exprSetTable")),
                         tabPanel("Selekcja cech roznicujacych",value=2,  plotOutput("heatmap")),
                         tabPanel("Analiza sciezek sygnalowych",value=3),
                         tabPanel("Heatmaps",value=4)
                         
                         
                      )
                ),
      
      )
  )

