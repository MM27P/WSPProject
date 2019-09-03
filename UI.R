BiocManager::install('shiny')
library(shiny)
BiocManager::install('DT')
library(DT)

ExpresSetConditionalPanel=conditionalPanel(id = 'ExpresSetConditionalPanel',condition="input.conditionPanel==1",
                                            
                                           shinyFilesButton("file1", "Wybierz plik zbioru" ,
                                                            title = "Please select a file:", multiple = FALSE,
                                                            buttonType = "default", class = NULL),
                                            uiOutput('file2'),
                                            uiOutput('buttonTag') 
                                            
                                            )

GenConditionalPanel=conditionalPanel(id = 'GenConditionalPanel',condition="input.conditionPanel==1",
                                            
                                            fileInput("file1", "Wybierz plik z genami",
                                                      accept = NULL
                                                     ),
                                            actionButton("button", "Run gen"),
                                            selectInput("variable", "Variable:",
                                                  c("holm" = "holm",
                                                    "hochberg" = "hochberg",
                                                    "hommel" = "hommel",
                                                    "bonferroni"="bonferroni",
                                                    "BH"="BH",
                                                    "BY"="BY",
                                                    "fdr"="fdr",
                                                    "none"="none"
                                                   ))
                                            
                                      )




s = c("set1", "set2", "set3", "set4", "set5") 
LoadPanel =panel(fileInput("file3", "Wybierz pli CSV",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")), 
                 actionButton("button", "Dodaj zbiór"),
                 textInput("textBoxInput", "Nazwa", value = "", width = NULL,
                           placeholder = NULL))



ui <- fluidPage(
  useShinyalert(),
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(id = 'ExpresSetConditionalPanel',condition="input.conditionPanel==1",
                       panel(
                         headerPanel("Wczytywanie"), 
                         fileInput("file1", "Wybierz plik ze zbiorem",
                                                           accept = c('.RDS')
                         ),
                         uiOutput('file2'),
                         uiOutput('buttonTag')
                       )
                       ),
      conditionalPanel(id = 'GenConditionalPanel',condition="input.conditionPanel==2",
                        fileInput("fileGen", "Wybierz plik z genami",
                        accept = NULL
                        ),
                        panel(
                               headerPanel("Selekcja"), 
                               selectInput("method", "Metoda:",
                                  c("holm" = "holm",
                                  "hochberg" = "hochberg",
                                  "hommel" = "hommel",
                                  "bonferroni"="bonferroni",
                                  "BH"="BH",
                                  "BY"="BY",
                                  "fdr"="fdr",
                                  "none"="none"
                                  )
                               ),
                              selectInput("clas1", "Klasa 1:",
                                          c("holm" = "holm",
                                            "hochberg" = "hochberg",
                                            "hommel" = "hommel",
                                            "bonferroni"="bonferroni",
                                            "BH"="BH",
                                            "BY"="BY",
                                            "fdr"="fdr",
                                            "none"="none"
                                          )
                              ),
                              selectInput("vclas2", "Klasa 2:",
                                          c("holm" = "holm",
                                            "hochberg" = "hochberg",
                                            "hommel" = "hommel",
                                            "bonferroni"="bonferroni",
                                            "BH"="BH",
                                            "BY"="BY",
                                            "fdr"="fdr",
                                            "none"="none"
                                          )
                              ),
                              selectInput("criterion", "Kryterium sortowania:",
                                          c("holm" = "holm",
                                            "hochberg" = "hochberg",
                                            "hommel" = "hommel",
                                            "bonferroni"="bonferroni",
                                            "BH"="BH",
                                            "BY"="BY",
                                            "fdr"="fdr",
                                            "none"="none"
                                          )
                              )
                        ),
                       checkboxGroupInput("variable", "Geny:",
                                          c("H","C1","C2","CGP","CP","CP:BIOCARTA","CP:KEGG","CP:REACTOME", 
                                     "C3","MIR","TFT","C4","CGN","CM","C5","BP","CC","MF","c6","C7")),
                       actionButton("buttonGen", "Run gen")
                       
                       
                       ),
     conditionalPanel(id = 'ExcelConditionalPanel',condition="input.conditionPanel==5",
                      panel(
                        headerPanel("Export"), 
                        shinySaveButton("save", "Zapisz", "Save file as ...", filetype=list(xlsx="xlsx")),
                        checkboxInput("somevalue", "Link?", FALSE)
                      )
                      )
      
    ),
    mainPanel(
      tabsetPanel(
        id = 'conditionPanel',
        tabPanel("Expresion Set",value=1, DT::dataTableOutput("exprSetTable")),
        tabPanel("Tabela genów",value=2, DT::dataTableOutput("mytable1")),
        tabPanel("Wczytywanie plików",value=3),
        tabPanel("Hitmapa",value=4,d3heatmapOutput("heatmap", width = "100%", height="600px")),
        tabPanel("Excel",value=5, uiOutput("table1"))
      )
    )
  )
)
