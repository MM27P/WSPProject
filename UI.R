library(shiny)
library(DT)
s = c("set1", "set2", "set3", "set4", "set5") 
ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      panel("Content goes here",
        checkboxGroupInput("show_vars", "Loaded set",
                           names(diamonds), selected = names(diamonds))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Tabela genów", DT::dataTableOutput("mytable1")),
        tabPanel("Wczytywanie plików",fileInput("file1", "Wybierz pli CSV",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")), actionButton("button", "Dodaj zbiór")),
        tabPanel("Wykresy", selectInput("Ind","Indipendent Variable",choices = names(mtcars)),
                 selectInput('Dep','  Dependent Variable',choices = names(mtcars)),
                 plotOutput("BoxPlot"),
                 plotOutput('Hist'))
      )
    )
  )
)
