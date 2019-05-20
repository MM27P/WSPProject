library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    sidebarPanel(
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Geny", plotOutput("Gens") , tableOutput('table')),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
  )
  )
)
