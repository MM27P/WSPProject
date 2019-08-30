source("API.R")

n1 <- 100
n2 <- 100
nr <- 30
nc <- 30
set.seed(1)
x <- matrix(rnorm(n1), nrow=nr, ncol=nc)
y <- matrix(rnorm(n2), nrow=nr, ncol=nc)
MAT <- cor(x,y)

T1 <- data.frame(Charge = c("Environmental", "Base Power Cost",
                            "Base Adjustment Cost", "Distribution Adder",
                            "Retail Rate Without Fuel", "Fuel Charge Adjustment",
                            "Retail Rate With Fuel"),
                 Summer = c(0.00303, 0.06018, 0.00492, 0.00501, 0.07314,
                            0.02252, 0.09566),
                 Winter = c(0.00303, 0.05707, 0.00468, 0.01264, 0.07742, 
                            0.02252, 0.09994),
                 Transition = c(0.00303, 0.05585, 0.00459, 0.01264,
                                0.07611, 0.02252, 0.09863),
                 stringsAsFactors = FALSE)


server <- function(input, output,session) {
  #output$heatmap <- renderD3heatmap({d3heatmap(MAT)})
  
  
  output$table1 <-
    renderUI({
      dust(T1) %>% 
        sprinkle(rows = 1, 
                 border = "bottom", 
                 part = "head") %>% 
        sprinkle(rows = c(5, 7),
                 cols = 2:4,
                 border = "top") %>% 
        sprinkle(rows = c(5, 7),
                 bold = TRUE) %>% 
        sprinkle(pad = 4) %>% 
        sprinkle_colnames(Charge = "") %>% 
        print(asis = FALSE) %>% 
        HTML()
    })

  
  #LOAD EXPRES SET
  observe({
    file1 = input$file1
    if(is.null(input$file1)) {
      return(NULL)
    }
    else
      showNotification("Wczytano plik zbioru")
      output$file2<-renderUI({fileInput("file2", "Wybierz plik z adnotacjami",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
      )})
  })
  
  
  #LOAD ADNOTATIONS
  observe({
    file1 = input$file1
    file2 = input$file2
    
    
    if (is.null(file1) && !(is.null(file2))) {
      shinyalert("Error!", "Nie wczytano pliku zbioru", type = "error")
      output$file2=NULL
      return(NULL)
    }
    else if(is.null(file2)){
      return(NULL)
    }
    opis<<-description(input$file2$datapath)
    
    output$selectClas1<-renderUI({selectInput("selectClas1", "Klasa 1:",
                                              opis@data$CLASS
    )})
    output$selectClas2<-renderUI({selectInput("selectClas2", "Klasa 2:",
                                              opis@data$CLASS
    )})
    
    showNotification("Wczytano adnotacje")
    output$buttonTag<-renderUI({actionButton("buttonTag", "Konwersja oznaczeń")})
    
    
    #HitMap
     hitMap<- GenerateHitMap (exprSet)
    #output$heatmap <- renderD3heatmap({d3heatmap(MAT)})
     output$heatmap <- renderD3heatmap({d3heatmap( hitMap)})
     showNotification("Wygenerowano Hitmapę")
  })
  
  #SAVE EXCEL
  observe({volumes <- c("UserFolder"=getwd())
  shinyFileSave(input, "save", roots=volumes, session=session)
  fileinfo <- parseSavePath(volumes, input$save)
 
   if (nrow(fileinfo) > 0) {
    #diffGenes<- Rest(exprSet,"ADENO","SQUAMOUS")
    SaveExcel(diffGenes,fileInfo)
    showNotification("Zapisano zbiór od excela")
    }
  })
  
  #ButtonTag

  observeEvent(input$buttonTag, {
    esetPath =  input$file1$datapath
    adnotationPath = input$file2$datapath
    exprSet<<-eSetAnnotation(esetPath, adnotationPath )
    if(!is.null(exprSet))
    {
      #output$exprSetTable <- renderDataTable(Transform_Exp2DataFrame(exprSet))
    }
    
  })
  
  observeEvent(input$selectClas1, {
    updateSelectInput(session, "selectClas2",
                      choices =  (opis@data$CLASS[opis@data$CLASS!=input$selectClas1]),
    
  )
})
  

    
}