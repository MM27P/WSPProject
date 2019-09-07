source("API.R")

buttonAddded=FALSE

server <- function(input, output,session) {

    #Start Render optional hide/seek elements
    output$chooseSource<-renderUI({
                                   radioButtons(
                                                  "chooseSource", "P wartość",
                                                  c("Wczytaj z pliku" = "File")
                                                )
    })
    output$chooseSource2<-renderUI({
                                    radioButtons(
                                                  "chooseSource2", "P wartość",
                                                  c("Wczytaj z pliku" = "File")
                                                )
                                  })
    #End
   
  
    observeEvent(input$buttonAdd, {
      
      if(!exists("added") || is.null(added))
      {
          insertUI(
                    selector = "#buttonAdd",
                    where = "afterEnd",
                    ui = textInput("txt","Insert some text")
                   )
          added=TRUE
      }
    })
    
    
    observeEvent(input$buttonDelete, {
      removeUI(
        selector = "div:has(> #txt)"
      )
      added=NULL
    })
    
    #TABLE EXPRESION SET
    #LOAD EXPRES SET
    observe({
    
        file1 = input$file1
        if(is.null(input$file1)) {
            return(NULL)
        }
        else
          showNotification("Wczytano plik zbioru")
          output$file2<-renderUI({
                                    fileInput(
                                                "file2", "Wybierz plik z adnotacjami",
                                               accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")
                                              )
                                })
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
      
      output$selectClas1<-renderUI({
                                    selectInput(
                                                   "selectClas1", "Klasa 1:",
                                                   opis@data$CLASS
                                                )
                                  })
      output$selectClas2<-renderUI({
                                    selectInput(
                                                  "selectClas2", "Klasa 2:",
                                                  opis@data$CLASS
                                               )
                                  })
      output$chooseSource<-renderUI({
                                    radioButtons(
                                                  "chooseSource", "Źródło",
                                                    c(
                                                        "Plik" = "File",
                                                        "Zbiór" = "Eset"
                                                      )
                                                )
                                     })

      showNotification("Wczytano adnotacje")
      output$buttonTag<-renderUI({actionButton("buttonTag", "Konwersja oznaczeń")})

      
      #HitMap
       #hitMap<- GenerateHitMap (exprSet)
      #output$heatmap <- renderD3heatmap({d3heatmap(MAT)})
      # output$heatmap <- renderD3heatmap({d3heatmap( hitMap)})
      # showNotification("Wygenerowano Hitmapę")
       
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
  
    #ADD ANOTAION TO FILE
    observeEvent(input$buttonTag, {
      esetPath =  input$file1$datapath
      adnotationPath = input$file2$datapath
      exprSet<<-eSetAnnotation(esetPath, adnotationPath )
      if(!is.null(exprSet))
      {
        #output$exprSetTable <- renderDataTable(Transform_Exp2DataFrame(exprSet))
      }
      
    })
    
    #TAB SELECT GENES
    
    #Select geneseats from file
    observeEvent(input$fileGen, {
      
        ##TODO W MIEJSCE XXX WSTAWIĆ KLASY POBRANE Z PLIKU##
        fileName=  input$fileGen
        output$selectClas1<-renderUI({
                                        selectInput(
                                                    "selectClas1", "Klasa 1:",c("XXX")
                                        )
                                  })
        output$selectClas2<-renderUI({
                                        selectInput(
                                                    "selectClas2", "Klasa 2:",c("XXX")
                                        )
                                      })
    })
    
    #CHANGE SECOND CLASS
    observeEvent(input$selectClas1, {
      
      updateSelectInput(
                        session, "selectClas2",
                        choices =  (opis@data$CLASS[opis@data$CLASS!=input$selectClas1]),
      
                        )
    })
    
    observeEvent(input$buttonSelection, {
      
        class1= input$selectClas1
        class2= input$selectClas2
        method =input$method
        sort_criterion= input$criterion
        threshold=NULL
        number=NULL
        
        if(input$chooseMode=='number')
        {
           number=input$obs
        }
        else if(input$chooseMode=='treshold')
        {
            threshold=input$obs
        }
        
        
        ##TODO NIE WIEM CO Z TYM RESULTEM ZROBIC##
        ##PLUS NIE WIEM co ma byc tym expressetem gdy wczytujemy z pliku, chyba że ta metoda ma działać tylko przy wczytywaniu z pliku
        result= summary_table(ExprSet,klasy=c(class1,class2), method, sort_criterion, threshold, number)
        
        #Add buttons for generate hitmap i save to excel
        output$buttonSelectionHitMap<-renderUI({actionButton("buttonSelectionHitMap", "Wygeneruj hitmape")})
        output$buttonSaveMap<-renderUI({shinySaveButton("buttonSaveMap", "Zapisz", "Save file as ...", filetype=list(xlsx="xlsx"))})
    })
    
    observeEvent(input$buttonSelectionHeatmap, {
      
      resultheatmapy=GenerateHitMap(eSet)
      output$heatmap <- renderD3heatmap({d3heatmap(resultheatmapy,colors="blues")})
      
##dobrze      
    })
    
    observeEvent(input$saveExcelSelection, {
      
      
      SaveExcel=input$saveExcelSelection
      
      ##chyba dobrze
      
    })
    
    #TAB PATH
    
    #Load PVALUE
    observeEvent(input$loadPValue, {
      geneEnrichment=input$loadPValue
      
      ###TODO ZAPISYWANEI DO EXCELA Z SCIEŻEK SYGNA�OWYCH###
      
    })
    
    
    #Run analysis
    observeEvent(input$buttonPath, {
      RunGen=input$buttonPAth
      ###Ruszenie analizy scieżek
      
    })
    
    observeEvent(input$buttonPathHeatmap, {
      class1= input$selectClas1
      class2= input$selectClas2
      genesets=NULL
      geneset_name=NULL
      geneset=NULL
      
      if(input$chooseMode=='genesets')
      {
        genesets=input$obs
      }
      else if(input$chooseMode=='geneset_name')
      {
        geneset_name=input$obs
      }
      else if(input$chooseMode=='geneset')
      {
        geneset=input$obs
      }

      resultheatmap=geneset.heatmap(eSet,genesets,geneset_name,geneset,klasy=c(class1,class2))
      #geneset.heatmap=input$buttonPathHeatmap
      
      output$heatmap <- renderD3heatmap({d3heatmap(resultheatmap,colors="reds")})
      
      ###GENEROWANEI HITMAPY dla selekcji genów###
      
    })
    
    observeEvent(input$saveExcelPath, {
      SaveExcel=input$saveExcelPath
      
      ###TODO ZAPISYWANEI DO EXCELA Z SELEKCJI GENÓW###
      
    })

    
}

