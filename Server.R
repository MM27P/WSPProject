source("API.R")
source("UI.R")

buttonAddded=FALSE

server <- function(input, output,session) {

    #Start Render optional hide/seek elements

    output$selectPanel<-renderUI({
                                  selectionPanel1
                                  })
    
    output$pathPanel<-renderUI({
                                pathPanel1
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

      showNotification("Wczytano adnotacje")
      output$buttonTag<-renderUI({actionButton("buttonTag", "Konwersja oznaczeń")})
       
    })
  
    
    #ButtonTag
  
    #ADD ANOTAION TO FILE
    observeEvent(input$buttonTag, {
      esetPath =  input$file1$datapath
      adnotationPath = input$file2$datapath
      exprSet<<-eSetAnnotation(esetPath, adnotationPath )
      if(is.null(exprSet))
      {
        #output$exprSetTable <- renderDataTable(Transform_Exp2DataFrame(exprSet))
      }
      showNotification("Dokoano konwersji oznaczeń")
      
      opis<<-description(input$file2$datapath)
      
      output$selectPanel<-renderUI({
        selectionPanel2
      })
      
      output$selectClas1<-renderUI({
        selectInput(
          "selectClas1", "Klasa 1:",
          choices=c(opis@data$CLASS),
          selected  =  1
        )
      })
      output$selectClas2<-renderUI({
        selectInput(
          "selectClas2", "Klasa 2:",
          choices=c(opis@data$CLASS),
          selected=2
        )
      })
    })
    
    #TAB SELECT GENES
    
    #CHANGE SECOND CLASS
    observeEvent(input$selectClas1, {

        
      updateSelectInput(
                        session, "selectClas2",
                        choices =  (opis@data$CLASS[opis@data$CLASS!=input$selectClas1])
                        )
    })
    
    observeEvent(input$buttonSelection, {
      
        class1= input$selectClas1
        class2= input$selectClas2
        method =input$method
        sort_criterion= input$criterion
        threshold=NULL
        number=NULL
        ExprSet = exprSet
        
        if(input$chooseMode=='number')
        {
           number=input$obs
        }
        else if(input$chooseMode=='treshold')
        {
            threshold=input$obs
        }
        
        classes=c(class1,class2)
        ##TODO NIE WIEM CO Z TYM RESULTEM ZROBIC##
        ##PLUS NIE WIEM co ma byc tym expressetem gdy wczytujemy z pliku, chyba że ta metoda ma działać tylko przy wczytywaniu z pliku
        resultSelect<<- summary_table(ExprSet,klasy=c(class1,class2), method, sort_criterion, threshold, number)
        classes<<-classes
        showNotification("Dokonano selekcji")
        #Add buttons for generate hitmap i save to excel
        choices= c("Selekcja" = "Eset")
        if(exists("pvalueFile") && !is.null(pvalueFile))
        {
          choices= c("Plik"="File","Selekcja" = "Eset")
        }
        output$chooseSource2<-renderUI({
                                          radioButtons(
                                            "chooseSource2", "Źródło",
                                            choices
                                          )
                                        })
        
        output$specialPanelSelect<-renderUI({specialPanelSelect})
        output$pathPanel<-renderUI({
                                      pathPanel2
                                    })
    })
    
    observeEvent(input$buttonSelectionHeatmap, {

      
      
      resultheatmapy=geneset.heatmap(exprSet, geneset = as.character(resultSelect[[1]]$SYMBOL),classes=classes)
      output$heatmap <-renderD3heatmap({ d3heatmap(resultheatmapy,colors=rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255)),
      Colv = F, scale = 'row')})
      showNotification("Wygenerowano heatmap")
##dobrze      
    })
    

    
    #TAB PATH
    
    #Load PVALUE
    observeEvent(input$loadPValue, {
      filePath= input$loadPValue$datapath
      if(is.null(filePath))
      {
        return (NULL);
      }
      pvalueFile<<-read.xlsx(filePath)
      
      choices= c("Plik"="File")
      if(exists("resultSelect") && !is.null(resultSelect))
      {
        choices= c("Plik"="File","Selekcja" = "Eset")
      }
      
      output$chooseSource2<-renderUI({
        radioButtons(
          "chooseSource2", "Źródło",
          choices
        )
      })
      
      output$pathPanel<-renderUI({
        pathPanel2
      })
      
    })
    
    
    #Run analysis
    observeEvent(input$buttonPath, {
    
      paths=  input$variable
      if( is.null(paths))
      {
        shinyalert("Error!", "Nie wybrano ścieżek sygnałowych", type = "error")
        return()
      }
      method = input$method2
      fdr=input$FDR_Correction
      genes<<-importGeneSets(paths)
      set=NULL
      if(input$chooseSource2=="File")
      {
        set=pvalueFile
      }
      else if(input$chooseSource2=="Eset")
      {
        set= resultSelect[[2]]
      }
      
      set=resultSelect[[2]]
      
      result2<<-geneEnrichment(set,genesets= genes, method = method, FDR_adjustment = fdr)
      showNotification("Dokonano analizy scieżek")
      output$specialPanelPath<-renderUI({specialPanelPath})
      
      if(input$chooseSource2=="Eset")
      {
        output$specialPanelPath2<-renderUI({specialPanelPath2})
      }
      else
      {
        removeUI(
          selector = "div:has( =#specialPanelPath2)"
        )
      }

      ###Ruszenie analizy scieżek
      
    })
    
    observeEvent(input$buttonPathHeatmap, {

      if(input$pathTextBox %in% names(genes))
      {
       
      lol=genes
      ll2=names(genes)
      name=input$pathTextBox
      resultheatmapy=geneset.heatmap(exprSet, genesets = genes, geneset_name=input$pathTextbox,classes=classes)
      showNotification("Wygenerowano heatmap")

      #geneset.heatmap=input$buttonPathHeatmap
      
      output$heatmap1 <- renderD3heatmap({d3heatmap(resultheatmap,colors=rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(255)),
                                                   Colv = F, scale = 'row')})
      }
      else
      {
        shinyalert("Error!", "Nie wybrano ścieżki do heatmapy", type = "error")
        return(NULL)
      }
      ###GENEROWANEI HITMAPY dla selekcji genów###
      
    })
    
    #SAVE EXCEL
    observe({
      
      volumes <- c("UserFolder"=getwd())
      shinyFileSave(input, "saveExcelSelection", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$saveExcelSelection)
      
      if (nrow(fileinfo) > 0) {
        SaveExcel(resultSelect[[1]],fileinfo$datapath)
        showNotification("Zapisano wyniki selekcji do pliku Excel")
      }
    })
    
    observe({
      
      volumes <- c("UserFolder"=getwd())
      shinyFileSave(input, "saveExcelPath", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$saveExcelPath)
      
      if (nrow(fileinfo) > 0) {
        SaveGenesetExcel(result2, fileinfo$datapath)
        showNotification("Zapisano wyniki analizy do pliku Excel")
      }
    })
}

