server <- function(input, output) {
  
  # choose columns to display
  smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
  colnames(smoke) <- c("Symbol","Id","Opis")
  rownames(smoke) <- c("Gen1","Gen2","Gen3")
  smoke <- as.table(smoke)

  
  #Histogram code
  data1 <- reactive({
    input$Ind
  })
  data2 <- reactive({                                                                                             
    input$Dep
  })
  
  output$BoxPlot <- renderPlot({
    boxplot(get(data2()) ~ get(data1()) , data=mtcars)
  })
  
  output$Hist <- renderPlot({
    req(data1())
    hist(mtcars[[data1()]])
  }) 
  #end


  observeEvent(input$button, {
    s= c("set12", "set22", "set32", "set42", "set52");
         reactive(s);
  })

  
  #iamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(  diamonds[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mtcars, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
}