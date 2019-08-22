test = c("set12", "set22", "set32", "set42", "set52") 

server <- function(input, output,session) {
  

  
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

  observeEvent(input$do, {  output$moreControls <- renderUI({
    radioButtons(inputId="choice", label="Loaded sets", 
                 choices=test) })
  })  

  observeEvent(input$do2, {
    x <- input$choice 
    updateRadioButtons(session, "choice",
                       label = paste("radioButtons label", x),
                       choices = x,
                       selected = x
    )
    }
  )  
  

  
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