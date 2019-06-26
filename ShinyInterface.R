library(shiny)
library(shinyWidgets)
library(ggplot2) 
source("UI.R")
source("Server.R")

# choose columns to display

#Student wykład : Plotly, selectizeinput



#smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
#colnames(smoke) <- c("Symbol","Id","Opis")
#rownames(smoke) <- c("Gen1","Gen2","Gen3")
#smoke <- as.table(smoke)
#prop.table(smoke)

shinyApp(ui=ui,server=server)


