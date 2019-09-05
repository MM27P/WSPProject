

source("Libraries")
#musi byc tutaj setwd bo sie k≥Ûci øe nie widzi ui i servera
setwd('C:/Users/Kara/Desktop/Projekt/WSPProject-feature-WebInterface_MG')
options(pixiedust_print_method = "html")
source("UI.R")
source("Server.R")

# choose columns to display

#Student wyk≈Çad : Plotly, selectizeinput



#smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
#colnames(smoke) <- c("Symbol","Id","Opis")
#rownames(smoke) <- c("Gen1","Gen2","Gen3")
#smoke <- as.table(smoke)
#prop.table(smoke)
options(shiny.maxRequestSize=99999*1024^2) 
shinyApp(ui=ui,server=server)


