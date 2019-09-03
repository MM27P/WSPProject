library(shinyWidgets)
BiocManager::install('shinyalert')
library(shinyalert)
BiocManager::install('shinyFiles')
library(shinyFiles)
BiocManager::install('xlsx')
library(xlsx)
BiocManager::install('ggplot2')
library(ggplot2)
BiocManager::install('d3heatmap')
library(d3heatmap)
BiocManager::install('pixiedust')
library(pixiedust)
BiocManager::install('dplyr')
library(dplyr)
BiocManager::install('shiny')
library(shiny)
BiocManager::install('DT')
library(DT)
options(pixiedust_print_method = "html")

source("UI.R")
source("Server.R")

# choose columns to display

#Student wykład : Plotly, selectizeinput



#smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
#colnames(smoke) <- c("Symbol","Id","Opis")
#rownames(smoke) <- c("Gen1","Gen2","Gen3")
#smoke <- as.table(smoke)
#prop.table(smoke)
options(shiny.maxRequestSize=99999*1024^2) 
shinyApp(ui=ui,server=server)


