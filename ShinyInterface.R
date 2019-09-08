library(shinyWidgets)
library(shinyalert)
library(shinyFiles)
library(xlsx)
library(ggplot2)
library(d3heatmap)
library(pixiedust)
library(dplyr)
library(shiny)
library(DT)
library(shinyjs)
library(iheatmapr)
options(pixiedust_print_method = "html")

source("UI.R")
source("Server.R")

# choose columns to display

#Student wykład : Plotly, selectizeinput

rm(list = ls())
options(shiny.maxRequestSize=99999*1024^2) 
options(encoding="UTF-8")
shinyApp(ui=ui,server=server)


