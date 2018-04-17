library(shiny)
library(shinyFiles)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ROCR)
shinyUI(navbarPage(title="Stock Market Forecasting",
tabPanel(title="Import",sidebarLayout(
  sidebarPanel(
    fileInput('file','Choose  file',
    accept = c("text/csv","text/comma-separated-values,text/plain",
               ".csv",".xls",".xlsx",".json","Excel-Worksheet")),
checkboxInput('Header','Header',TRUE),
radioButtons('sep','seperator',c(comma=',',slash='/',space=' ',semicolon=';',tab='\t')),multiple = TRUE),
mainPanel(title="Import",dataTableOutput('DataTable')))),

navbarMenu(title="Visualizations",tabPanel(title="single plots",
                                           sidebarPanel(radioButtons("Models","Select the model",
                                           choices = c("SES","DES","TES","Arima","RNN"))),
                                           uiOutput("std.dev")),
                           tabPanel(title="Full Plot",uiOutput("Amy"))),
                                    


theme =shinytheme("superhero") ))
