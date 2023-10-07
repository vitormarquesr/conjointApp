library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

conjointApp <- function(...){
  ui <- navbarPage(
    "Conjoint App",
    tabPanel("Settings"),
    tabPanel("Conjoint", conjUI("conj")),
    tabPanel("Market Prediction"),
    tabPanel("Price Optimization")
  )
  
  server <- function(input, output, session) {
    conjServer("conj")
  }
  shinyApp(ui, server)

}