library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

conjointApp <- function(...){
  ui <- navbarPage(
    "Conjoint App",
    tabPanel("Settings"),
    tabPanel("Conjoint", conjUI("conj1")),
    tabPanel("Market Prediction"),
    tabPanel("Price Optimization")
  )
  
  server <- function(input, output, session) {
    conjServer("conj1")
  }
  shinyApp(ui, server)

}