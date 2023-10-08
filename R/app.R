library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

conjointApp <- function(...){
  ui <- navbarPage(
    "Conjoint App",
    tabPanel("Settings", settingsUI("settings")),
    tabPanel("Conjoint", conjUI("conj")),
    tabPanel("Market Prediction"),
    tabPanel("Price Optimization")
  )
  
  server <- function(input, output, session) {
    data <- settingsServer("settings")
    conjServer("conj", data)
  }
  shinyApp(ui, server)

}