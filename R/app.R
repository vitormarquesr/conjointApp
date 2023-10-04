library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)

conjointApp <- function(...){
  ui <- fluidPage(
    titlePanel("Conjoint Analysis GUI"),
    conjUI("conj1")
  )
  
  server <- function(input, output, session) {
    conjServer("conj1")
  }
  shinyApp(ui, server)

}