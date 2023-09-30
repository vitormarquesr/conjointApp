library(shiny)
library(dplyr)


conjointApp <- function(...){
  ui <- fluidPage(
      titlePanel("Conjoint Analysis GUI"),
      fluidRow(column(6, selectInput("resp", "Respondent", 
                           choices = c("All", unique(icecream$respondent)), 
                           width="100%")
                      ),
               column(4, selectInput("metric", "Metric",
                                     choices=c("Part-Worths (PW)", "Importance Weights (IW)"),
                                      width="100%"))
               ),
      plotOutput("pw")
      
  )
  
  server <- function(input, output) {
    filtered <- reactive({
      icecream %>% 
        filter(("All" == input$resp) | (respondent %in% input$resp)) %>%
        select(-profile, -respondent)
    })
    
    data_pw <- reactive({
      mod <- lm(ratings ~ ., data = filtered())
      model_to_pw(mod)
    })
    
    output$pw <- renderPlot({
        viz_pw(data_pw())
    }, res = 96)
    
  }
  
  shinyApp(ui = ui, server = server)

}