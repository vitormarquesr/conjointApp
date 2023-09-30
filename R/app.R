library(shiny)
library(dplyr)
library(ggplot2)

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
      plotOutput("info")
      
  )
  
  server <- function(input, output) {
    selected <- reactive({
      icecream %>% 
        filter(("All" == input$resp) | (respondent %in% input$resp)) %>%
        select(-profile, -respondent)
    })
    
    mod <- reactive({
      lm(ratings ~ ., data = selected())
    })
    
    pw <- reactive({
      mod_to_pw(mod())
    })
    
    iw <- reactive({
      pw_to_iw(pw())
    })
    
    
    output$info <- renderPlot({
        if (input$metric == "Part-Worths (PW)"){
          plot_pw(pw())
        }else if(input$metric == "Importance Weights (IW)"){
          plot_iw(iw())
        }
    }, res = 96)
    
  }
  
  shinyApp(ui = ui, server = server)

}