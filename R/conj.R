
conjUI <- function(id){
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput(NS(id, "resp"), "Respondent", 
                      choices = c("All", unique(icecream$respondent)), 
                      width="100%"),
          selectInput(NS(id, "metric"), "Metric",
                      choices=c("Part-Worths (PW)", "Importance Weights (IW)"),
                      width="100%")
          ),
          mainPanel(
            plotOutput(NS(id, "info"))
          )
      )
    )
}

conjServer <- function(id){
  moduleServer(id, function(input, output, session){
    filtered <- reactive({
      icecream %>% 
        filter(("All" == input$resp) | (respondent %in% input$resp)) %>%
        select(-profile, -respondent)
    })
    
    mdl <- reactive(lm(ratings ~ ., data = filtered()))
    
    part_worths <- reactive(lm_part_worths(mdl()))
    
    impt_weights <- reactive({
      part_worths() %>%
        group_by(feature) %>%
        summarize(iw = abs(max(pw) - min(pw))) %>%
        mutate(iw = iw/sum(iw))
    })
    
    output$info <- renderPlot({
      thm <- theme_linedraw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
      
      if (input$metric == "Part-Worths (PW)"){
        part_worths() %>%
          ggplot(aes(x=level, y=pw, group=1))+
          geom_point(size=5)+
          geom_line(linetype=4)+
          facet_wrap(~feature,
                     scales = "free") + 
          labs(x = "", y = "Part-Worths (PW)") +
          thm
        
      }else if(input$metric == "Importance Weights (IW)"){
        impt_weights() %>%
          ggplot(aes(y=iw, x = feature)) +
          geom_col()+
          labs(x = "", y = "Importance-Weights (IW)") +
          ylim(c(0, 1)) +
          geom_text(aes(y = iw, label = paste0(round(iw*100,2), "%")),
                    vjust = -0.5) +
          thm
      }
    }, res = 96)
  })
}

conjApp <- function(){
  ui <- fluidPage(
    conjUI("conj1")
  )
  
  server <- function(input, output, session){
    conjServer("conj1")
  }
  
  shinyApp(ui, server)
}
