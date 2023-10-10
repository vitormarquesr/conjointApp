conjUI <- function(id){
    tagList(
          selectInput(NS(id, "resp"), "Respondent", 
                      choices = c("All", unique(icecream$respondent))),
          tabsetPanel(
            tabPanel("Part Worths (PW)", plotOutput(NS(id, "pworths"))),
            tabPanel("Importance Weights (IW)",plotOutput(NS(id, "iweights"))),
            tabPanel("Predict", uiOutput(NS(id, "features")),
                     column(7, plotOutput(NS(id, 'prediction')))
                     )
                     
            )
          )
}

conjServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    
    filtered <- reactive({
      data() %>% 
        filter(("All" == input$resp) | (respondent %in% input$resp)) %>%
        select(-profile, -respondent) %>%
        mutate(across(-ratings, as.factor))
    })
    
    mdl <- reactive(lm(ratings ~ ., data = filtered()))
    
    part_worths <- reactive(lm_part_worths(mdl()))
    
    impt_weights <- reactive({
      part_worths() %>%
        group_by(feature) %>%
        summarize(iw = abs(max(pw) - min(pw))) %>%
        mutate(iw = iw/sum(iw))
    })
    
    output$pworths <- renderPlot(plot_pworths(part_worths()), res=96)
    
    output$iweights <- renderPlot(plot_iweights(impt_weights()), res=96)
    
    output$features <- renderUI({
      ft_names <- get_ftnames(filtered())
      map(ft_names, function(x) freezeReactiveValue(input, x))
      
      column(5, map(ft_names, function(x) selectInput(NS(id, x), x, 
                                            choices = pull(filtered(), x),
                                            selected = TRUE,
                                            width="100%"))
      )
    })
    
    output$prediction <- renderPlot({
      ft_names <- get_ftnames(model.frame(mdl()))
      X <- lapply(ft_names, function(x) input[[x]])
      names(X) <- ft_names
      y_max <- max(model.frame(mdl())$ratings)*1.4
      y_min <- -y_max*0.3
      
      plot_prediction(mdl(), as_tibble(X), y_min, y_max)
      
      }, res=96)
    
  })
}


