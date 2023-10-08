conjUI <- function(id){
    tagList(
          selectInput(NS(id, "resp"), "Respondent", 
                      choices = c("All", unique(icecream$respondent))),
          tabsetPanel(
            tabPanel("Part Worths (PW)", plotOutput(NS(id, "pworths"))),
            tabPanel("Importance Weights (IW)",plotOutput(NS(id, "iweights"))),
            tabPanel("Predict", uiOutput(NS(id, "fcts")),
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
        select(-profile, -respondent)
    })
    
    name_factors <- reactive({
      filtered() %>%
        select(-ratings) %>%
        names()
    })
    
    mdl <- reactive(lm(ratings ~ ., data = filtered()))
    
    part_worths <- reactive(lm_part_worths(mdl()))
    
    impt_weights <- reactive({
      part_worths() %>%
        group_by(feature) %>%
        summarize(iw = abs(max(pw) - min(pw))) %>%
        mutate(iw = iw/sum(iw))
    })
    
    output$pworths <- renderPlot({
      part_worths() %>%
        ggplot(aes(x=level, y=pw, group=1))+
        geom_point(size=5)+
        geom_line(linetype=4)+
        facet_wrap(~feature,
                   scales = "free") + 
        labs(x = "", y = "Part-Worths (PW)") +
        theme_linedraw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
    }, res=96)
    
    output$iweights <- renderPlot({
      impt_weights() %>%
        ggplot(aes(y=iw, x = feature)) +
        geom_col()+
        labs(x = "", y = "Importance-Weights (IW)") +
        ylim(c(0, 1)) +
        geom_text(aes(y = iw, label = paste0(round(iw*100,2), "%")),
                  vjust = -0.5) +
        theme_linedraw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
    }, res=96)
    
    output$fcts <- renderUI({
      column(5, map(name_factors(), 
                    function(x) selectInput(NS(id, x), x, 
                                            choices = pull(filtered(), x),
                                            selected = TRUE,
                                            width="100%"))
      )
    })
    
    output$prediction <- renderPlot({
      newdata <- lapply(name_factors(), function(x) input[[x]])
      names(newdata) <- name_factors()
      newdata <- as_tibble(newdata)
      
      tibble(y_fitted = predict(mdl(), newdata),
             rating = "") %>%
        ggplot(aes(x=rating, y=y_fitted))+
            geom_col()+
            coord_cartesian(ylim=c(-max(filtered()$ratings)*0.4, max(filtered()$ratings)*1.4))+
            labs(x = "", y = "", title = "Predicted Rating")+
            geom_text(aes(y = y_fitted, label = round(y_fitted,2)),
                  vjust = -0.5)+
            theme_linedraw()
            
      }, res=96)
    
  })
}


