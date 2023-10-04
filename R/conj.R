varsIcecream <- function() colnames(icecream)[2:5]

conjUI <- function(id){
    tagList(
          titlePanel("Conjoint Analysis"),
          selectInput(NS(id, "resp"), "Respondent", 
                      choices = c("All", unique(icecream$respondent))),
          tabsetPanel(
            tabPanel("PW", plotOutput(NS(id, "pworths"))),
            tabPanel("IW",plotOutput(NS(id, "iweights"))),
            tabPanel("Predict", 
                     column(7, map(varsIcecream(), 
                              function(x) selectInput(NS(id, x), x, 
                                                      choices = pull(icecream, x),
                                                      selected = TRUE))
                                        ),
                             
                        column(5, verbatimTextOutput(NS(id, 'prediction')))
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
    
    
    output$prediction <- renderPrint({
      newdata <- lapply(varsIcecream(), function(x) input[[x]])
      names(newdata) <- varsIcecream()
      newdata <- as_tibble(newdata)
      
      predict(mdl(), newdata)
      
    }, width='10000')
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

