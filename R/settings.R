
settingsUI <- function(id){
  tagList(
    column(5, datasetInput(NS(id, "dataset"))),
    column(7, selectInput(NS(id, "profile"), "Profile", choices = NULL, width="100%"),
           selectInput(NS(id, "respondent"), "Respondent", choices = NULL, width="100%"),
           selectInput(NS(id, "ratings"), "Ratings", choices = NULL, width="100%"),
           selectInput(NS(id, "price"), "Price", choices = NULL, width="100%")
           )
  )
}

settingsServer <- function(id){
  moduleServer(id, function(input, output, session) {
    data <- datasetServer("dataset")
    
    observeEvent(data(), {
      updateSelectInput(session, "profile", choices = find_vars(data()))
      updateSelectInput(session, "respondent", choices = find_vars(data()))
      updateSelectInput(session, "ratings", choices = find_vars(data(), is.numeric))
      updateSelectInput(session, "price", choices = c("Not Applicable",
                                                      find_vars(data(), is.numeric)))
    })
    
    reactive({
      data_new <- data()
      old_names <- c(input$profile, input$respondent, input$ratings)
      new_names <- c("profile", "respondent", "ratings")
      
      if (input$price != "Not Applicable"){
        old_names <- c(old_names, input$price)
        new_names <- c(new_names, "price")
      }
      names(data_new)[match(old_names, names(data_new))] <- new_names
      data_new
    })
  })
}


settingsApp <- function(){
  ui <- fluidPage(
    settingsUI("settings"),
    verbatimTextOutput("test")
  )
  
  server <- function(input, output, session){
    data <- settingsServer("settings")
    
    output$test <- renderPrint(data())
  }
  
  shinyApp(ui, server)
}
