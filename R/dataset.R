
datasetInput <- function(id){
  tagList(
    fileInput(NS(id, "upload"), "Upload", accept = ".csv", width="100%"),
    selectInput(NS(id, "example"), "Examples", 
                            choices = "icecream", width="100%"),
    actionButton(NS(id, "load"), "Load", width="100%"),
    
    verbatimTextOutput(NS(id, "teste"))
  )
}

datasetServer <- function(id){
  moduleServer(id, function(input, output, session){
    data <- eventReactive(input$load, {
      if (!is.null(input$upload)){
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = read_csv(input$upload$datapath),
               validate("Invalid file; Please upload a .csv")
               )
      }else{
        get(input$example)
      }
    })
    
    output$teste <- renderPrint(data())
    
  })
}

datasetApp <- function(){
  ui <- fluidPage(
    datasetInput("dataset1")
  )
  
  server <- function(input, output, session){
    datasetServer("dataset1")
  }
  
  shinyApp(ui, server)
}



