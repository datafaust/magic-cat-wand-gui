library(shiny)


startServo <- function() {
  response = httr::GET(
    paste0(
      Sys.getenv("BASE_URL"),
      "start"
      )
    )
  print(response)
}

stopServo <- function() {
  response = httr::GET(
    paste0(
      Sys.getenv("BASE_URL"),
      "stop"
    )
  )
  print(response)
}

testServo <- function() {
  response = httr::GET(
    paste0(
      Sys.getenv("BASE_URL"),
      "test"
    )
  )
  print(response)
}


server <- function(input, output) {
  
  output$ui <- renderUI({
    uiOutput("authenticated") 
  })
  
  
  output$authenticated <- renderUI({
    
    fluidPage(
      fluidRow(
        mainPanel(
          tabsetPanel(
            tabPanel("Log Blood Pressure", uiOutput("dash"))
          )
        )
      )
    )
  })
  
  output$dash <- renderUI({
    fluidPage(
      fluidRow(
        actionButton("start", "Start Servo")
      ),
      br(),
      fluidRow(
        actionButton("stop", "Stop Servo")
      ),
      br(),
      fluidRow(
        actionButton("test", "Test Servo")
      )
    )
  })
  
  
  observeEvent(input$start, {
    startServo()
  })
  
  observeEvent(input$start, {
    stopServo()
  })
  
  observeEvent(input$start, {
    testServo()
  })
  
}