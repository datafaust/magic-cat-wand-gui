library(shiny)
library(shinyjs)
library(shinyWidgets)

source("./servo_functions.R")

killAll <- function() {
  
  url <- paste0(
    Sys.getenv("BASE_URL"),
    "kill_all"
  )
  tryCatch(
    expr = {
      resp <- httr::GET(url)
      resp <- jsonlite::fromJSON(rawToChar(resp$content))
      
    }, error = function(e) {
      list(connection_status = "connection error")
    }, warning = function(w) {
      list(connection_status = "connection error")
    }
  )
}

# test connection function
testConnection <- function() {
  
  tryCatch(
    expr = {
      resp <- httr::GET(
        paste0(
          Sys.getenv("BASE_URL"),
          "amiconnected"
        )
      )
      
      resp <- jsonlite::fromJSON(rawToChar(resp$content))
    },
    error = function(e){ 
      list(connection_status = "connection error")
    },
    warning = function(w){
      list(connection_status = "connection warning")
    }
  )
}

server <- function(input, output, session) {
  
  output$ui <- renderUI({
    uiOutput("authenticated") 
  })
  
  
  output$authenticated <- renderUI({
    
    fluidPage(
      setBackgroundColor(
        color = c("#89CFF0")
      ),
      useShinyjs(),
      br(),
      uiOutput("connection_panel"),
      uiOutput("main")
    )
  })
  
  output$connection_panel <- renderUI({
    fluidPage(
      column(12, align="right",
             fluidRow(
               htmlOutput("connection_status")
             ),
             fluidRow(
               textOutput("job_status")
             )
        )
    )
  })
  
  output$main <- renderUI({
    fluidRow(
      column(12,align="center",
             br(),
             fluidRow(
               
               actionButton("amiconnected", "Am I Connected?")
             ),
             br(),
     
               fluidRow(
               #actionButton("killall", "Stop the Magic")
               tags$button(
                 id = "killall",
                 class = "btn action-button",
                 tags$img(src = "stop.png",
                          height = "100px")
               ),
               fluidRow(
                 p("Stop the Magic")
               )
               
             ),
             br(),
             fluidRow(
               #actionButton("beginner", "My Cat Likes Gentle Play"),
               tags$button(
                 id = "beginner",
                 class = "btn action-button",
                 tags$img(src = "catBeginner.png",
                          height = "100px")
               ),
               fluidRow(
                 p("My Cat Likes Gentle Play")
               )
             ),
             br(),
             fluidRow(
               #actionButton("intermediate", "My Cat likes Adventure"),
               tags$button(
                 id = "intermediate",
                 class = "btn action-button",
                 tags$img(src = "catIntermediate.png",
                          height = "100px")
               ),
               fluidRow(
                 p("My Cat likes Adventure")
               )
             ),
             br(),
             fluidRow(
               #actionButton("advanced", "My Cat is a Nut"),
               tags$button(
                 id = "advanced",
                 class = "btn action-button",
                 tags$img(src = "catAdvanced.png",
                          height = "100px")
               ),
               fluidRow(
                 p("My Cat is a Nut")
               )
             )
      )
    )
  })
  
  
  # Connection Management -----
  connection <- reactiveValues(
    connection_status = NULL,
    job_type = NULL,
    job_pid = NULL
  )
  
  buttons <- reactiveValues(
    beginner = FALSE,
    intermediate = FALSE,
    advanced = FALSE
  )
  
  # Event Managment ------------
  observeEvent(input$beginner, {
    
    # disable other buttons
    shinyjs::disable("intermediate")
    shinyjs::disable("advanced")
    
    # coms
    buttons$beginner <- TRUE
    mode <- "beginner"
    con <- beginnerServo(mode)
    connection$job_type <- con$job_type
    job_pid <- con$job_pid
  })
  
  observeEvent(input$intermediate, {
    
    # disable other buttons
    shinyjs::disable("beginner")
    shinyjs::disable("advanced")
    
    # coms
    buttons$intermediate <- TRUE
    mode <- "intermediate"
    con <- beginnerServo(mode)
    connection$job_type <- con$job_type
    job_pid <- con$job_pid
  })
  
  observeEvent(input$advanced, {
    
    # disable other buttons
    shinyjs::disable("intermediate")
    shinyjs::disable("beginner")
    
    # coms
    buttons$advanced <- TRUE
    mode <- "advanced"
    con <- beginnerServo(mode)
    connection$job_type <- con$job_type
    job_pid <- con$job_pid
  })
  
  observeEvent(input$killall, {
    con <- killAll()
    connection$job_type <- 'Job Stopped'
    job_pid <- NULL
    
    # enable any disabled buttons
    shinyjs::enable("beginner")
    shinyjs::enable("advanced")
    shinyjs::enable("intermediate")
  })
  
  observeEvent(input$amiconnected, {
    #print("testing connection")
    con <- testConnection()
    connection$connection_status <- con$connection_status
    
  })
  
    output$connection_status <- renderText({
      paste0("Connection Status: ",if(is.null(connection$connection_status)) { "Not Connected" } else { paste("<b>",isolate(connection$connection_status),"</b>") } )
    })
    
    output$job_status <- renderText({
      paste0("Job Status: ",if(is.null(connection$job_type)) { "No Job Running" } else { isolate(connection$job_type) } )
      
    })
   
}

