# beginner servo-----
beginnerServo <- function(mode) {
  
  url <- paste0(
    Sys.getenv("BASE_URL"),
    "start?mode=",
    mode
  )
  
  print(url)
  
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
