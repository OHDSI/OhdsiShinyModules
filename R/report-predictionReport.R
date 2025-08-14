predictionReportViewer <- function(
    id = 'predictionReport'
){
  
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    title = 'Prediction report inputs: ', 
    solidHeader = TRUE
      )
  
}

predictionReportServer <- function(
    id = 'predictionReport',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    server = Sys.getenv("RESULTS_SERVER"), 
    username = Sys.getenv("RESULTS_USER"), 
    password = Sys.getenv("RESULTS_PASSWORD"), 
    dbms = Sys.getenv("RESULTS_DBMS")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
     # add code here
      
      
      
    }
  )
}

