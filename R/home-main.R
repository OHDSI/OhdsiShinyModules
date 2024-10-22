#' The location of the home module helper file
#'
#' @details
#' Returns the location of the home helper file
#' @family {Home}
#' @return
#' string location of the home helper file
#'
#' @export
homeHelperFile <- function(){
  fileLoc <- system.file('home-www', "home.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring home
#'
#' @details
#' The user specifies the id for the module
#' @family {Home}
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the home viewer module
#'
#' @export
homeViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  screens <- list(
    shinyglide::screen(
      shiny::p("Assure Executive Summary"),
      shiny::p("Study Name"),
      shiny::p("The study question was..."),
      shiny::p("<list available results>"),
      next_label="Estimation Results"
    ),
    shinyglide::screen(
      shiny::p("Estimation Results"),
      shiny::p("User Inputs are possible"),
      shiny::numericInput(
        inputId = ns("n"),
        label =  "n", 
        value = 10, 
        min = 10
        ),
      next_label="Prediction Results"
    ),
    shinyglide::screen(
      shiny::p("Cool plot here"),
      shiny::plotOutput(ns("cool_plot"))
    )
  )
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("house"), "Executive Summary"),
    solidHeader = TRUE,
    
    shinyglide::glide(
      height = "350px",
      screens
    )
    
  )
  
}


#' The module server for exploring home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#' @family {Home}
#' @return
#' The server for the home module
#'
#' @export
homeServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$cool_plot <- shiny::renderPlot({
        graphics::hist(
          stats::rnorm(input$n), 
          main = paste("n =", input$n), 
          xlab = ""
          )
      })
      
    }
  )
}
