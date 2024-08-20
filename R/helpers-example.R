#' A connection details to an example result database
#'
#' @details
#' Finds the location within the package of an sqlite database with example results for 1) CohortGenerator, 
#' 2) Characterization, 3) PatientLevelPrediction, 4) CohortMethod, 5) SelfControlledCaseSeries and 6) CohortIncidence
#' @family {Example}
#' @return
#' The connection details to an example result database
#'
#' @export
getExampleConnectionDetails <- function(){
  server <- system.file("extdata", "results.sqlite", package = "OhdsiShinyModules")
  
  cd <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite', 
    server = server
    )
  
  return(cd)
}