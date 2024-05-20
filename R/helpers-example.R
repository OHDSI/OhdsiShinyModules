#' A connection details to an example result database
#'
#' @details
#' Finds the location within the package of an sqlite database with example results for 1) CohortGenerator, 
#' 2) Characterization, 3) PatientLevelPrediction, 4) CohortMethod, 5) SelfControlledCaseSeries and 6) CohortIncidence
#'
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

#' Get design spec of example result database
#'
#' @details
#' Finds the location within the package of the analysis spec json with example results for 1) CohortGenerator, 
#' 2) Characterization, 3) PatientLevelPrediction, 4) CohortMethod, 5) SelfControlledCaseSeries and 6) CohortIncidence
#'
#' @return
#' The json of the analysis spec for the example results
#'
#' @export
getExampleAnalysisSpec <- function(){
  jsonPath <- system.file("extdata", "analysisSpecification.json", package = "OhdsiShinyModules")
  specJson <- RJSONIO::fromJSON(jsonPath, digits = 23)

  return(specJson)
}
