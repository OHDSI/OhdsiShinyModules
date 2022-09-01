# @file cohortgenerator-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.




#' The viewer of the main cohort generator module
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort generator results viewer
#' 
#' @export
cohortGeneratorViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  fluidPage(
    tabsetPanel(
      id = ns("cohortGeneratorTabs"),
      tabPanel(title = "Cohort Counts",
               DT::dataTableOutput(outputId = ns("cohortCounts"))
      ),
      tabPanel(title = "Cohort Generation",
               DT::dataTableOutput(outputId = ns("cohortGeneration"))
      ),
      tabPanel(title = "Cohort Inclusions",
               reactable::reactableOutput(outputId = ns("inclusionStats"))
      )
    )
  )
}




#' The module server for the main cohort generator module
#'
#' @param id the unique reference id for the module
#' @param resultDatabaseSettings a named list containing the cohort generator results database connection details
#' @param resultsSchema the schema with the cohort generator results
#'
#' @return
#' the cohort generator results viewer main module server
#' 
#' @export
cohortGeneratorServer <- function(id, resultDatabaseSettings) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      resultsSchema <- resultDatabaseSettings$schema
      
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = resultDatabaseSettings$dbms,
                                                                      user = resultDatabaseSettings$user,
                                                                      password = resultDatabaseSettings$password,
                                                                      server = sprintf("%s/%s", resultDatabaseSettings$server,
                                                                                       resultDatabaseSettings$database))
      
      
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      
      output$cohortCounts <- DT::renderDataTable({
        data <- getCohortGeneratorCohortCounts(connection, resultsSchema)
        data
      })
      
      output$cohortGeneration <- DT::renderDataTable({
        data <- getCohortGeneratorCohortMeta(connection, resultsSchema)
        data
      })
      
      output$inclusionStats <- reactable::renderReactable({
        data <- getCohortGeneratorCohortInclusionStats(connection, resultsSchema)
        
        reactable::reactable(
          data,
          groupBy = c("databaseId", "cohortDefinitionId"),
          striped = TRUE,
          filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE
        )
        
      })
      
    }
  )
}