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


#' The location of the cohort-generator module helper file
#'
#' @details
#' Returns the location of the cohort-generator helper file
#' 
#' @return
#' string location of the cohort-generator helper file
#'
#' @export
cohortGeneratorHelperFile <- function(){
  fileLoc <- system.file('cohort-generator-www', "cohort-generator.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

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
  
  shiny::div(
    
    shinydashboard::box(
      status = 'info', 
      width = 12,
      title = 'Cohort Generator Results',
      solidHeader = TRUE,
    
    shiny::tabsetPanel(
      id = ns("cohortGeneratorTabs"),
      
      shiny::tabPanel(
        title = "Cohort Counts",
        reactable::reactableOutput(
          outputId = ns("cohortCounts")
          )
      ),
      shiny::tabPanel(
        title = "Cohort Generation",
        DT::dataTableOutput(
          outputId = ns("cohortGeneration")
          )
      ),
      shiny::tabPanel(
        title = "Cohort Inclusions",
        reactable::reactableOutput(
          outputId = ns("inclusionStats")
          )
      )
      
    )
    
   )
  )
}




#' The module server for the main cohort generator module
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a named list containing the cohort generator results database details (schema, table prefix)
#'
#' @return
#' the cohort generator results viewer main module server
#' 
#' @export

cohortGeneratorServer <- function(
  id, 
  connectionHandler, 
  resultDatabaseSettings
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      resultsSchema <- resultDatabaseSettings$schema
      
      output$cohortCounts <- reactable::renderReactable({
        data <- getCohortGeneratorCohortCounts(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
          ) %>%
          dplyr::select("cdmSourceName",
                        "cohortId",
                        "cohortName",
                        "cohortSubjects",
                        "cohortEntries")
        reactable::reactable(data,
                             
                             )
      })

      output$cohortGeneration <- DT::renderDataTable({
        data <- getCohortGeneratorCohortMeta(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix
          )
        data
      })
      
      inclusionStats <- getCohortGeneratorCohortInclusionStats(
        connectionHandler = connectionHandler, 
        resultsSchema = resultsSchema,
        tablePrefix = resultDatabaseSettings$tablePrefix
      )
      
      output$inclusionStats <- reactable::renderReactable({
        reactable::reactable(
          data = inclusionStats,
          groupBy = c(
            "databaseId", 
            "cohortDefinitionId"
            ),
          striped = TRUE,
          filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE
        )
        
      })
      
    }
  )
}
