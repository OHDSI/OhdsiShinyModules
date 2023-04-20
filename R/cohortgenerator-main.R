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
      type = "pills",
      
      shiny::tabPanel(
        title = "Cohort Counts",
        shiny::downloadButton(
          ns('downloadCohortCounts'),
          label = "Download",
          icon = shiny::icon("download"),
          style="float:right"
        ),
        reactable::reactableOutput(
          outputId = ns("cohortCounts")
          )
        # ,
        # shiny::downloadButton(
        #   ns('downloadCohortCounts'),
        #   label = "Download"
        # )
      ),
      shiny::tabPanel(
        title = "Cohort Generation",
        reactable::reactableOutput(
          outputId = ns("cohortGeneration")
          )
      ),
      shiny::tabPanel(
        title = "Cohort Summary",
        reactable::reactableOutput(
          outputId = ns("inclusionSummary")
          )
      )
      
    )
    
   )
   # ,
   # shinydashboard::box(
   #   status = 'info',
   #   width = 12,
   #   # Title can include an icon
   #   title = shiny::tagList(shiny::icon("gear"), "Download"),
   # 
   #   shiny::downloadButton(
   #     ns('downloadCohortCounts'),
   #     label = "Download"
   #   )
   # )
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
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      format_yesorno <- function(value) {
        # Render as an X mark or check mark
        if (value == "COMPLETE") "\u2714\ufe0f Yes" #if generation complete then green check mark with "yes"
        else "\u274c No" #if not then red x with "no"
      }
      
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
                             columns = list(
                               # Render a "show details" button in the last column of the table.
                               # This button won't do anything by itself, but will trigger the custom
                               # click action on the column.
                               cdmSourceName = reactable::colDef( 
                                 header = withTooltip(
                                   "Database Name", 
                                   "The name of the database"
                                 )),
                               cohortId = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort ID", 
                                   "The unique numeric identifier of the cohort"
                                 )),
                               cohortName = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort Name", 
                                   "The name of the cohort"
                                 )),
                               cohortSubjects = reactable::colDef( 
                                 header = withTooltip(
                                   "Number of Subjects", 
                                   "The number of distinct subjects in the cohort"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 )),
                               cohortEntries = reactable::colDef( 
                                 header = withTooltip(
                                   "Number of Records", 
                                   "The number of records in the cohort"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 ))
                             ),
                             filterable = TRUE,
                             sortable = TRUE,
                             defaultColDef = reactable::colDef(
                               align = "left"
                             )
                             )
      })
      
      # download button
      output$downloadCohortCounts <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-count-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(getCohortGeneratorCohortCounts(
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
                          "cohortEntries"), con)
        }
      )

      output$cohortGeneration <- reactable::renderReactable({
        data <- getCohortGeneratorCohortMeta(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix
          ) %>%
          dplyr::select("cohortId",
                        "cohortName",
                        "generationStatus",
                        "startTime",
                        "endTime")
        reactable::reactable(data,
                             columns = list(
                               # Render a "show details" button in the last column of the table.
                               # This button won't do anything by itself, but will trigger the custom
                               # click action on the column.
                               cohortId = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort ID", 
                                   "The unique numeric identifier of the cohort"
                                 )),
                               cohortName = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort Name", 
                                   "The name of the cohort"
                                 )),
                               generationStatus = reactable::colDef( 
                                 header = withTooltip(
                                   "Is the Cohort Generated?", 
                                   "Indicator of if the cohort has been generated"
                                 ),
                                 cell = format_yesorno
                                 ),
                               startTime = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation Start Time", 
                                   "The time and date the cohort started generating"
                                 ),
                                 format = reactable::colFormat(datetime = TRUE
                                 )),
                               endTime = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation End Time", 
                                   "The time and date the cohort finished generating"
                                 ),
                                 format = reactable::colFormat(datetime = TRUE
                                 ))
                             ),
                             filterable = TRUE,
                             sortable = TRUE,
                             defaultColDef = reactable::colDef(
                               align = "left"
                             )
        )
      })
      
      output$inclusionSummary <- reactable::renderReactable({
        data <- getCohortGeneratorCohortInclusionSummary(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
        ) %>%
          dplyr::select("cdmSourceName",
                        "cohortDefinitionId",
                        "cohortName",
                        "baseCount",
                        "finalCount",
                        "modeId") %>%
          dplyr::mutate(modeId = 
                          dplyr::case_when(
                            modeId == 1 ~ "Subjects",
                            .default = "Records"
                          )
                        )
        reactable::reactable(data,
                             columns = list(
                               # Render a "show details" button in the last column of the table.
                               # This button won't do anything by itself, but will trigger the custom
                               # click action on the column.
                               cdmSourceName = reactable::colDef( 
                                 header = withTooltip(
                                   "Database Name", 
                                   "The name of the database"
                                 )),
                               cohortDefinitionId = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort ID", 
                                   "The unique numeric identifier of the cohort"
                                 )),
                               cohortName = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort Name", 
                                   "The name of the cohort"
                                 )),
                               baseCount = reactable::colDef( 
                                 header = withTooltip(
                                   "Base Count", 
                                   "The number of records before any inclusion criteria are applied (entry events)"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 )),
                               finalCount = reactable::colDef( 
                                 header = withTooltip(
                                   "Final Count", 
                                   "The number of records after all inclusion criteria are applied"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 )),
                               modeId = reactable::colDef( 
                                 header = withTooltip(
                                   "Records or Subjects?", 
                                   "An indicator of whether the counts shown are the number of subjects or the number of records"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 ))
                             ),
                             filterable = TRUE,
                             sortable = TRUE,
                             defaultColDef = reactable::colDef(
                               align = "left"
                             )
        )
      })
      
    }
  )
}
