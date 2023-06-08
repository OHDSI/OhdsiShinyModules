# @file datasources-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
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




#' Define the helper file for the module
#'
#' @return The helper html file for the datasources module
#' @export
#'
datasourcesHelperFile <- function() {
  fileLoc <-
    system.file('datasources-www', "datasources.html", package = "OhdsiShinyModules")
  return(fileLoc)
}



#' The viewer function for hte datasources module
#'
#' @param id The unique id for the datasources viewer namespace
#'
#' @return The UI for the datasources module
#' @export
#'
datasourcesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info',
    width = "100%",
    title =  shiny::span(shiny::icon("database"), "Data Sources"),
    solidHeader = TRUE,
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = FALSE,
      title = shiny::span( shiny::icon("circle-question"), "Help & Information"),
      width = "100%",
      shiny::htmlTemplate(system.file("datasources-www", "datasources.html", package = utils::packageName()))
    ),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('mainPanel'),
        
        shiny::tabPanel(
          title = "Data Source Information",
           resultTableViewer(ns("datasourcesTable"),
                             downloadedFileName = "datasourcesTable-")
        )
       )
      )
}




#' The server function for the datasources module
#'
#' @param id The unique id for the datasources server namespace
#' @param connectionHandler A connection to the database with the results
#' @param resultDatabaseSettings A named list containing the cohort generator results database details (schema, table prefix)
#'
#' @return The server for the datasources module
#' @export
#'
datasourcesServer <- function(
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
      
      datasourcesData <- shiny::reactive({
        getDatasourcesData(
        connectionHandler = connectionHandler,
        resultsSchema = resultDatabaseSettings$schema,
        databaseMetaData = resultDatabaseSettings$databaseMetaData
      )
      })
      
      #defining column definitions
      # datasourcesColDefs <- createCustomColDefList(
      #   rawColNames = colnames(datasourcesData),
      #   niceColNames = c("DB Name",
      #                    "DB Abbreviation",
      #                    "DB Holder",
      #                    "DB Description",
      #                    "DB Description Link",
      #                    "DB ETL Link",
      #                    "Source Data Release Date",
      #                    "CDM DB Release Date",
      #                    "CDM Version",
      #                    "Vocabulary Version",
      #                    "DB ID",
      #                    "Max Obs. Period End Date"),
      #   tooltipText = c("Name of the database (DB)",
      #                   "Abbreviation for the database (DB)",
      #                   "Holder of the database (DB)",
      #                   "Description of the database (DB)",
      #                   "HTML link to the database (DB) description",
      #                   "HTML link to the ETL for the database (DB)",
      #                   "Date the source data was released",
      #                   "Date the CDM database (DB) was accessible",
      #                   "Version of the common data model (CDM)",
      #                   "Version of the vocabulary used in the database (DB)",
      #                   "Unique identifier (ID) of the database (DB)",
      #                   "Maximum/Latest observation period date in the database (DB)"),
      #   customColDefOptions = list(
      #     list(NULL), 
      #     list(NULL),  
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(format = reactable::colFormat(date = T)),
      #     list(format = reactable::colFormat(date = T)),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(format = reactable::colFormat(date = T))
      #   )
      # )
      
      #save the colDefs as json
      #ParallelLogger::saveSettingsToJson(datasourcesColDefs, "./inst/components-columnInformation/datasources-colDefs.json")
      
      datasourcesColList <- ParallelLogger::loadSettingsFromJson(system.file("components-columnInformation",
                                                                        "datasources-colDefs.json",
                                                                        package = "OhdsiShinyModules")
      )
      
      
      resultTableServer(id = "datasourcesTable",
                        df = datasourcesData,
                        colDefsInput = datasourcesColList,
                        downloadedFileName = "datasourcesTable-")
      
      return(invisible(NULL))
      
      
      
      
    })
}








