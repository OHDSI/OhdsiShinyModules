# @file datasources-main.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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
          resultDatabaseSettings = resultDatabaseSettings
      )
      })
      
      # not used for now
      #dbSummaryTable <- getDbSummaryTable()
      
      dbSummaryTable <- read.csv(system.file("extras", "dbSummaryTable.csv", package = "OhdsiShinyModules")) %>%
        dplyr::select(Data.Source, cdmSourceAbbreviation, Source.Country, Data.Provenance, Visits, Source.Vocabularies) %>%
        dplyr::mutate(
          cdmSourceAbbreviation = dplyr::case_when(
            Data.Source == "Clinical Practice Research Datalink (CPRD)" ~ "CPRD",
            Data.Source == "Health Verity Comprehensive Claims and EHR Closed Claims Enrollment (Health Verity CC EHR CCE)" ~ "Health Verity CC EHR",
            Data.Source == "Premier Healthcare Database (PHD)" ~ "Premier",
            .default = cdmSourceAbbreviation
          ),
          Visits = dplyr::case_when(
            Visits == "" | Visits == " " ~ "Not Available",
            .default = Visits
          )
        ) %>%
        dplyr::rename(datasource = Data.Source,
                      #cdmSourceName = cdmSourceAbbreviation,
                      sourceCountry = Source.Country,
                      dataProvenance = Data.Provenance,
                      visits = Visits,
                      sourceVocabularies = Source.Vocabularies) %>%
        dplyr::add_row(datasource = "IQVIA Ambulatory EMR",
                       cdmSourceAbbreviation = "AMBULATORY EMR",
                       sourceCountry = "NotAv",
                       dataProvenance = "NotAv",
                       visits = "NotAv",
                       sourceVocabularies = "NotAv"
                       )
      
      dbCombined <-  dplyr::inner_join(dbSummaryTable, datasourcesData(), by = "cdmSourceAbbreviation") %>%
        dplyr::select(c(datasource, cdmSourceAbbreviation, sourceCountry, dataProvenance,
                        visits, sourceVocabularies, maxObsPeriodEndDate, cdmReleaseDate,
                        vocabularyVersion, sourceDescription))
      
  # defining column definitions
      # datasourcesColDefs <- createCustomColDefList(
      #   rawColNames = colnames(dbCombined),
      #   niceColNames = c("DB Name",
      #                    "DB Abbreviation",
      #                    "Country",
      #                    "Data Provenance",
      #                    "Visits Available",
      #                    "Source Vocabularies",
      #                    "Max Obs. Period End Date",
      #                    "CDM DB Release Date",
      #                    "Vocabulary Version",
      #                    "DB Description"),
      #   tooltipText = c("Name of the database (DB)",
      #                   "Abbreviation for the database (DB)",
      #                   "Country for the data in the database (DB)",
      #                   "Real-world data type/source",
      #                   "Types of visits available",
      #                   "Source vocabularies that are present in the database (DB)",
      #                   "Maximum/Latest observation period date in the database (DB)",
      #                   "Date the CDM database (DB) was accessible",
      #                   "Version of the vocabulary used in the database (DB)",
      #                   "Description of the database (DB)"),
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
      #     list(NULL)
      #     )
      # )
      # 
      # #save the colDefs as json
      # ParallelLogger::saveSettingsToJson(datasourcesColDefs, "./inst/components-columnInformation/datasources-new-colDefs.json")

      datasourcesColList <- ParallelLogger::loadSettingsFromJson(system.file("components-columnInformation",
                                                                        "datasources-new-colDefs.json",
                                                                        package = "OhdsiShinyModules")
      )
      
      #need to do for any colDefs that have JS and that are getting loaded in from a JSON
      #class(datasourcesColList[["sourceDocumentationReference"]]$cell) <- "JS_EVAL"
      #class(datasourcesColList[["cdmEtlReference"]]$cell) <- "JS_EVAL"
      
      
      renderDatasourcesTable <- shiny::reactive(
        {
          dbCombined
        }
      )
      
      resultTableServer(id = "datasourcesTable",
                        df = dbCombined,
                        colDefsInput = datasourcesColList,
                        selectedCols = c("cdmSourceAbbreviation", "sourceCountry", "dataProvenance",
                                         "maxObsPeriodEndDate", "cdmReleaseDate", "vocabularyVersion"),
                        sortedCols = c("cdmSourceAbbreviation"),
                        downloadedFileName = "datasourcesTable-")
      
      return(invisible(NULL))
      
      
      
      
    })
}

#pull database meta data table
getDatasourcesData <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * from @schema.@database_table
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      database_table = resultDatabaseSettings$databaseTable
    )
  )
}





