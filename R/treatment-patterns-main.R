# @file treatment-patterns-main.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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


#' The location of the treatmentPatterns module helper file
#'
#' @details
#' Returns the location of the treatment-patterns helper file
#' @family Treatment Patterns
#' @return
#' string location of the treatment-patterns helper file
#'
#' @export
cohortGeneratorHelperFile <- function() {
  fileLoc <- system.file("treatment-patterns-www", "treatment-patterns.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring treatment patterns studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id the unique reference id for the module
#' @family Treatment Patterns
#' @return
#' The user interface to the treatment-patterns viewer module
#'
#' @export
treatmentPatternsViewer <- function(id = 1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = "info", width = "100%",
    title = shiny::span(shiny::icon("sitemap"), "Treatment Patterns Viewer"),
    solidHeader = TRUE,
    
    # pick targetIds
    tableSelectionViewer(id = ns("target-table-selector")),
    shiny::uiOutput(outputId = ns("main"))
  )
}


#' The module server for exploring treatment patterns studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @family Treatment Patterns
#' @return
#' The server for the treatment-patterns module
#'
#' @export
treatmentPatternsServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # grab analsysisId, targetCohorts, eventCohorts
      targetTable <- OhdsiReportGenerator::getAnalysisCohorts(
        connectionHandler = connectionHandler,
        schema = resultDatabaseSettings$schema,
        tpTablePrefix = resultDatabaseSettings$tpTablePrefix
      )
      
      # create reactive that saved selected rowId
      reactiveTargetRowId <- shiny::reactiveVal(NULL)
      
      tableSelectionServer(
        id = "target-table-selector",
        table = shiny::reactive(
          targetTable %>% dplyr::select(-"databaseId", -"databaseName")
        ),
        selectedRowId = reactiveTargetRowId,
        selectMultiple = TRUE,
        elementId = session$ns("table-selector"),
        inputColumns = treatmentPatternsTargetInputColumns(),
        displayColumns = treatmentPatternsTargetInputColumns(),
        selectButtonText = "Select Anaylsis and Target"
      )
      
      
      reactiveTargetRow <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(reactiveTargetRowId(), {
        reactiveTargetRow(targetTable[reactiveTargetRowId(), ])
      })
      
      
      # After selecting target render UI
      output$main <- shiny::renderUI({
        req(nrow(reactiveTargetRow()) > 0)
        
        shiny::tabsetPanel(
          id = session$ns("treatmentPatternsTabs"),
          type = "pills",
          shiny::tabPanel(
            title = "Overview",
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = "100%",
              treatmentPatternsOverviewViewer(id = session$ns('sunburstView'))
            )
          ),
          shiny::tabPanel(
            title = "Tabular Data",
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = "100%",
              # treatmentPatternsTabularViewer(id = session$ns('tabularView'))
            )
          ),
          shiny::tabPanel(
            title = "Tabular Data",
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = "100%",
              
              # treatmentPatternsSankeyViewer(id = session$ns('sankeyView'))
            )
          )
        )
      })
      
      treatmentPatternsOverviewServer(
        id = 'sunburstView',
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow
      )
      
      # treatmentPatternsTabularServer(
      #   id = 'tabularView',
      #   connectionHandler = connectionHandler,
      #   resultDatabaseSettings = resultDatabaseSettings,
      #   reactiveTargetRow = reactiveTargetRow
      # )
    }
  )
}


treatmentPatternsTargetInputColumns <- function() {
  return(
    list(
      analysisId = reactable::colDef(
        show = TRUE,
        name = "Analysis Id",
      ),
      targetCohortId = reactable::colDef(
        show = TRUE,
        name = "Target Id"
      ),
      targetCohortName = reactable::colDef(
        show = TRUE,
        name = "Target",
        minWidth = 300
      ),
      eventCohortList = reactable::colDef(
        name = "Events",
        minWidth = 600
      )
    )
  )
}