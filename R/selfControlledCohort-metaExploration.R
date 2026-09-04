# @file selfControlledCohort-metaExploration.R
#
# Copyright 2026 Observational Health Data Sciences and Informatics
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

#' The module viewer for the meta analytic target-outcome pair exploration
#'
#' @param id the unique reference id for the module
#' @family SelfControlledCohort
#' @return
#' The user interface to the meta analytic exploration module
#' @export
selfControlledCohortMetaExplorationViewer <- function(id = "metaExploration") {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = shiny::span(shiny::icon("sliders"), "Filters"),
        collapsible = TRUE,
        collapsed = FALSE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::uiOutput(ns("analysisSelector"))
          ),
          shiny::column(
            width = 4,
            shiny::radioButtons(
              ns("status"),
              "Show pairs",
              choices = c(
                "All" = "All",
                "Passed diagnostics" = "Pass",
                "Failed diagnostics" = "Fail"
              ),
              selected = "All",
              inline = TRUE
            )
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              ns("apply"),
              "Apply filters",
              icon = shiny::icon("play"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        )
      )
    ),
    shiny::p(
      "The meta analytic (evidence synthesis) results for every target-outcome
      pair are shown with their counts, descriptive statistics and study
      diagnostics.  Effect estimates are only revealed for pairs that passed
      the study diagnostics - failed pairs are blinded (effect estimates
      shown as -).  Results are loaded from the database in pages - change the
      filters and press 'Apply filters' to update the table.  Click 'Open' on
      a row to explore that exposure-outcome pair in the pair explorer.",
      style = "margin-top: 10px;"
    ),
    largeTableView(
      id = ns("metaTable"),
      selectedPageSize = 25
    )
  )
}

#' The module server for the meta analytic target-outcome pair exploration
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list with the result schema and table
#'   prefixes
#' @param selectedPair a reactiveVal used to communicate the selected pair to
#'   the parent module
#' @family SelfControlledCohort
#' @return
#' The server for the meta analytic exploration module
#' @export
selfControlledCohortMetaExplorationServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    selectedPair
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      analyses <- shiny::reactive({
        OhdsiReportGenerator::getSccAnalysisSettings(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix
        )
      })

      output$analysisSelector <- shiny::renderUI({
        df <- analyses()
        choices <- if (nrow(df) > 0) {
          stats::setNames(df$analysisId, df$description)
        } else {
          c("No analyses" = 1)
        }
        shiny::selectInput(
          inputId = session$ns("analysisId"),
          label = "Analysis",
          choices = choices,
          selected = if (nrow(df) > 0) df$analysisId[1] else 1
        )
      })

      metaTableParams <- shiny::reactiveVal(list(
        schema = resultDatabaseSettings$schema,
        scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
        cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
        es_table_prefix = resultDatabaseSettings$esTablePrefix,
        analysis_id = 1,
        status_value = "All"
      ))

      setDefaultParams <- function() {
        metaTableParams(list(
          schema = resultDatabaseSettings$schema,
          scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
          cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
          es_table_prefix = resultDatabaseSettings$esTablePrefix,
          analysis_id = if (nrow(analyses()) > 0) analyses()$analysisId[1] else 1,
          status_value = "All"
        ))
      }

      # populate the table once with the default filters when the module loads
      shiny::observe({
        shiny::req(nrow(analyses()) > 0)
        if (is.null(input$apply) || input$apply == 0) {
          setDefaultParams()
        }
      })

      shiny::observeEvent(input$apply, {
        shiny::req(input$analysisId)
        metaTableParams(list(
          schema = resultDatabaseSettings$schema,
          scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
          cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
          es_table_prefix = resultDatabaseSettings$esTablePrefix,
          analysis_id = as.numeric(input$analysisId),
          status_value = input$status
        ))
      })

      baseSql <- selfControlledCohortMetaSql(
        schema = resultDatabaseSettings$schema,
        sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
        esTablePrefix = resultDatabaseSettings$esTablePrefix
      )
      ldt <- createLargeSqlQueryDt(
        connectionHandler = connectionHandler,
        baseQuery = baseSql
      )

      # add a link to open the pair in the pair explorer
      nsOpen <- session$ns("openPairRow")
      columns <- selfControlledCohortMetaExplorationColDef()
      columns$pairKey <- reactable::colDef(
        name = "Open",
        width = 90,
        sortable = FALSE,
        filterable = FALSE,
        cell = function(value) {
          parts <- strsplit(as.character(value), "\\|")[[1]]
          if (length(parts) != 2) {
            return("")
          }
          onclick <- sprintf(
            "Shiny.setInputValue('%s', { targetId: %s, outcomeId: %s, seed: Math.random() }, { priority: 'event' }); return false;",
            nsOpen, parts[1], parts[2]
          )
          shiny::tags$a(
            href = "#",
            class = "btn btn-xs btn-info",
            onclick = onclick,
            shiny::icon("magnifying-glass-chart"),
            " Open"
          )
        }
      )

      targetMap <- shiny::reactive({
        OhdsiReportGenerator::getSccTargets(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix
        )
      })

      shiny::observeEvent(input$openPairRow, {
        event <- input$openPairRow
        if (is.null(event) || is.null(event$targetId)) {
          return()
        }
        targetId <- as.numeric(event$targetId)
        outcomeId <- as.numeric(event$outcomeId)
        targetDf <- targetMap()
        outcomeDf <- tryCatch(
          OhdsiReportGenerator::getSccOutcomes(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema,
            sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            targetIds = targetId
          ),
          error = function(e) data.frame()
        )
        targetName <- if (length(i <- match(targetId, targetDf$cohortDefinitionId)) > 0) {
          targetDf$cohortName[i]
        } else {
          as.character(targetId)
        }
        outcomeName <- if (nrow(outcomeDf) > 0 &&
                           length(j <- match(outcomeId, outcomeDf$cohortDefinitionId)) > 0) {
          outcomeDf$cohortName[j]
        } else {
          as.character(outcomeId)
        }
        selectedPair(data.frame(
          targetId = targetId,
          targetName = targetName,
          outcomeId = outcomeId,
          outcomeName = outcomeName,
          stringsAsFactors = FALSE
        ))
      })

      largeTableServer(
        id = "metaTable",
        ldt = ldt,
        inputParams = metaTableParams,
        columns = shiny::reactive(columns)
      )
    }
  )
}

#' The column definitions for the meta analytic target-outcome pair table
#'
#' @details
#' Counts and descriptive statistics are shown for all pairs.  The calibrated
#' effect estimate columns are returned as NA (displayed as -) for any
#' evidence synthesis analysis that failed a study diagnostic
#'
#' @family SelfControlledCohort
#' @return
#' A named list of reactable::colDef
#' @export
selfControlledCohortMetaExplorationColDef <- function() {
  results <- list(
    targetId = reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(show = FALSE),
    analysisId = reactable::colDef(show = FALSE),
    evidenceSynthesisAnalysisId = reactable::colDef(show = FALSE),
    databaseName = reactable::colDef(show = FALSE),
    unblind = reactable::colDef(show = FALSE),
    timeAtRiskExposed = reactable::colDef(show = FALSE),
    timeAtRiskUnexposed = reactable::colDef(show = FALSE),

    targetName = reactable::colDef(
      name = "Exposure",
      filterable = TRUE,
      minWidth = 150
    ),
    outcomeName = reactable::colDef(
      name = "Outcome",
      filterable = TRUE,
      minWidth = 150
    ),
    description = reactable::colDef(
      name = "Analysis settings",
      filterable = TRUE,
      minWidth = 200
    ),
    overallStatus = reactable::colDef(
      name = "Status",
      filterable = TRUE,
      minWidth = 80
    ),
    mdrr = reactable::colDef(
      name = "MDRR",
      format = reactable::colFormat(digits = 2),
      na = "-"
    ),
    ease = reactable::colDef(
      name = "EASE",
      format = reactable::colFormat(digits = 3),
      na = "-"
    ),
    i2 = reactable::colDef(
      name = "I2",
      format = reactable::colFormat(digits = 2),
      na = "-"
    ),
    tau = reactable::colDef(
      name = "Tau",
      format = reactable::colFormat(digits = 3),
      na = "-"
    ),
    numPersons = reactable::colDef(
      name = "Persons",
      format = reactable::colFormat(digits = 0)
    ),
    numOutcomesExposed = reactable::colDef(
      name = "Outcomes exposed",
      format = reactable::colFormat(digits = 0)
    ),
    numOutcomesUnexposed = reactable::colDef(
      name = "Outcomes unexposed",
      format = reactable::colFormat(digits = 0)
    ),
    numExposures = reactable::colDef(
      name = "Exposures",
      format = reactable::colFormat(digits = 0)
    ),
    nDatabases = reactable::colDef(
      name = "Databases",
      format = reactable::colFormat(digits = 0)
    ),
    calibratedRr = reactable::colDef(
      name = "Calibrated IRR",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    calibratedCi95Lb = reactable::colDef(
      name = "Calibrated LB",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    calibratedCi95Ub = reactable::colDef(
      name = "Calibrated UB",
      format = reactable::colFormat(digits = 4),
      na = "-"
    )
  )
  return(results)
}
