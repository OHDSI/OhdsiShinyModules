# @file selfControlledCohort-pair.R
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

#' The module viewer for exploring a single exposure-outcome pair
#'
#' @param id the unique reference id for the module
#' @family SelfControlledCohort
#' @return
#' The user interface to the pair explorer module
#' @export
selfControlledCohortPairViewer <- function(id = "pair") {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = shiny::span(shiny::icon("magnifying-glass-chart"), "Choose an exposure-outcome pair"),
        collapsible = TRUE,
        collapsed = FALSE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::uiOutput(ns("targetSelector"))
          ),
          shiny::column(
            width = 4,
            shiny::uiOutput(ns("outcomeSelector"))
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              ns("viewPair"),
              "View pair",
              icon = shiny::icon("play"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        )
      )
    ),
    shiny::uiOutput(ns("pairSummary")),
    shiny::p(
      "All results shown below are empirically calibrated estimates.",
      style = "color: #555; font-style: italic; margin-bottom: 10px;"
    ),
    shiny::tabsetPanel(
      type = "pills",
      id = ns("pairTabset"),
      shiny::tabPanel(
        title = "Detailed results",
        value = "detailed",
        resultTableViewer(
          id = ns("detailedTable"),
          boxTitle = "Detailed results"
        )
      ),
      shiny::tabPanel(
        title = "Diagnostics",
        value = "diagnostics",
        resultTableViewer(
          id = ns("diagnosticsTable"),
          boxTitle = "Study diagnostics"
        )
      ),
      shiny::tabPanel(
        title = "Forest plot",
        value = "forest",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("forestPlot"), height = "600px")
        )
      ),
      shiny::tabPanel(
        title = "Systematic error",
        value = "systematicError",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("systematicErrorPlot"), height = "600px")
        )
      ),
      shiny::tabPanel(
        title = "Time on treatment",
        value = "timeOnTreatment",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("timeOnTreatmentPlot"), height = "400px")
        )
      ),
      shiny::tabPanel(
        title = "Time to outcome",
        value = "timeToOutcome",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("timeToOutcomePlot"), height = "400px")
        )
      )
    )
  )
}

#' The module server for exploring a single exposure-outcome pair
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list with the result schema and table
#'   prefixes
#' @param selectedPair a reactiveVal holding the selected pair (a one row
#'   data.frame with targetId, targetName, outcomeId and outcomeName)
#' @family SelfControlledCohort
#' @return
#' The server for the pair explorer module
#' @export
selfControlledCohortPairServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    selectedPair
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      chosenPair <- shiny::reactiveVal(NULL)

      # allow a pair to be opened from elsewhere (e.g. via a link from the
      # parent module)
      shiny::observe({
        if (!is.null(selectedPair())) {
          chosenPair(selectedPair())
        }
      })

      targets <- shiny::reactive({
        OhdsiReportGenerator::getSccTargets(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix
        )
      })

      output$targetSelector <- shiny::renderUI({
        df <- targets()
        choices <- stats::setNames(df$cohortDefinitionId, df$cohortName)
        shiny::selectInput(
          inputId = session$ns("targetId"),
          label = "Exposure (target cohort)",
          choices = choices,
          selected = if (nrow(df) > 0) df$cohortDefinitionId[1] else NULL
        )
      })

      outcomes <- shiny::reactive({
        shiny::req(input$targetId)
        OhdsiReportGenerator::getSccOutcomes(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          targetIds = as.numeric(input$targetId)
        )
      })

      output$outcomeSelector <- shiny::renderUI({
        df <- outcomes()
        choices <- stats::setNames(df$cohortDefinitionId, df$cohortName)
        shiny::selectInput(
          inputId = session$ns("outcomeId"),
          label = "Outcome cohort",
          choices = choices,
          selected = if (nrow(df) > 0) df$cohortDefinitionId[1] else NULL
        )
      })

      shiny::observeEvent(input$viewPair, {
        shiny::req(input$targetId, input$outcomeId)
        targetDf <- targets()
        outcomeDf <- outcomes()
        chosenPair(data.frame(
          targetId = as.numeric(input$targetId),
          targetName = targetDf$cohortName[match(
            as.numeric(input$targetId), targetDf$cohortDefinitionId
          )],
          outcomeId = as.numeric(input$outcomeId),
          outcomeName = outcomeDf$cohortName[match(
            as.numeric(input$outcomeId), outcomeDf$cohortDefinitionId
          )],
          stringsAsFactors = FALSE
        ))
      })

      pair <- shiny::reactive({
        chosenPair()
      })

      output$pairSummary <- shiny::renderUI({
        p <- pair()
        if (is.null(p)) {
          return(shiny::p(
            "No pair selected.  Choose an exposure and outcome above and press
            'View pair' to explore the self controlled cohort results.",
            style = "font-weight: bold;"
          ))
        }
        shiny::p(
          shiny::strong("Exposure:"), p$targetName,
          shiny::strong(" Outcome:"), p$outcomeName,
          if (!is.null(p$metaRr)) {
            shiny::tagList(
              shiny::strong(" Meta RR:"), round(p$metaRr, 2)
            )
          },
          style = "font-weight: normal; margin-bottom: 10px;"
        )
      })

      combinedData <- shiny::reactive({
        p <- pair()
        if (is.null(p)) {
          return(data.frame())
        }
        est <- tryCatch(
          OhdsiReportGenerator::getSccEstimation(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema,
            sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            databaseTable = resultDatabaseSettings$databaseTable,
            targetIds = p$targetId,
            outcomeIds = p$outcomeId
          ),
          error = function(e) data.frame()
        )
        meta <- tryCatch(
          OhdsiReportGenerator::getSccMetaEstimation(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema,
            sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            esTablePrefix = resultDatabaseSettings$esTablePrefix,
            targetIds = p$targetId,
            outcomeIds = p$outcomeId
          ),
          error = function(e) data.frame()
        )
        return(selfControlledCohortCombineResults(est, meta))
      })

      resultTableServer(
        id = "detailedTable",
        df = combinedData,
        colDefsInput = selfControlledCohortDetailedColDef(),
        elementId = session$ns("detailedTable")
      )

      diagnosticsData <- shiny::reactive({
        p <- pair()
        if (is.null(p)) {
          return(data.frame())
        }
        OhdsiReportGenerator::getSccDiagnosticsData(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          targetIds = p$targetId,
          outcomeIds = p$outcomeId
        )
      })

      resultTableServer(
        id = "diagnosticsTable",
        df = diagnosticsData,
        colDefsInput = selfControlledCohortDiagnosticsColDef(),
        elementId = session$ns("diagnosticsTable")
      )

      output$forestPlot <- shiny::renderPlot({
        shiny::req(pair())
        data <- combinedData()
        if (nrow(data) == 0) {
          return(NULL)
        }
        plot <- OhdsiReportGenerator::plotSccForest(
          data = data,
          calibrated = TRUE
        )
        return(plot)
      })

      controlEstimates <- shiny::reactive({
        p <- pair()
        if (is.null(p)) {
          return(data.frame())
        }
        OhdsiReportGenerator::getSccNegativeControlEstimates(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          targetIds = p$targetId
        )
      })

      output$systematicErrorPlot <- shiny::renderPlot({
        shiny::req(pair())
        data <- controlEstimates()
        if (nrow(data) == 0) {
          return(NULL)
        }
        plot <- OhdsiReportGenerator::plotSccSystematicError(data)
        return(plot)
      })

      summaryStats <- shiny::reactive({
        statType <- NULL
        p <- pair()
        if (is.null(p)) {
          return(data.frame())
        }
        OhdsiReportGenerator::getSccSummaryStats(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          statTypes = NULL,
          targetIds = p$targetId,
          outcomeIds = p$outcomeId
        )
      })

      output$timeOnTreatmentPlot <- shiny::renderPlot({
        shiny::req(pair())
        data <- summaryStats()
        data <- data[data$statType == "time_exposed", , drop = FALSE]
        if (nrow(data) == 0) {
          return(NULL)
        }
        OhdsiReportGenerator::plotSccBoxPlot(
          data = data,
          xLabel = "Data source",
          yLabel = "Time exposed (days)"
        )
      })

      output$timeToOutcomePlot <- shiny::renderPlot({
        shiny::req(pair())
        data <- summaryStats()
        data <- data[data$statType %in%
                       c("time_to_outcome", "time_to_outcome_exposed",
                         "time_to_outcome_unexposed"), , drop = FALSE]
        if (nrow(data) == 0) {
          return(NULL)
        }
        OhdsiReportGenerator::plotSccBoxPlot(
          data = data,
          xLabel = "Data source",
          yLabel = "Time to outcome (days)"
        )
      })
    }
  )
}

#' Combine the per database and meta analysis self controlled cohort results
#'
#' @details
#' Only the empirically calibrated estimates are returned - the uncalibrated
#' estimates are removed so that the results shown and downloaded always use
#' the calibrated estimates
#'
#' @param estimation the per database estimates from getSccEstimation
#' @param meta the meta analysis estimates from getSccMetaEstimation
#' @family SelfControlledCohort
#' @return
#' A data.frame with the calibrated estimates and a meta indicator column
#' @export
selfControlledCohortCombineResults <- function(estimation, meta) {
  commonCols <- c(
    "databaseName", "databaseId", "analysisId", "description",
    "targetId", "targetName", "outcomeId", "outcomeName", "meta",
    "unblind",
    "calibratedRr", "calibratedLb95", "calibratedUb95", "calibratedPValue",
    "numPersons", "timeAtRiskExposed", "timeAtRiskUnexposed",
    "numOutcomesExposed", "numOutcomesUnexposed", "numExposures"
  )

  if (is.null(estimation) || nrow(estimation) == 0) {
    estimation <- data.frame()
  }
  if (is.null(meta) || nrow(meta) == 0) {
    meta <- data.frame()
  }

  if (nrow(estimation) > 0) {
    estimation$meta <- 0
  }

  if (nrow(meta) > 0) {
    meta$meta <- 1
    meta$databaseId <- as.character(meta$databaseId)
    # convert the evidence synthesis column names to the per database column
    # names used by getSccEstimation
    if (!"calibratedLb95" %in% colnames(meta) &&
        "calibratedCi95Lb" %in% colnames(meta)) {
      meta$calibratedLb95 <- meta$calibratedCi95Lb
    }
    if (!"calibratedUb95" %in% colnames(meta) &&
        "calibratedCi95Ub" %in% colnames(meta)) {
      meta$calibratedUb95 <- meta$calibratedCi95Ub
    }
    if (!"calibratedPValue" %in% colnames(meta)) {
      meta$calibratedPValue <- if ("calibratedP" %in% colnames(meta)) {
        meta$calibratedP
      } else {
        NA
      }
    }
  }

  estimation <- estimation[, intersect(commonCols, colnames(estimation)), drop = FALSE]
  meta <- meta[, intersect(commonCols, colnames(meta)), drop = FALSE]

  combined <- dplyr::bind_rows(estimation, meta)
  # collapse any fully duplicated rows
  combined <- dplyr::distinct(combined)
  return(combined)
}

#' The column definitions for the detailed pair results table
#'
#' @details
#' Only the calibrated estimates are shown (together with the analysis
#' description so the results from different study parameter settings can be
#' distinguished)
#'
#' @family SelfControlledCohort
#' @return
#' A named list of reactable::colDef
#' @export
selfControlledCohortDetailedColDef <- function() {
  results <- list(
    databaseId = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(show = FALSE),
    analysisId = reactable::colDef(show = FALSE),
    meta = reactable::colDef(show = FALSE),
    targetName = reactable::colDef(show = FALSE),
    outcomeName = reactable::colDef(show = FALSE),
    numPersons = reactable::colDef(show = FALSE),
    timeAtRiskExposed = reactable::colDef(show = FALSE),
    timeAtRiskUnexposed = reactable::colDef(show = FALSE),
    numOutcomesExposed = reactable::colDef(show = FALSE),
    numOutcomesUnexposed = reactable::colDef(show = FALSE),
    numExposures = reactable::colDef(show = FALSE),

    databaseName = reactable::colDef(
      name = "Data source",
      filterable = TRUE,
      minWidth = 200
    ),
    description = reactable::colDef(
      name = "Analysis settings",
      filterable = TRUE,
      minWidth = 220
    ),
    unblind = reactable::colDef(
      name = "Unblinded",
      filterable = TRUE,
      minWidth = 90,
      cell = function(value) {
        if (is.na(value)) "-" else if (value == 1) "Yes" else "No"
      }
    ),
    calibratedRr = reactable::colDef(
      name = "Calibrated IRR",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    calibratedLb95 = reactable::colDef(
      name = "Calibrated LB",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    calibratedUb95 = reactable::colDef(
      name = "Calibrated UB",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    calibratedPValue = reactable::colDef(
      name = "Calibrated P",
      format = reactable::colFormat(digits = 4),
      na = "-"
    )
  )
  return(results)
}

#' The column definitions for the per database study diagnostics table
#'
#' @details
#' The diagnostics come from the scc_diagnostics_summary table (MDRR, EASE,
#' pre exposure tests etc) and the summaryValue column reflects whether the
#' pair passed the diagnostics in each database
#'
#' @family SelfControlledCohort
#' @return
#' A named list of reactable::colDef
#' @export
selfControlledCohortDiagnosticsColDef <- function() {
  results <- list(
    databaseId = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(show = FALSE),
    analysisId = reactable::colDef(show = FALSE),
    targetName = reactable::colDef(show = FALSE),
    outcomeName = reactable::colDef(show = FALSE),
    unblindForCalibration = reactable::colDef(show = FALSE),
    eventDependentObservation = reactable::colDef(show = FALSE),

    databaseName = reactable::colDef(
      name = "Data source",
      filterable = TRUE,
      minWidth = 200
    ),
    description = reactable::colDef(
      name = "Analysis settings",
      filterable = TRUE,
      minWidth = 220
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
    preExposurePValue = reactable::colDef(
      name = "Pre-exposure P",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    preExposureRateRatio = reactable::colDef(
      name = "Pre-exposure RR",
      format = reactable::colFormat(digits = 2),
      na = "-"
    ),
    unblind = reactable::colDef(
      name = "Unblinded",
      filterable = TRUE,
      minWidth = 90,
      cell = function(value) {
        if (is.na(value)) "-" else if (value == 1) "Yes" else "No"
      }
    ),
    summaryValue = reactable::colDef(
      name = "Status",
      filterable = TRUE,
      minWidth = 90
    )
  )
  return(results)
}
