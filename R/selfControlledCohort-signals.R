# @file selfControlledCohort-signals.R
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

#' The module viewer for the self controlled cohort signal discovery grid
#'
#' @param id the unique reference id for the module
#' @family SelfControlledCohort
#' @return
#' The user interface to the signal discovery grid
#' @export
selfControlledCohortSignalsViewer <- function(id = "signals") {
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
            shiny::numericInput(
              ns("benefitRr"),
              "Maximum benefit RR",
              value = 0.8,
              min = 0.1,
              max = 1,
              step = 0.05
            )
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              ns("riskRr"),
              "Minimum risk RR",
              value = 1.25,
              min = 1,
              max = 5,
              step = 0.05
            )
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              ns("pCut"),
              "P value cut off",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            )
          ),
          shiny::column(
            width = 4,
            shiny::radioButtons(
              ns("filterMode"),
              "Filter results by",
              choices = c(
                "Database benefit counts" = "count",
                "Meta analysis estimate" = "meta"
              ),
              selected = "count"
            )
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              ns("minBenefitSources"),
              "Minimum databases showing a benefit",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              ns("maxRiskSources"),
              "Maximum databases showing a risk",
              value = 100,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          shiny::column(
            width = 4,
            shiny::textInput(ns("targetSearch"), "Search exposure")
          ),
          shiny::column(
            width = 4,
            shiny::textInput(ns("outcomeSearch"), "Search outcome")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
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
      "Select a row to open the exposure-outcome pair in the pair explorer.",
      style = "margin-top: 10px;"
    ),
    resultTableViewer(
      id = ns("signalsTable"),
      boxTitle = "Signal discovery grid"
    )
  )
}

#' The module server for the self controlled cohort signal discovery grid
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list with the result schema and table
#'   prefixes
#' @param selectedPair a reactiveVal used to communicate the selected pair to
#'   the parent module
#' @family SelfControlledCohort
#' @return
#' The server for the signal discovery grid
#' @export
selfControlledCohortSignalsServer <- function(
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
        choices <- if (nrow(df) > 0) stats::setNames(df$analysisId, df$description) else c("No analyses" = 1)
        shiny::selectInput(
          inputId = session$ns("analysisId"),
          label = "Analysis",
          choices = choices,
          selected = if (nrow(df) > 0) df$analysisId[1] else 1
        )
      })

      signalsData <- shiny::eventReactive(input$apply, {
        shiny::req(input$analysisId)
        if (nrow(analyses()) == 0) {
          return(data.frame())
        }
        result <- OhdsiReportGenerator::getSccSignals(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          sccTablePrefix = resultDatabaseSettings$sccTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          esTablePrefix = resultDatabaseSettings$esTablePrefix,
          analysisIds = as.numeric(input$analysisId),
          benefit = as.numeric(input$benefitRr),
          lowerBenefit = 0,
          risk = as.numeric(input$riskRr),
          pValueCut = as.numeric(input$pCut),
          calibrated = TRUE,
          filterByMeta = input$filterMode == "meta",
          minBenefitSources = as.numeric(input$minBenefitSources),
          maxRiskSources = as.numeric(input$maxRiskSources)
        )

        if (nrow(result) > 0) {
          # if more than one evidence synthesis analysis contributes rows for a
          # pair, show the pair only once
          result <- result |>
            dplyr::distinct(.data$targetId, .data$outcomeId, .keep_all = TRUE)

          if (nzchar(input$targetSearch)) {
            result <- result |>
              dplyr::filter(grepl(
                pattern = input$targetSearch,
                x = .data$targetName,
                ignore.case = TRUE
              ))
          }
          if (nzchar(input$outcomeSearch)) {
            result <- result |>
              dplyr::filter(grepl(
                pattern = input$outcomeSearch,
                x = .data$outcomeName,
                ignore.case = TRUE
              ))
          }
        }
        return(result)
      })

      # only display results once the apply filters button has been pressed
      signalsTableData <- shiny::reactive({
        if (is.null(input$apply) || input$apply == 0) {
          return(data.frame())
        }
        return(signalsData())
      })

      resultTableOutputs <- resultTableServer(
        id = "signalsTable",
        df = signalsTableData,
        colDefsInput = selfControlledCohortSignalsColDef(),
        addActions = list(
          createActionButton(
            actionType = "openPair",
            buttonIcon = "magnifying-glass-chart",
            hoverText = "Open this pair in the pair explorer",
            buttonLabel = "Open",
            buttonClass = "btn btn-xs",
            buttonStyle = actionButtonStyleInfo()
          )
        ),
        elementId = session$ns("signalsTable")
      )

      shiny::observeEvent(resultTableOutputs$actionCount(), {
        actionInfo <- resultTableOutputs$actionIndex()
        actionRow <- if (!is.null(actionInfo) && !is.null(actionInfo$index)) {
          actionInfo$index
        } else {
          NA
        }

        data <- signalsTableData()
        if (resultTableOutputs$actionType() == "openPair" &&
            !is.na(actionRow) && actionRow > 0 &&
            !is.null(data) && nrow(data) >= actionRow) {
          selectedPair(data[actionRow, ])
        }
      })
    }
  )
}

#' The column definitions for the signal discovery grid
#'
#' @family SelfControlledCohort
#' @return
#' A named list of reactable::colDef
#' @export
selfControlledCohortSignalsColDef <- function() {
  results <- list(
    targetId = reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(show = FALSE),
    targetName = reactable::colDef(
      name = "Exposure",
      filterable = TRUE,
      minWidth = 200
    ),
    outcomeName = reactable::colDef(
      name = "Outcome",
      filterable = TRUE,
      minWidth = 200
    ),
    benefitCount = reactable::colDef(
      name = "Databases showing benefit",
      format = reactable::colFormat(digits = 0)
    ),
    riskCount = reactable::colDef(
      name = "Databases showing risk",
      format = reactable::colFormat(digits = 0)
    ),
    requiredBenefitCount = reactable::colDef(
      name = "Required databases showing benefit",
      show = FALSE
    ),
    metaRr = reactable::colDef(
      name = "Meta RR",
      format = reactable::colFormat(digits = 2),
      na = "-"
    ),
    metaP = reactable::colDef(
      name = "Meta p",
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    i2 = reactable::colDef(
      name = "I2",
      format = reactable::colFormat(digits = 2),
      na = "-"
    ),
    nDatabases = reactable::colDef(
      name = "Databases in meta",
      format = reactable::colFormat(digits = 0),
      na = "-"
    )
  )
  return(results)
}
