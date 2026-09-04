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
      "Results are loaded from the database in pages.  Change the filters and
      press 'Apply filters' to update the table.  Click 'Open' on a row to
      explore that exposure-outcome pair in the pair explorer.",
      style = "margin-top: 10px;"
    ),
    largeTableView(
      id = ns("signalsTable"),
      selectedPageSize = 25
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

      # the parameters that are passed to the (server side paginated) query.
      # They are only refreshed when the apply filters button is pressed so
      # changing a filter does not trigger a query
      appliedParams <- shiny::reactiveVal(list(
        schema = resultDatabaseSettings$schema,
        scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
        cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
        es_table_prefix = resultDatabaseSettings$esTablePrefix,
        analysis_id = 1,
        benefit_rr = 0.8,
        lower_benefit_rr = 0,
        risk_rr = 1.25,
        p_cut = 0.05,
        filter_by_meta = 0,
        min_benefit_sources = 0,
        max_risk_sources = 100,
        target_search = "",
        outcome_search = ""
      ))

      setDefaultParams <- function() {
        appliedParams(list(
          schema = resultDatabaseSettings$schema,
          scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
          cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
          es_table_prefix = resultDatabaseSettings$esTablePrefix,
          analysis_id = if (nrow(analyses()) > 0) analyses()$analysisId[1] else 1,
          benefit_rr = 0.8,
          lower_benefit_rr = 0,
          risk_rr = 1.25,
          p_cut = 0.05,
          filter_by_meta = 0,
          min_benefit_sources = 0,
          max_risk_sources = 100,
          target_search = "",
          outcome_search = ""
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
        appliedParams(list(
          schema = resultDatabaseSettings$schema,
          scc_table_prefix = resultDatabaseSettings$sccTablePrefix,
          cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
          es_table_prefix = resultDatabaseSettings$esTablePrefix,
          analysis_id = as.numeric(input$analysisId),
          benefit_rr = as.numeric(input$benefitRr),
          lower_benefit_rr = 0,
          risk_rr = as.numeric(input$riskRr),
          p_cut = as.numeric(input$pCut),
          filter_by_meta = ifelse(input$filterMode == "meta", 1, 0),
          min_benefit_sources = as.numeric(input$minBenefitSources),
          max_risk_sources = as.numeric(input$maxRiskSources),
          target_search = input$targetSearch,
          outcome_search = input$outcomeSearch
        ))
      })

      baseSql <- selfControlledCohortSignalsSql(
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
      columns <- selfControlledCohortSignalsColDef()
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
        id = "signalsTable",
        ldt = ldt,
        inputParams = appliedParams,
        columns = shiny::reactive(columns)
      )
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
