# @file selfControlledCohort-main.R
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

#' The location of the self controlled cohort module helper file
#'
#' @details
#' Returns the location of the self controlled cohort helper file
#'
#' @family SelfControlledCohort
#' @return
#' string location of the self controlled cohort helper file
#'
#' @export
selfControlledCohortHelperFile <- function() {
  fileLoc <- system.file(
    "self-controlled-cohort-www",
    "selfControlledCohort.html",
    package = "OhdsiShinyModules"
  )
  return(fileLoc)
}

#' The module viewer for exploring self controlled cohort results
#'
#' @details
#' The user specifies the id for the module.  The module provides a signal
#' discovery view (all exposure-outcome pairs with the number of databases
#' showing a benefit/risk and the meta analytic estimate) and a pair explorer
#' for a single exposure-outcome pair (detailed results, forest plot,
#' systematic error and time at risk / time to outcome boxplots)
#'
#' @param id the unique reference id for the module
#' @family SelfControlledCohort
#' @return
#' The user interface to the self controlled cohort viewer module
#'
#' @export
selfControlledCohortViewer <- function(id = "selfControlledCohort") {
  ns <- shiny::NS(id)

  shinydashboard::box(
    status = "info",
    width = "100%",
    title = shiny::span(shiny::icon("magnifying-glass-chart"), "Self Controlled Cohort Viewer"),
    solidHeader = TRUE,

    shiny::tabsetPanel(
      type = "pills",
      id = ns("mainTabset"),

      shiny::tabPanel(
        title = "Signal discovery",
        value = "signals",
        selfControlledCohortSignalsViewer(ns("signals"))
      ),

      shiny::tabPanel(
        title = "Pair explorer",
        value = "pair",
        selfControlledCohortPairViewer(ns("pair"))
      )
    )
  )
}

#' The module server for exploring self controlled cohort results
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the self controlled cohort
#'   result schema and table prefixes (schema, sccTablePrefix, esTablePrefix,
#'   cgTablePrefix and databaseTable)
#' @family SelfControlledCohort
#' @return
#' The server for the self controlled cohort module
#'
#' @export
selfControlledCohortServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      selectedPair <- shiny::reactiveVal(value = NULL)

      selfControlledCohortSignalsServer(
        id = "signals",
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        selectedPair = selectedPair
      )

      selfControlledCohortPairServer(
        id = "pair",
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        selectedPair = selectedPair
      )

      # when a pair is selected in the signal discovery view switch to the
      # pair explorer
      shiny::observeEvent(selectedPair(), {
        if (!is.null(selectedPair())) {
          shiny::updateTabsetPanel(session, "mainTabset", selected = "pair")
        }
      })
    }
  )
}
