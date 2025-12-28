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


#' The module viewer for exploring treatment patterns sunburst plots
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
treatmentPatternsOverviewViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::helpText("View sunburst plots for target cohorts across databases"),
    shinydashboard::box(
      collapsible = TRUE,
      title = NULL,
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    shiny::conditionalPanel(
      condition = "output.showSunburst != 0",
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      shiny::uiOutput(ns("sunburstPlot"))
    )
  )
}


treatmentPatternsOverviewServer <- function(
  id,
  connectionHandler,
  resultDatabaseSettings,
  reactiveTargetRow
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      #---- states ----
      generateIcon <- shiny::reactiveVal(NULL)
      showSunburst <- shiny::reactiveVal(0)
      pathwayTable <- shiny::reactiveVal(NULL)

      #---- selection ui ----
      output$inputs <- shiny::renderUI({
        req(reactiveTargetRow())

        databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseName, split = ", ")))

        shiny::div(
          shinyWidgets::pickerInput(
            inputId = session$ns("databaseNames"),
            label = "Database: ",
            choices = unique(databaseNames()),
            selected = databaseNames()[1],
            multiple = T,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          shiny::actionButton(
            inputId = session$ns("generate"),
            icon = shiny::icon(generateIcon()),
            label = "Generate"
          )
        )
      })

      #---- Conditional Panel ----
      output$showSunburst <- shiny::reactive({
        showSunburst()
      })

      shiny::outputOptions(output, "showSunburst", suspendWhenHidden = FALSE)

      shiny::observeEvent(reactiveTargetRow(),
        {
          showSunburst(0)
          pathwayTable(NULL)
          generateIcon(NULL)
        },
        ignoreInit = TRUE
      )

      analysisGroup <- shiny::reactive({
        req(reactiveTargetRow())
        reactiveTargetRow() %>%
          dplyr::select("analysisId", "targetCohortName") %>%
          dplyr::distinct()
      })
      
      #---- generate click: fetch pathways ----
      inputSelectionDfServer(
        id = 'inputSelected', 
        dataFrameRow = selected,
        ncol = 1
      )
      
      selected <- shiny::reactiveVal(value = NULL)
      
      shiny::observeEvent(input$generate, {
        if (is.null(input$databaseNames) | is.null(reactiveTargetRow())) {
          output$showDatabase <- shiny::reactive(0)

          shiny::showNotification("No databases selected")
        } else {
          showSunburst(1)

          generateIcon("redo")
          
          pathwayTable(
            OhdsiReportGenerator::getTreatmentPathways(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              tpTablePrefix = resultDatabaseSettings$tpTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              targetIds = unique(reactiveTargetRow()$targetCohortId),
              analysisIds = unique(reactiveTargetRow()$analysisId),
              databaseNames = input$databaseNames
            )
          )
          
          selected(
            data.frame(
              Analyses = paste(unique(reactiveTargetRow()$analysisId), collapse = ", "),
              Targets = paste(unique(reactiveTargetRow()$targetCohortName), collapse = ", "),
              Databases = paste(input$databaseNames, collapse =", ")
            )
          )
        }
      })

      #---- render sunburst ui ----
      output$sunburstPlot <- shiny::renderUI({
        req(showSunburst() != 0)
        req(!is.null(analysisGroup()) && nrow(analysisGroup()) > 0)

        UIList <- lapply(seq_len(nrow(analysisGroup())), function(idx) {
          analysis <- analysisGroup()[idx, ]$analysisId
          target <- analysisGroup()[idx, ]$targetCohortName

          id <- paste0("sunburstUI", "_", idx)

          shinydashboard::box(
            collapsible = TRUE,
            status = "primary",
            solidHeader = TRUE,
            title = paste("Analysis:", analysis, "Target:", target),
            width = 12,
            sunburstPlotViewer(session$ns(id))
          )
        })

        shiny::tagList(UIList)
      })

      #---- render serve ----#
      shiny::observeEvent(pathwayTable(), {
        req(showSunburst() != 0)
        req(!is.null(pathwayTable()))
        req(!is.null(analysisGroup()) && nrow(analysisGroup()) > 0)

        lapply(seq_len(nrow(analysisGroup())), function(idx) {
          local({
            analysis <- analysisGroup()[idx, ]$analysisId
            target <- analysisGroup()[idx, ]$targetCohortName

            pathway <- pathwayTable() %>% dplyr::filter(analysisId == analysis, targetName == target)

            id <- paste0("sunburstUI", "_", idx)

            sunburstPlotServer(
              id = id,
              pathwayTable = pathway,
              plotWidth = "100%",
              plotHeight = "600px",
              sunburstList = input$databaseNames,
              filterColumn = "databaseName",
              filenamePrefix = paste0(analysis, "_", target)
            )
          })
        })
      })
    }
  )
}