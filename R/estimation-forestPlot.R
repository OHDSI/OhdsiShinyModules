# @file estimation-forestPlot
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

#' The module viewer for rendering the PLE results forest plot
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation forest plot
#' 
#' @export
estimationForestPlotViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::plotOutput(outputId = ns("forestPlot")),
    shiny::uiOutput(outputId = ns("forestPlotCaption")),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadForestPlotPng"),
                       label = "Download plot as PNG"),
               shiny::downloadButton(outputId = ns("downloadForestPlotPdf"),
                       label = "Download plot as PDF"))
  )
}





#' The module server for rendering the PLE multiple results forest plot
#'
#' @param id the unique reference id for the module
#' @param connectionHandler connection
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param metaAnalysisDbIds metaAnalysisDbIds
#' @param resultsSchema resultsSchema
#' @param tablePrefix tablePrefix
#' @param databaseTable databaseTable
#'
#' @return
#' the PLE forest plot content server
#' 
#' @export
estimationForestPlotServer <- function(
  id, connectionHandler, selectedRow, inputParams, metaAnalysisDbIds = NULL,
  resultsSchema,
  tablePrefix,
  databaseTable
  ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      forestPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          results <- getEstimationMainResults(connectionHandler = connectionHandler,
                                              resultsSchema = resultsSchema,
                                              tablePrefix = tablePrefix,
                                              databaseTable = databaseTable,
                                              targetIds = row$targetId,
                                              comparatorIds = row$comparatorId,
                                              outcomeIds = row$outcomeId,
                                              analysisIds = row$analysisId)
          plot <- plotEstimationForest(results)
          return(plot)
        }
      })
      
      output$forestPlot <- shiny::renderPlot({
        forestPlot()
      })
      
      output$forestPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 6.</strong> Forest plot showing the per-database and summary hazard ratios (and 95 percent confidence
      intervals) comparing %s to %s for the outcome of %s, using %s. Estimates are shown both before and after empirical
      calibration. The I2 is computed on the uncalibrated estimates."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator, inputParams()$outcome, row$psStrategy)))
        }
      })
      
      output$downloadForestPlotPng <- shiny::downloadHandler(filename = "ForestPlot.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = forestPlot(), width = 12, height = 9, dpi = 400)
                                                             })
      
      output$downloadForestPlotPdf <- shiny::downloadHandler(filename = "ForestPlot.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = forestPlot(), width = 12, height = 9)
                                                             })
      
    }
  )
}
