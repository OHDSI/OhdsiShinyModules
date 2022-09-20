# @file estimation-attrition
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

#' The module viewer for rendering the PLE attrition results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation attrition
#' 
#' @export
estimationAttritionViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("attritionPlot"), width = 600, height = 600),
    uiOutput(outputId = ns("attritionPlotCaption")),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadAttritionPlotPng"),
                       label = "Download diagram as PNG"),
        downloadButton(outputId = ns("downloadAttritionPlotPdf"),
                       label = "Download diagram as PDF"))
  )
}

#' The module server for rendering the PLE attrition results
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE attrition results content server
#' 
#' @export
estimationAttritionServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix, databaseTable) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      attritionPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          attrition <- getEstimationAttrition(connection = connection,
                                              resultsSchema = resultsSchema,
                                              tablePrefix = tablePrefix,
                                              databaseTable = databaseTable,
                                              targetId = inputParams()$target,
                                              comparatorId = inputParams()$comparator,
                                              outcomeId = inputParams()$outcome,
                                              databaseId = row$databaseId,
                                              analysisId = row$analysisId)
          plot <- drawEstimationAttritionDiagram(attrition)
          return(plot)
        }
      })
      
      output$attritionPlot <- shiny::renderPlot({
        return(attritionPlot())
      })
      
      output$downloadAttritionPlotPng <- shiny::downloadHandler(filename = "Attrition.png",
                                                                contentType = "image/png",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file, plot = attritionPlot(), width = 6, height = 7, dpi = 400)
                                                                })
      
      output$downloadAttritionPlotPdf <- shiny::downloadHandler(filename = "Attrition.pdf",
                                                                contentType = "application/pdf",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file = file, plot = attritionPlot(), width = 6, height = 7)
                                                                })
      
      output$attritionPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 1.</strong> Attrition diagram, showing the Number of subjects in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after various stages in the analysis."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
    }
  )
}
