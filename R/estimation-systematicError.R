# @file estimation-systematicError
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


#' The module viewer for rendering the PLE systematic error objects
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation systematic error module
#' 
#' @export
estimationSystematicErrorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("systematicErrorPlot")),
    div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadSystematicErrorPlotPng"),
                       label = "Download plot as PNG"),
        downloadButton(outputId = ns("downloadSystematicErrorPlotPdf"),
                       label = "Download plot as PDF")
    ),
    conditionalPanel(condition = "output.isMetaAnalysis == true",
                     ns = ns,
                     plotOutput(outputId = ns("systematicErrorSummaryPlot")),
                     div(strong("Figure 8."),"Fitted null distributions per data source."),
                     div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                         downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPng"),
                                        label = "Download plot as PNG"),
                         downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPdf"),
                                        label = "Download plot as PDF")))
  )
}



#' The module server for rendering the systematic error objects
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE systematic error content server
#' 
#' @export
estimationSystematicErrorServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$isMetaAnalysis <- shiny::reactive({
        return(FALSE)
        # TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        return(isMetaAnalysis)
      })
      
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      
      
      systematicErrorPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          controlResults <- getEstimationControlResults(connection = connection,
                                                        resultsSchema = resultsSchema,
                                                        tablePrefix = tablePrefix,
                                                        targetId = inputParams()$target,
                                                        comparatorId = inputParams()$comparator,
                                                        analysisId = row$analysisId,
                                                        databaseId = row$databaseId)
          
          plot <- plotEstimationScatter(controlResults)
          return(plot)
        }
      })
      
      output$systematicErrorPlot <- shiny::renderPlot({
        return(systematicErrorPlot())
      })
      
      output$downloadSystematicErrorPlotPng <- shiny::downloadHandler(filename = "SystematicError.png",
                                                                      contentType = "image/png",
                                                                      content = function(file) {
                                                                        ggplot2::ggsave(file, plot = systematicErrorPlot(), width = 12, height = 5.5, dpi = 400)
                                                                      })
      
      output$downloadSystematicErrorPlotPdf <- shiny::downloadHandler(filename = "SystematicError.pdf",
                                                                      contentType = "application/pdf",
                                                                      content = function(file) {
                                                                        ggplot2::ggsave(file = file, plot = systematicErrorPlot(), width = 12, height = 5.5)
                                                                      })
      
      systematicErrorSummaryPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          negativeControls <- getEstimationNegativeControlEstimates(connection = connection,
                                                                    resultsSchema = resultsSchema,
                                                                    targetId = inputParams()$target,
                                                                    comparatorId = inputParams()$comparator,
                                                                    analysisId =  row$analysisId)
          if (is.null(negativeControls))
            return(NULL)
          
          plot <- plotEstimationEmpiricalNulls(negativeControls)
          return(plot)
        }
      })
      
      output$systematicErrorSummaryPlot <- shiny::renderPlot({
        return(systematicErrorSummaryPlot())
      }, res = 100)
      
      output$downloadSystematicErrorSummaryPlotPng <- shiny::downloadHandler(filename = "SystematicErrorSummary.png",
                                                                             contentType = "image/png",
                                                                             content = function(file) {
                                                                               ggplot2::ggsave(file, plot = systematicErrorSummaryPlot(), width = 12, height = 5.5, dpi = 400)
                                                                             })
      
      output$downloadSystematicErrorSummaryPlotPdf <- shiny::downloadHandler(filename = "SystematicErrorSummary.pdf",
                                                                             contentType = "application/pdf",
                                                                             content = function(file) {
                                                                               ggplot2::ggsave(file = file, plot = systematicErrorSummaryPlot(), width = 12, height = 5.5)
                                                                             })
      
      
    }
  )
}
