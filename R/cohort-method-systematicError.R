# @file cohort-method-systematicError
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
#' The user interface to the cohort method systematic error module
#' 
#' @export
cohortMethodSystematicErrorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::plotOutput(outputId = ns("systematicErrorPlot")),
    shiny::div(shiny::strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadSystematicErrorPlotPng"),
                       label = "Download plot as PNG"),
               shiny::downloadButton(outputId = ns("downloadSystematicErrorPlotPdf"),
                       label = "Download plot as PDF")
    ),
    shiny::conditionalPanel(condition = "output.isMetaAnalysis == true",
                     ns = ns,
                     shiny::plotOutput(outputId = ns("systematicErrorSummaryPlot")),
                     shiny::div(shiny::strong("Figure 8."),"Fitted null distributions per data source."),
                     shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                shiny::downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPng"),
                                        label = "Download plot as PNG"),
                                shiny::downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPdf"),
                                        label = "Download plot as PDF")))
  )
}



#' The module server for rendering the systematic error objects
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connectionHandler the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param metaAnalysisDbIds metaAnalysisDbIds
#'
#' @return
#' the PLE systematic error content server
#' 
#' @export
cohortMethodSystematicErrorServer <- function(id, selectedRow, inputParams, connectionHandler, resultsSchema, tablePrefix, metaAnalysisDbIds = NULL) {
  
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
          controlResults <- getCohortMethodControlResults(connectionHandler = connectionHandler,
                                                        resultsSchema = resultsSchema,
                                                        tablePrefix = tablePrefix,
                                                        targetId = inputParams()$target,
                                                        comparatorId = inputParams()$comparator,
                                                        analysisId = row$analysisId,
                                                        databaseId = row$databaseId)
          
          # remove the RR zeros that replace NAs during data upload 
          controlResults$logRr[controlResults$logRr == 0] <- NA
          controlResults$ci95Lb[controlResults$ci95Lb == 0] <- NA
          controlResults$ci95Ub[controlResults$ci95Ub == 0] <- NA
          controlResults$calibratedLogRr[controlResults$calibratedLogRr == 0] <- NA
          controlResults$calibratedCi95Lb[controlResults$calibratedCi95Lb == 0] <- NA
          controlResults$calibratedCi95Ub[controlResults$calibratedCi95Ub == 0] <- NA
          
          plot <- plotCohortMethodScatter(controlResults)
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
          ##negativeControls <- getCohortMethodNegativeControlEstimates(connection = connection,
          ##                                                          #resultsSchema = resultsSchema, unused argument
          ##                                                          targetId = inputParams()$target,
          ##                                                         comparatorId = inputParams()$comparator,
          ##                                                          analysisId =  row$analysisId)
          ##if (is.null(negativeControls))
            return(NULL)
          
          ## plotCohortMethodEmpiricalNulls() not found
          #plot <- plotCohortMethodEmpiricalNulls(negativeControls)
          ##return(plot)
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
