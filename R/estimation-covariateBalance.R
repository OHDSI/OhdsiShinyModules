# @file estimation-covariateBalance
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


#' The module viewer for rendering the PLE covariate balance analysis
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation covariate balance results
#' 
#' @export
estimationCovariateBalanceViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::conditionalPanel(condition = "output.isMetaAnalysis == false",
                     ns = ns,
                     shiny::uiOutput(outputId = ns("hoverInfoBalanceScatter")),
                     shiny::plotOutput(outputId = ns("balancePlot"),
                                hover = shiny::hoverOpts(id = ns("plotHoverBalanceScatter"), delay = 100, delayType = "debounce")),
                     shiny::uiOutput(outputId = ns("balancePlotCaption")),
                     shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                shiny::downloadButton(outputId = ns("downloadBalancePlotPng"),
                                        label = "Download plot as PNG"),
                                shiny::downloadButton(outputId = ns("downloadBalancePlotPdf"),
                                        label = "Download plot as PDF"))
    ),
    shiny::conditionalPanel(condition = "output.isMetaAnalysis == true",
                     ns = ns,
                     shiny::plotOutput(outputId = ns("balanceSummaryPlot")),
                     shiny::uiOutput(outputId = ns("balanceSummaryPlotCaption")),
                     shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                shiny::downloadButton(outputId = ns("downloadBalanceSummaryPlotPng"),
                                        label = "Download plot as PNG"),
                                shiny::downloadButton(outputId = ns("downloadBalanceSummaryPlotPdf"),
                                        label = "Download plot as PDF")
                     ))
  )
}


#' The module server for rendering the covariate balance plot
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param metaAnalysisDbIds metaAnalysisDbIds
#'
#' @return
#' the PLE covariate balance content server
#' 
#' @export
estimationCovariateBalanceServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix, metaAnalysisDbIds = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      balance <- shiny::reactive({
        row <- selectedRow()
        balance <- getEstimationCovariateBalance(connection = connection,
                                                 resultsSchema = resultsSchema,
                                                 tablePrefix = tablePrefix,
                                                 targetId = inputParams()$target,
                                                 comparatorId = inputParams()$comparator,
                                                 databaseId = row$databaseId,
                                                 analysisId = row$analysisId)
        return(balance)
      })
      
      output$isMetaAnalysis <- shiny::reactive({
        return(FALSE)
        ##TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        return(isMetaAnalysis)
      })
      
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      balancePlot <- shiny::reactive({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          plot <- plotEstimationCovariateBalanceScatterPlot(balance = balance(),
                                                            beforeLabel = "Before propensity score adjustment",
                                                            afterLabel = "After propensity score adjustment")
          return(plot)
        }
      })
      
      output$balancePlot <- shiny::renderPlot({
        return(balancePlot())
      })
      
      output$downloadBalancePlotPng <- shiny::downloadHandler(filename = "Balance.png",
                                                              contentType = "image/png",
                                                              content = function(file) {
                                                                ggplot2::ggsave(file, plot = balancePlot(), width = 4, height = 4, dpi = 400)
                                                              })
      
      output$downloadBalancePlotPdf <- shiny::downloadHandler(filename = "Balance.pdf",
                                                              contentType = "application/pdf",
                                                              content = function(file) {
                                                                ggplot2::ggsave(file = file, plot = balancePlot(), width = 4, height = 4)
                                                              })
      
      output$balancePlotCaption <- shiny::renderUI({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          text <- "<strong>Figure 3.</strong> Covariate balance before and after propensity score adjustment. Each dot represents
      the standardizes difference of means for a single covariate before and after propensity score adjustment on the propensity
      score. Move the mouse arrow over a dot for more details."
          return(shiny::HTML(sprintf(text)))
        }
      })
      
      output$hoverInfoBalanceScatter <- shiny::renderUI({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          hover <- input$plotHoverBalanceScatter
          point <- shiny::nearPoints(balance(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
          if (nrow(point) == 0) {
            return(NULL)
          }
          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:",
                          left_px - 251,
                          "px; top:",
                          top_px - 150,
                          "px; width:500px;")
          beforeMatchingStdDiff <- formatC(point$beforeMatchingStdDiff, digits = 2, format = "f")
          afterMatchingStdDiff <- formatC(point$afterMatchingStdDiff, digits = 2, format = "f")
          shiny::div(
            style = "position: relative; width: 0; height: 0",
            shiny::wellPanel(
              style = style,
              shiny::p(shiny::HTML(paste0("<b> Covariate: </b>", point$covariateName, "<br/>",
                                          "<b> Std. diff before ",tolower(row$psStrategy),": </b>", beforeMatchingStdDiff, "<br/>",
                                          "<b> Std. diff after ",tolower(row$psStrategy),": </b>", afterMatchingStdDiff)))
            )
          )
        }
      })
      
      balanceSummaryPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          balanceSummary <- getEstimationCovariateBalanceSummary(connection = connection,
                                                                 targetId = row$targetId,
                                                                 comparatorId = row$comparatorId,
                                                                 analysisId = row$analysisId,
                                                                 beforeLabel = paste("Before", row$psStrategy),
                                                                 afterLabel = paste("After", row$psStrategy))
          plot <- plotEstimationCovariateBalanceSummary(balanceSummary,
                                                        threshold = 0.1,
                                                        beforeLabel = paste("Before", row$psStrategy),
                                                        afterLabel = paste("After", row$psStrategy))
          return(plot)
        }
      })
      
      output$balanceSummaryPlot <- shiny::renderPlot({
        balanceSummaryPlot()
      }, res = 100)
      
      output$balanceSummaryPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 7.</strong> Covariate balance before and after %s. The y axis represents
      the standardized difference of mean before and after %s on the propensity
      score. The whiskers show the minimum and maximum values across covariates. The box represents the
      interquartile range, and the middle line represents the median. The dashed lines indicate a standardized
      difference of 0.1."
          return(shiny::HTML(sprintf(text, row$psStrategy, row$psStrategy)))
        }
      })
      
      output$downloadBalanceSummaryPlotPng <- shiny::downloadHandler(filename = "BalanceSummary.png",
                                                                     contentType = "image/png",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file, plot = balanceSummaryPlot(), width = 12, height = 5.5, dpi = 400)
                                                                     })
      
      output$downloadBalanceSummaryPlotPdf <- shiny::downloadHandler(filename = "BalanceSummary.pdf",
                                                                     contentType = "application/pdf",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file = file, plot = balanceSummaryPlot(), width = 12, height = 5.5)
                                                                     })
      
    }
  )
}
