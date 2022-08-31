# @file estimation-propensityScoreDistribution
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


#' The module viewer for rendering the propensity score distribution
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation propensity score distribution
#' 
#' @export
estimationPropensityScoreDistViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("psDistPlot")),
    div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadPsDistPlotPng"),
                       label = "Download plot as PNG"),
        downloadButton(outputId = ns("downloadPsDistPlotPdf"),
                       label = "Download plot as PDF"))
  )
}


#' The module server for rendering a PLE propensity score distribution
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE propensity score distribution content server
#' 
#' @export
estimationPropensityScoreDistServer <- function(id, selectedRow, inputParams, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      psDistPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (FALSE && row$databaseId %in% metaAnalysisDbIds) {
            #TODO: update once MA implemented
            ps <- getEstimationPs(connection = connection,
                                  targetIds = row$targetId,
                                  comparatorIds = row$comparatorId,
                                  analysisId = row$analysisId)
          } else {
            ps <- getEstimationPs(connection = connection,
                                  resultsSchema = resultsSchema,
                                  targetId = inputParams()$target,
                                  comparatorId = inputParams()$comparator,
                                  analysisId = row$analysisId,
                                  databaseId = row$databaseId)
          }
          if (nrow(ps) == 0) {
            return(NULL) #TODO: handle more gracefully
          }
          plot <- plotEstimationPs(ps, inputParams()$target, inputParams()$comparator)
          return(plot)
        }
      })
      
      output$psDistPlot <- shiny::renderPlot({
        return(psDistPlot())
      })
      
      output$downloadPsDistPlotPng <- shiny::downloadHandler(filename = "Ps.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = psDistPlot(), width = 5, height = 3.5, dpi = 400)
                                                             })
      
      output$downloadPsDistPlotPdf <- shiny::downloadHandler(filename = "Ps.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = psDistPlot(), width = 5, height = 3.5)
                                                             })
      
    }
  )
}
