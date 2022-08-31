# @file estimation-kaplainMeier
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


#' The module viewer for rendering the PLE Kaplan Meier curve
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The module viewer for Kaplan Meier objects
#' 
#' @export
estimationKaplanMeierViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("kaplanMeierPlot"), height = 550),
    uiOutput(outputId = ns("kaplanMeierPlotPlotCaption")),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadKaplanMeierPlotPng"),
                       label = "Download plot as PNG"),
        downloadButton(outputId = ns("downloadKaplanMeierPlotPdf"),
                       label = "Download plot as PDF"))
  )
}

#' The module server for rendering the Kaplan Meier curve
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE Kaplain Meier content server
#' 
#' @export
estimationKaplanMeierServer <- function(id, selectedRow, inputParams, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$isMetaAnalysis <- shiny::reactive({
        #TODO: update once MA implemented
        return(FALSE)
        row <- selectedRow()
        isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        return(isMetaAnalysis)
      })
      
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      kaplanMeierPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          km <- getEstimationKaplanMeier(connection = connection,
                                         resultsSchema = resultsSchema,
                                         targetId = inputParams()$target,
                                         comparatorId = inputParams()$comparator,
                                         outcomeId = inputParams()$outcome,
                                         databaseId = row$databaseId,
                                         analysisId = row$analysisId)
          plot <- plotEstimationKaplanMeier(kaplanMeier = km,
                                            targetName = inputParams()$target,
                                            comparatorName = inputParams()$comparator)
          return(plot)
        }
      })
      
      output$kaplanMeierPlot <- shiny::renderPlot({
        return(kaplanMeierPlot())
      }, res = 100)
      
      output$downloadKaplanMeierPlotPng <- shiny::downloadHandler(filename = "KaplanMeier.png",
                                                                  contentType = "image/png",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file, plot = kaplanMeierPlot(), width = 7, height = 5, dpi = 400)
                                                                  })
      
      output$downloadKaplanMeierPlotPdf <- shiny::downloadHandler(filename = "KaplanMeier.pdf",
                                                                  contentType = "application/pdf",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file = file, plot = kaplanMeierPlot(), width = 7, height = 5)
                                                                  })
      
      output$kaplanMeierPlotPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 5.</strong> Kaplan Meier plot, showing survival as a function of time. This plot
      is adjusted using the propensity score: The target curve (<em>%s</em>) shows the actual observed survival. The
      comparator curve (<em>%s</em>) applies reweighting to approximate the counterfactual of what the target survival
      would look like had the target cohort been exposed to the comparator instead. The shaded area denotes
      the 95 percent confidence interval."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      
    }
  )
}
