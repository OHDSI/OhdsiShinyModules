# @file cohort-method-subgroups
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


#' The module viewer for rendering the PLE subgroup results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method subgroup results module
#' 
#' @export
cohortMethodSubgroupsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::uiOutput(outputId = ns("subgroupTableCaption")),
    DT::dataTableOutput(outputId = ns("subgroupTable"))
  )
}


#' The module server for rendering the subgroup results
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param exposureOfInterest exposureOfInterest
#' @param outcomeOfInterest outcomeOfInterest
#' @param connectionHandler connection
#'
#' @return
#' the PLE subgroup results server
#' 
#' @export
cohortMethodSubgroupsServer <- function(id, selectedRow, inputParams, exposureOfInterest, outcomeOfInterest, connectionHandler) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      interactionEffects <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == inputParams()$outcome]
          subgroupResults <- getCohortMethodSubgroupResults(connectionHandler = connectionHandler,
                                                          targetIds = targetId,
                                                          comparatorIds = comparatorId,
                                                          outcomeIds = outcomeId,
                                                          databaseIds = row$databaseId,
                                                          analysisIds = row$analysisId)
          if (nrow(subgroupResults) == 0) {
            return(NULL)
          } else {
            if (!row$unblind) {
              subgroupResults$rrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$ci95Lb <- rep(NA, nrow(subgroupResults))
              subgroupResults$ci95Ub <- rep(NA, nrow(subgroupResults))
              subgroupResults$logRrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$seLogRrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$p <- rep(NA, nrow(subgroupResults))
              subgroupResults$calibratedP <- rep(NA, nrow(subgroupResults))
            }
            return(subgroupResults)
          }
        }
      })
      
      output$subgroupTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 4.</strong> Subgroup interactions. For each subgroup, the number of subject within the subroup
      in the target (<em>%s</em>) and comparator (<em>%s</em>) cohorts are provided, as well as the hazard ratio ratio (HRR)
      with 95 percent confidence interval and p-value (uncalibrated and calibrated) for interaction of the main effect with
      the subgroup."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      output$subgroupTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          subgroupResults <- interactionEffects()
          if (is.null(subgroupResults)) {
            return(NULL)
          }
          subgroupTable <- prepareCohortMethodSubgroupTable(subgroupResults, output = "html")
          colnames(subgroupTable) <- c("Subgroup",
                                       "Target subjects",
                                       "Comparator subjects",
                                       "HRR",
                                       "P",
                                       "Cal.P")
          options <- list(searching = FALSE,
                          ordering = FALSE,
                          paging = FALSE,
                          bInfo = FALSE)
          subgroupTable <- DT::datatable(subgroupTable,
                                         options = options,
                                         rownames = FALSE,
                                         escape = FALSE,
                                         class = "stripe nowrap compact")
          return(subgroupTable)
        }
      })
    }
  )
}
