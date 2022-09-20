# @file estimation-power
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


#' The module viewer for rendering the PLE power analysis
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation power calculation results
#' 
#' @export
estimationPowerViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::uiOutput(outputId = ns("powerTableCaption")),
    shiny::tableOutput(outputId = ns("powerTable")),
    shiny::uiOutput(outputId = ns("timeAtRiskTableCaption")),
    shiny::tableOutput(outputId = ns("timeAtRiskTable"))
  )
}


#' The module server for rendering the PLE power analysis results
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE systematic error power server
#' 
#' @export
estimationPowerServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$powerTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row)) {
          text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$powerTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          #TODO: update once MA implemented
          if (FALSE && row$databaseId %in% metaAnalysisDbIds) {
            results <- getEstimationMainResults(connection = connection,
                                                targetIds = row$targetId,
                                                comparatorIds = row$comparatorId,
                                                outcomeIds = row$outcomeId,
                                                analysisIds = row$analysisId)
            table <- prepareEstimationPowerTable(results, connection, resultsSchema)
            table$description <- NULL
            if (!row$unblind) {
              table$targetOutcomes  <- NA
              table$comparatorOutcomes   <- NA
              table$targetIr   <- NA
              table$comparatorIr   <- NA
            }
            table$databaseId[table$databaseId %in% metaAnalysisDbIds] <- "Summary"
            colnames(table) <- c("Source",
                                 "Target subjects",
                                 "Comparator subjects",
                                 "Target years",
                                 "Comparator years",
                                 "Target events",
                                 "Comparator events",
                                 "Target IR (per 1,000 PY)",
                                 "Comparator IR (per 1,000 PY)",
                                 "MDRR")
          } else {
            table <- prepareEstimationPowerTable(row, connection, resultsSchema, tablePrefix)
            table$description <- NULL
            table$databaseId <- NULL
            if (!row$unblind) {
              table$targetOutcomes  <- NA
              table$comparatorOutcomes   <- NA
              table$targetIr   <- NA
              table$comparatorIr   <- NA
            }
            colnames(table) <- c("Target subjects",
                                 "Comparator subjects",
                                 "Target years",
                                 "Comparator years",
                                 "Target events",
                                 "Comparator events",
                                 "Target IR (per 1,000 PY)",
                                 "Comparator IR (per 1,000 PY)",
                                 "MDRR")
          }
          return(table)
        }
      })
      
      output$timeAtRiskTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row)) {
          text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$timeAtRiskTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (FALSE && row$databaseId %in% metaAnalysisDbIds) {
            # TODO: update when MA implemented
            followUpDist <- getCmFollowUpDist(cmFollowUpDist = cmFollowUpDist,
                                              connection = connection,
                                              targetId = targetId,
                                              comparatorId = comparatorId,
                                              outcomeId = outcomeId,
                                              analysisId = row$analysisId)
          } else {
            followUpDist <- getCmFollowUpDist(connection = connection,
                                              resultsSchema = resultsSchema,
                                              tablePrefix = tablePrefix,
                                              targetId = inputParams()$target,
                                              comparatorId = inputParams()$comparator,
                                              outcomeId = inputParams()$outcome,
                                              databaseId = row$databaseId,
                                              analysisId = row$analysisId)
          }
          table <- prepareEstimationFollowUpDistTable(followUpDist)
          return(table)
        }
      })
    })
}
