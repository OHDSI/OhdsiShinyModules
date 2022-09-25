# @file estimation-resultsTable
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





#' The module viewer for rendering the PLE main results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the PLE main results
#' 
#' @export
estimationResultsTableViewer <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(outputId = ns("mainTable"))
}




#' The module server for rendering the PLE results per current selections
#'
#' @param id the unique reference id for the module
#' @param connection the connection to the PLE results database
#' @param inputParams  the selected study parameters of interest
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param databaseTable databaseTable
#'
#' @return
#' the PLE main results table server server
#' 
#' @export
estimationResultsTableServer <- function(
  id, 
  connection, 
  inputParams, 
  resultsSchema, 
  tablePrefix, 
  databaseTable
  ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      mainColumns <- c("description",
                       "cdmSourceAbbreviation",
                       "rr",
                       "ci95Lb",
                       "ci95Ub",
                       "p",
                       "calibratedRr",
                       "calibratedCi95Lb",
                       "calibratedCi95Ub",
                       "calibratedP")
      
      mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>",
                           "<span title=\"Data source\">Data source</span>",
                           "<span title=\"Hazard ratio (uncalibrated)\">HR</span>",
                           "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                           "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>",
                           "<span title=\"Two-sided p-value (uncalibrated)\">P</span>",
                           "<span title=\"Hazard ratio (calibrated)\">Cal.HR</span>",
                           "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                           "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>",
                           "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")
      
      
      
      resultSubset <- shiny::reactive({
        
        results <- getEstimationMainResults(connection = connection,
                                            resultsSchema = resultsSchema,
                                            tablePrefix = tablePrefix,
                                            databaseTable = databaseTable,
                                            targetIds = filterEstimationEmptyNullValues(inputParams()$target),
                                            comparatorIds = filterEstimationEmptyNullValues(inputParams()$comparator),
                                            outcomeIds = filterEstimationEmptyNullValues(inputParams()$outcome),
                                            databaseIds = filterEstimationEmptyNullValues(inputParams()$database),
                                            analysisIds = filterEstimationEmptyNullValues(inputParams()$analysis))
        results <- results[order(results$analysisId), ]
        
        
        results[which(results$unblind == 0), getEstimationColumnsToBlind(results)] <- NA
        
        return(results)
      })
      
      selectedRow <- shiny::reactive({
        idx <- input$mainTable_rows_selected
        if (is.null(idx)) {
          return(NULL)
        } else {
          subset <- resultSubset()
          if (nrow(subset) == 0) {
            return(NULL)
          }
          row <- subset[idx, ]
          # row$psStrategy <- gsub("^PS ", "", gsub(", .*$", "", cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == row$analysisId]))
          return(row)
        }
      })
      
      output$mainTable <- DT::renderDataTable({
        table <- resultSubset()
        if (is.null(table) || nrow(table) == 0) {
          shiny::validate(shiny::need(nrow(table) > 0, "No CM results for selections."))
          return(NULL)
        }
        table <- table[, mainColumns]
        table$rr <- prettyEstimationHr(table$rr)
        table$ci95Lb <- prettyEstimationHr(table$ci95Lb)
        table$ci95Ub <- prettyEstimationHr(table$ci95Ub)
        table$p <- prettyEstimationHr(table$p)
        table$calibratedRr <- prettyEstimationHr(table$calibratedRr)
        table$calibratedCi95Lb <- prettyEstimationHr(table$calibratedCi95Lb)
        table$calibratedCi95Ub <- prettyEstimationHr(table$calibratedCi95Ub)
        table$calibratedP <- prettyEstimationHr(table$calibratedP)
        colnames(table) <- mainColumnNames
        options = list(pageLength = 15,
                       searching = FALSE,
                       lengthChange = TRUE,
                       ordering = TRUE,
                       paging = TRUE)
        selection = list(mode = "single", target = "row")
        table <- DT::datatable(table,
                               options = options,
                               selection = selection,
                               rownames = FALSE,
                               escape = FALSE,
                               class = "stripe nowrap compact")
        table
      })
      
      selectedRow
    })
}
