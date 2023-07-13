# @file cohort-method-resultsTable
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
cohortMethodResultsTableViewer <- function(id) {
  ns <- shiny::NS(id)
  
  reactable::reactableOutput(outputId = ns("mainTable"))
}




#' The module server for rendering the PLE results per current selections
#'
#' @param id the unique reference id for the module
#' @param connectionHandler the connection to the PLE results database
#' @param inputParams  the selected study parameters of interest
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param databaseTable databaseTable
#'
#' @return
#' the PLE main results table server server
#' 
#' @export
cohortMethodResultsTableServer <- function(
  id, 
  connectionHandler, 
  inputParams, 
  resultsSchema, 
  tablePrefix, 
  databaseTable
  ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      
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
      
      resultSubset <- shiny::reactive({
        
        results <- getCohortMethodMainResults(connectionHandler = connectionHandler,
                                            resultsSchema = resultsSchema,
                                            tablePrefix = tablePrefix,
                                            databaseTable = databaseTable,
                                            targetIds = filterCohortMethodEmptyNullValues(inputParams()$target),
                                            comparatorIds = filterCohortMethodEmptyNullValues(inputParams()$comparator),
                                            outcomeIds = filterCohortMethodEmptyNullValues(inputParams()$outcome),
                                            databaseIds = filterCohortMethodEmptyNullValues(inputParams()$database),
                                            analysisIds = filterCohortMethodEmptyNullValues(inputParams()$analysis))
        results <- results[order(results$analysisId), ]
        
        
        results[which(results$unblind == 0), getCohortMethodColumnsToBlind(results)] <- NA
        
        return(results)
      })
      
      selectedRow <- shiny::reactive({
        idx <- reactable::getReactableState(
          outputId = 'mainTable', 
          name = 'selected'
          ) 
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
      
      output$mainTable <- reactable::renderReactable({
        table <- resultSubset()
        if (is.null(table) || nrow(table) == 0) {
          shiny::validate(shiny::need(nrow(table) > 0, "No CM results for selections."))
          return(NULL)
        }
        table <- table[, mainColumns]
        table$rr <- prettyCohortMethodHr(table$rr)
        table$ci95Lb <- prettyCohortMethodHr(table$ci95Lb)
        table$ci95Ub <- prettyCohortMethodHr(table$ci95Ub)
        table$p <- prettyCohortMethodHr(table$p)
        table$calibratedRr <- prettyCohortMethodHr(table$calibratedRr)
        table$calibratedCi95Lb <- prettyCohortMethodHr(table$calibratedCi95Lb)
        table$calibratedCi95Ub <- prettyCohortMethodHr(table$calibratedCi95Ub)
        table$calibratedP <- prettyCohortMethodHr(table$calibratedP)
        #colnames(table) <- mainColumnNames

        reactable::reactable( # add extras
          data = table, 
          rownames = FALSE, 
          defaultPageSize = 15,
          showPageSizeOptions = T, 
          onClick = 'select', 
          selection = 'single',
          striped = T,
          
          columns = list(
            description = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Analysis", 
                "Analysis"
              )),
            cdmSourceAbbreviation = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Data source", 
                "Data source"
              )),
            rr = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "HR", 
                "Hazard ratio (uncalibrated)"
              )),
            ci95Lb = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "LB", 
                "Lower bound of the 95 percent confidence interval (uncalibrated)"
              )),
            ci95Ub = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "UB", 
                "Upper bound of the 95 percent confidence interval (uncalibrated)"
              )),
            p = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "P", 
                "Two-sided p-value (uncalibrated)"
              )),
            calibratedRr = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Cal.HR", 
                "Hazard ratio (calibrated)"
              )),
            calibratedCi95Lb = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Cal.LB", 
                "Lower bound of the 95 percent confidence interval (calibrated)"
              )),
            calibratedCi95Ub = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Cal.UB", 
                "Upper bound of the 95 percent confidence interval (calibrated)"
              )),
            calibratedP = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Cal.P", 
                "Two-sided p-value (calibrated)"
              ))
          )
          )
      })
      
      return(selectedRow)
    })
}
