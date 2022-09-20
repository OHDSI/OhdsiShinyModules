# @file estimation-populationCharacteristics
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


#' The module viewer for rendering the PLE population characteristics
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation population characteristics objects
#' 
#' @export
estimationPopulationCharacteristicsViewer <- function(id) {
  
  ns <- shiny::NS(id)
  shiny::div(
    uiOutput(outputId = ns("table1Caption")),
    DT::dataTableOutput(outputId = ns("table1Table"))
  )
}


#' The module server for rendering the population characteristics
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE population characteristics content server
#' 
#' @export
estimationPopulationCharacteristicsServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$table1Caption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 2.</strong> Select characteristics before and after propensity score adjustment, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      output$table1Table <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          balance <- getEstimationCovariateBalance(connection = connection,
                                                   resultsSchema = resultsSchema,
                                                   tablePrefix = tablePrefix,
                                                   targetId = inputParams()$target,
                                                   comparatorId = inputParams()$comparator,
                                                   outcomeId = inputParams()$outcome,
                                                   databaseId = row$databaseId,
                                                   analysisId = row$analysisId)
          if (nrow(balance) == 0) {
            return(NULL)
          }
          table1 <- prepareEstimationTable1(balance = balance,
                                            beforeLabel = paste("Before PS adjustment"),
                                            afterLabel = paste("After PS adjustment"))
          
          container <- htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 3, "Characteristic"),
                th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
                th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
              ),
              tr(
                lapply(table1[1, 2:ncol(table1)], th)
              ),
              tr(
                lapply(table1[2, 2:ncol(table1)], th)
              )
            )
          ))
          options <- list(columnDefs = list(list(className = 'dt-right',  targets = 1:6)),
                          searching = FALSE,
                          ordering = FALSE,
                          paging = FALSE,
                          bInfo = FALSE)
          table1 <- DT::datatable(table1[3:nrow(table1), ],
                                  options = options,
                                  rownames = FALSE,
                                  escape = FALSE,
                                  container = container,
                                  class = "stripe nowrap compact")
          return(table1)
        }
      })
    }
  )
}

