# @file estimation-propensityModel
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


#' The module viewer for rendering the PLE propensity score model covariates/coefficients
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation propensity score model covariates/coefficients
#' 
#' @export
estimationPropensityModelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::div(shiny::strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
    DT::dataTableOutput(outputId = ns("propensityModelTable"))
  )
}


#' The module server for rendering the propensity score model
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connection the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#'
#' @return
#' the PLE propensity score model
#' 
#' @export
estimationPropensityModelServer <- function(id, selectedRow, inputParams, connection, resultsSchema, tablePrefix) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$propensityModelTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          model <- getEstimationPropensityModel(connection = connection,
                                                resultsSchema = resultsSchema,
                                                tablePrefix = tablePrefix,
                                                targetId = inputParams()$target,
                                                comparatorId = inputParams()$comparator,
                                                databaseId = row$databaseId,
                                                analysisId = row$analysisId)
          
          table <- prepareEstimationPropensityModelTable(model)
          options = list(columnDefs = list(list(className = 'dt-right',  targets = 0)),
                         pageLength = 15,
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
          return(table)
        }
      })
      
    }
  )
}
