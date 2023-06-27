# @file sccs-diagnosticsSummary
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


#' The module viewer for rendering the SCCS diagnostics results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation diagnostics viewer
#' 
#' @export
sccsDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    # div(HTML("<em>Enhancements to come...</em>")),
    reactable::reactableOutput(outputId = ns("diagnosticsTable"))
  )
}


#' The module server for rendering the SCCS diagnostics summary
#'
#' @param id the unique reference id for the module
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings the resultDatabaseSettings with the schemas, prefix and table names
#'
#' @return
#' the SCCS diagnostics summary results
#' 
#' @export
sccsDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$diagnosticsTable <- reactable::renderReactable({
        data <- getSccsAllDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
        
        reactable::reactable(data,
                             striped = TRUE,
                             filterable = TRUE,
                             searchable = TRUE,
                             bordered = TRUE
        )
      })
      
    }
  )
}