# @file data-diagnostic-main.R
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


#' The location of the data-diagnostic module helper file
#'
#' @details
#' Returns the location of the data-diagnostic helper file
#' 
#' @return
#' string location of the data-diagnostic helper file
#'
#' @export
dataDiagnosticHelperFile <- function(){
  fileLoc <- system.file('data-diagnostic-www', "data-diagnostic.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring data-diagnostic
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the data-diagnostic viewer module
#'
#' @export
dataDiagnosticViewer <- function(id = 'dataDiag') {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    header = shiny::h1("Data Diagnostic Explorer"),
    id = ns('main_tabs'),
    type = "pills",
    
    shiny::tabPanel(
      "Summary",  
      dataDiagnosticSummaryViewer(ns('summary-tab'))
    ),
    
    shiny::tabPanel(
      "Drill-Down",  
      dataDiagnosticDrillViewer(ns('drill-down-tab'))
    )
    
  )
  
}

#' The module server for exploring data-diagnostic
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param resultDatabaseSettings a list containing the data-diagnostic result schema and connection details
#' 
#' @return
#' The server for the data-diagnostic module
#'
#' @export
dataDiagnosticServer <- function(
  id = 'dataDiag', 
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # =============================
      #   CONNECTION
      # =============================

      # old connection 
      connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = resultDatabaseSettings$dbms,
        server = resultDatabaseSettings$server,
        user = resultDatabaseSettings$user,
        password = resultDatabaseSettings$password,
        port = resultDatabaseSettings$port
      )
      
      con <- DatabaseConnector::connect(connectionDetails)
      
      shiny::onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      #==========================
      #    SERVERS
      #==========================
      
      dataDiagnosticSummaryServer(
        id = 'summary-tab', 
        con = con, 
        mySchema = resultDatabaseSettings$schema, 
        targetDialect = resultDatabaseSettings$dbms,
        myTableAppend = resultDatabaseSettings$tablePrefix
        )
      
      dataDiagnosticDrillServer(
        id = 'drill-down-tab', 
        con = con, 
        mySchema = resultDatabaseSettings$schema, 
        targetDialect = resultDatabaseSettings$dbms,
        myTableAppend = resultDatabaseSettings$tablePrefix
      )
    
      
    }
  )
}
