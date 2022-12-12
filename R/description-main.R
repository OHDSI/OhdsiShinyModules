# @file description-main.R
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


#' The location of the description module helper file
#'
#' @details
#' Returns the location of the description helper file
#' 
#' @return
#' string location of the description helper file
#'
#' @export
descriptionHelperFile <- function(){
  fileLoc <- system.file('description-www', "description.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring description studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description viewer module
#'
#' @export
descriptionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    id = ns('mainPanel'),
    
    shiny::tabPanel(
      "Target Viewer",  
      descriptionTableViewer(ns('descriptiveTableTab'))
    ),
    
    shiny::tabPanel(
      "Outcome Stratified",  
      descriptionAggregateFeaturesViewer(ns('aggregateFeaturesTab'))
    ),
    
    shiny::tabPanel(
      "Incidence Rate",  
      descriptionIncidenceViewer(ns('incidenceTab'))
    ),
    
    shiny::tabPanel(
      "Time To Event",  
      descriptionTimeToEventViewer(ns('timeToEventTab'))
    ),
    
    shiny::tabPanel(
      "Dechallenge Rechallenge",
      descriptionDechallengeRechallengeViewer(ns('dechallengeRechallengeTab'))
      )
    )
  
}

#' The module server for exploring description studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#' 
#' @return
#' The server for the description module
#'
#' @export
descriptionServer <- function(
  id, 
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # =============================
      #   CONNECTION
      # =============================
      if(F){
        if(resultDatabaseSettings$port != ""){
          ParallelLogger::logInfo('Port')
          ParallelLogger::logInfo(paste(resultDatabaseSettings$port))
          con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                              dbms = resultDatabaseSettings$dbms,
                              server = resultDatabaseSettings$server,
                              user = resultDatabaseSettings$user,
                              password = resultDatabaseSettings$password,
                              port = resultDatabaseSettings$port)
          
        } else{
          ParallelLogger::logInfo('No Port')
          con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                              dbms = resultDatabaseSettings$dbms,
                              server = resultDatabaseSettings$server,
                              user = resultDatabaseSettings$user,
                              password = resultDatabaseSettings$password
          )
          
        }
      }
      
      # old connection 
      connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = resultDatabaseSettings$dbms,
        server = resultDatabaseSettings$server,
        user = resultDatabaseSettings$user,
        password = resultDatabaseSettings$password,
        port = resultDatabaseSettings$port
        #pathToDriver =  '/Users/jreps/Documents/drivers'
      )
      
      
      con <- DatabaseConnector::connect(connectionDetails)
      
      
      shiny::onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      mainPanelTab <- shiny::reactiveVal(input$mainPanel)
      shiny::observeEvent(
        input$mainPanel,
        {
          mainPanelTab(input$mainPanel)
        })
      
      
      # =============================
      #   Table of cohorts
      # =============================
      descriptionTableServer(
        id = 'descriptiveTableTab',
        con = con, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        dbms = resultDatabaseSettings$dbms,
        tablePrefix = resultDatabaseSettings$tablePrefix,
        tempEmulationSchema = resultDatabaseSettings$tempEmulationSchema, 
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Aggregrate Features
      # =============================
      
      descriptionAggregateFeaturesServer(
        id = 'aggregateFeaturesTab',
        con = con, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        dbms = resultDatabaseSettings$dbms,
        tablePrefix = resultDatabaseSettings$tablePrefix,
        tempEmulationSchema = resultDatabaseSettings$tempEmulationSchema, 
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
      )
      
      # =============================
      #   Incidence
      # =============================
      descriptionIncidenceServer(
        id = 'incidenceTab',
        con = con, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        dbms = resultDatabaseSettings$dbms,
        incidenceTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
        tempEmulationSchema = resultDatabaseSettings$tempEmulationSchema,
        databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Time To Event
      # =============================
      
      descriptionTimeToEventServer(
          id = 'timeToEventTab', 
          con = con, 
          mainPanelTab = mainPanelTab,
          schema = resultDatabaseSettings$schema, 
          dbms = resultDatabaseSettings$dbms,
          tablePrefix = resultDatabaseSettings$tablePrefix,
          tempEmulationSchema = resultDatabaseSettings$tempEmulationSchema,
          cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
          databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Dechallenge Rechallenge
      # =============================
      
      descriptionDechallengeRechallengeServer(
        id = 'dechallengeRechallengeTab', 
        con = con, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        dbms = resultDatabaseSettings$dbms,
        tablePrefix = resultDatabaseSettings$tablePrefix,
        tempEmulationSchema = resultDatabaseSettings$tempEmulationSchema,
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
      )
      
    }
  )
}
