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
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("table"), "Characterization Viewer"),
    solidHeader = TRUE,
  
  shiny::tabsetPanel(
    type = 'pills',
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
  )
  
}

#' The module server for exploring description studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the description result schema, dbms, tablePrefix, databaseTable and cohortTablePrefix
#' 
#' @return
#' The server for the description module
#'
#' @export
descriptionServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
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
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        tablePrefix = resultDatabaseSettings$tablePrefix,
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Aggregrate Features
      # =============================
      
      descriptionAggregateFeaturesServer(
        id = 'aggregateFeaturesTab',
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        tablePrefix = resultDatabaseSettings$tablePrefix,
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
      )
      
      # =============================
      #   Incidence
      # =============================
      descriptionIncidenceServer(
        id = 'incidenceTab',
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        incidenceTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
        databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Time To Event
      # =============================
      
      descriptionTimeToEventServer(
          id = 'timeToEventTab', 
          connectionHandler = connectionHandler, 
          mainPanelTab = mainPanelTab,
          schema = resultDatabaseSettings$schema, 
          tablePrefix = resultDatabaseSettings$tablePrefix,
          cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
          databaseTable = resultDatabaseSettings$databaseTable
        )

      
      # =============================
      #   Dechallenge Rechallenge
      # =============================
      
      descriptionDechallengeRechallengeServer(
        id = 'dechallengeRechallengeTab', 
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        schema = resultDatabaseSettings$schema, 
        tablePrefix = resultDatabaseSettings$tablePrefix,
        cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable
      )
      
    }
  )
}
