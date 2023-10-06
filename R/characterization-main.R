# @file characterization-main.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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


#' The location of the characterization module helper file
#'
#' @details
#' Returns the location of the characterization helper file
#' 
#' @return
#' string location of the characterization helper file
#'
#' @export
characterizationHelperFile <- function(){
  fileLoc <- system.file('characterization-www', "characterization.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring characterization studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the characterization viewer module
#'
#' @export
characterizationViewer <- function(id=1) {
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
      characterizationTableViewer(ns('descriptiveTableTab'))
    ),
    
    shiny::tabPanel(
      "Outcome Stratified",  
      characterizationAggregateFeaturesViewer(ns('aggregateFeaturesTab'))
    ),
    
    shiny::tabPanel(
      "Incidence Rate",  
      characterizationIncidenceViewer(ns('incidenceTab'))
    ),
    
    shiny::tabPanel(
      "Time To Event",  
      characterizationTimeToEventViewer(ns('timeToEventTab'))
    ),
    
    shiny::tabPanel(
      "Dechallenge Rechallenge",
      characterizationDechallengeRechallengeViewer(ns('dechallengeRechallengeTab'))
      )
    )
  )
  
}

#' The module server for exploring characterization studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server for the characterization module
#'
#' @export
characterizationServer <- function(
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
      characterizationTableServer(
        id = 'descriptiveTableTab',
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        resultDatabaseSettings = resultDatabaseSettings
        )

      
      # =============================
      #   Aggregrate Features
      # =============================
      
      characterizationAggregateFeaturesServer(
        id = 'aggregateFeaturesTab',
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        resultDatabaseSettings = resultDatabaseSettings
        )
      
      # =============================
      #   Incidence
      # =============================
      characterizationIncidenceServer(
        id = 'incidenceTab',
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        resultDatabaseSettings = resultDatabaseSettings
        )

      
      # =============================
      #   Time To Event
      # =============================
      
      characterizationTimeToEventServer(
          id = 'timeToEventTab', 
          connectionHandler = connectionHandler, 
          mainPanelTab = mainPanelTab,
          resultDatabaseSettings = resultDatabaseSettings
         )

      
      # =============================
      #   Dechallenge Rechallenge
      # =============================
      
      characterizationDechallengeRechallengeServer(
        id = 'dechallengeRechallengeTab', 
        connectionHandler = connectionHandler, 
        mainPanelTab = mainPanelTab,
        resultDatabaseSettings = resultDatabaseSettings
        )
      
    }
  )
}
