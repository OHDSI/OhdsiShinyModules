# @file characterization-main.R
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
    id = ns('mainPanel')
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

      # this function checks tables exist for the tabs
      # and returns the tabs that should be displayed
      # as the tables exist
      charTypes <- getCharacterizationTypes(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      # add the tabs based on results
      types <- list(
        c("Target Viewer","characterizationTableViewer", "descriptiveTableTab"),
        c("Outcome Stratified", "characterizationAggregateFeaturesViewer", "aggregateFeaturesTab"),
        c("Incidence Rate", "characterizationIncidenceViewer", "incidenceTab"),
        c("Time To Event", "characterizationTimeToEventViewer", "timeToEventTab"),
        c("Dechallenge Rechallenge", 'characterizationDechallengeRechallengeViewer', 'dechallengeRechallengeTab')
        )
      selectVal <- T
      for( type in types){
        if(type[1] %in% charTypes){
          shiny::insertTab(
            inputId = "mainPanel",
            tab = shiny::tabPanel(
              type[1],  
              do.call(what = type[2], args = list(id = session$ns(type[3])))
            ), 
            select = selectVal
          )
        }
        selectVal = F
      }
        
      previouslyLoaded <- shiny::reactiveVal(c())
      
      # only render the tab when selected
      shiny::observeEvent(input$mainPanel,{
      
      # =============================
      #   Table of cohorts
      # =============================
        if(input$mainPanel == "Target Viewer"){
          if(!"Target Viewer" %in% previouslyLoaded()){
            characterizationTableServer(
              id = 'descriptiveTableTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
            )
            previouslyLoaded(c(previouslyLoaded(), "Target Viewer"))
          }
        }

      
      # =============================
      #   Aggregrate Features
      # =============================
        if(input$mainPanel == "Outcome Stratified"){
          if(!"Outcome Stratified" %in% previouslyLoaded()){
            characterizationAggregateFeaturesServer(
              id = 'aggregateFeaturesTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
            )
            previouslyLoaded(c(previouslyLoaded(), "Outcome Stratified"))
          }
        }
      
      # =============================
      #   Incidence
      # =============================
        if(input$mainPanel == "Incidence Rate"){
          if(!"Incidence Rate" %in% previouslyLoaded()){
            characterizationIncidenceServer(
              id = 'incidenceTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
            )
            previouslyLoaded(c(previouslyLoaded(), "Incidence Rate"))
          }
        }

      
      # =============================
      #   Time To Event
      # =============================
        if(input$mainPanel == "Time To Event"){
          if(!"Time To Event" %in% previouslyLoaded()){
            characterizationTimeToEventServer(
              id = 'timeToEventTab', 
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
            )
            previouslyLoaded(c(previouslyLoaded(), "Time To Event"))
          }
        }
      
      # =============================
      #   Dechallenge Rechallenge
      # =============================
        if(input$mainPanel == "Dechallenge Rechallenge"){
          if(!"Dechallenge Rechallenge" %in% previouslyLoaded()){
            characterizationDechallengeRechallengeServer(
              id = 'dechallengeRechallengeTab', 
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
            )
            previouslyLoaded(c(previouslyLoaded(), "Dechallenge Rechallenge"))
          }
        }
        
      }) # end observed input tab
      
    }
  )
}

getCharacterizationTypes <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  results <- c()
  
  conn <- DatabaseConnector::connect(
    connectionDetails = connectionHandler$connectionDetails
    )
  on.exit(DatabaseConnector::disconnect(conn))
  tbls <- DatabaseConnector::getTableNames(
    connection = conn,
    databaseSchema = resultDatabaseSettings$schema
  )

  # check Targets
  if(sum(paste0(
    resultDatabaseSettings$cTablePrefix,
    c('covariates', 'covariate_ref', 'cohort_details', 'settings')
  ) %in% tbls) == 4){
    results <- c(results, "Target Viewer", "Outcome Stratified" )
  }
  
  # check dechallenge_rechallenge
  if(paste0(
    resultDatabaseSettings$cTablePrefix,
    'dechallenge_rechallenge'
  ) %in% tbls){
    results <- c(results, "Dechallenge Rechallenge")
  }
  
  # check time_to_event
  if(paste0(
    resultDatabaseSettings$cTablePrefix,
    'time_to_event'
  ) %in% tbls){
    results <- c(results, "Time To Event")
  }
  
  # check incidence
  if(paste0(
    resultDatabaseSettings$incidenceTablePrefix,
    'incidence_summary'
  ) %in% tbls){
    results <- c(results, "Incidence Rate")
  }
  
  return(results)
}