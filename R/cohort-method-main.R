# @file cohort-method-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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


#' The location of the cohort method module helper file
#'
#' @details
#' Returns the location of the cohort method helper file
#' 
#' @return
#' string location of the cohort method helper file
#'
#' @export
cohortMethodHelperFile <- function(){
  fileLoc <- system.file('cohort-method-www', "cohort-method.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The viewer of the main cohort method module
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method results viewer
#' 
#' @export
cohortMethodViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title = shiny::span( shiny::icon("chart-column"), 'Cohort Method'),
    solidHeader = TRUE,
    
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("cohort-method-www", "cohort-method.html", package = utils::packageName())
    ),
    
    # Input selection of T, C and Os
    inputSelectionViewer(ns("input-selection")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
    shiny::tabsetPanel(
      type = 'pills',
      id = ns('mainPanel'),
      
      shiny::tabPanel(
        title = "Diagnostics",
        cohortMethodDiagnosticsSummaryViewer(ns("cmDiganostics"))
      ),
      
      shiny::tabPanel(
        title = "Results",
        cohortMethodResultSummaryViewer(ns("cmResults"))
      )
    )
    )
    
  )
}
    

#' The module server for the main cohort method module
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a named list containing the PLE results database connection details
#'
#' @return
#' the PLE results viewer main module server
#' 
#' @export
cohortMethodServer <- function(
    id, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      dataFolder <- NULL
      
      targetIds <- getCmCohorts(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        type = 'target'
      )
      outcomeIds <- getCmCohorts(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        type = 'outcome'
      )
      comparatorIds <- getCmCohorts(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        type = 'comparator'
      )
      analysisIds <- getCmAnalyses(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = targetIds,
              selected = targetIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              selected = outcomeIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 6,
            varName = 'comparatorIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Comparator: ',
              choices = comparatorIds,
              selected = comparatorIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 6,
            varName = 'analysisIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Analysis: ',
              choices = analysisIds,
              selected = analysisIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      cohortMethodDiagnosticsSummaryServer(
        id = "cmDiganostics",
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        inputSelected = inputSelected
        )
      
      cohortMethodResultSummaryServer(
        id = "cmResults",
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        inputSelected = inputSelected
      )
      
    }
  )
}

getCmCohorts <- function(
    connectionHandler,
    resultDatabaseSettings,
    type = 'target'
){
  
  sql <- "
    SELECT DISTINCT
      cgcd1.cohort_name as names,
      cgcd1.cohort_definition_id
    FROM
      @schema.@cm_table_prefixresult cmds
      INNER JOIN 
      @schema.@cg_table_prefixcohort_definition cgcd1 
      ON cmds.@type_id = cgcd1.cohort_definition_id;
  "
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    type = type
  )
  
  res <- result$cohortDefinitionId
  names(res) <- result$names
  
  return(
    res
  )
}

getCmAnalyses <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  sql <- "
    SELECT DISTINCT
      cma.analysis_id,
      cma.description as names
    FROM
      @schema.@cm_table_prefixresult cmds
      INNER JOIN 
      @schema.@cm_table_prefixanalysis cma 
      ON cmds.analysis_id = cma.analysis_id
      ;
  "
  
  result <-  connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix
  )
  
  res <- result$analysisId
  names(res) <- result$names
  
  return(
    res
  )
  
}
