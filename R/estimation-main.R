# @file Estimation-main.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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


#' The location of the estimation module helper file
#'
#' @details
#' Returns the location of the characterization helper file
#' @family Estimation
#' @return
#' string location of the characterization helper file
#'
#' @export
estimationHelperFile <- function(){
  fileLoc <- system.file('estimation-www', "estimation.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring characterization studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family Estimation
#' @return
#' The user interface to the characterization viewer module
#'
#' @export
estimationViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = '100%',
    title =  shiny::span( shiny::icon("table"), "Estimation Viewer"),
    solidHeader = TRUE,
    
    # pick a targetId of interest 
    shiny::uiOutput(ns("targetSelection")),
    
    inputSelectionDfViewer(id = ns('targetSelected'), title = 'Selected'),
    
 
    # first show diagnostics with:
    # database, analysis, pass/fail, viewResult/viewDiagnostic
    # extracts from SCCS/CM/Evidence Synthesis
  
    shiny::conditionalPanel(
      condition = 'input.targetSelect', 
      ns = ns,
      
    shiny::tabsetPanel(
      type = 'pills',
      id = ns('mainPanel'),
      
      shiny::tabPanel(
        title = 'Diagnostics', 
        shiny::tabsetPanel(
          type = 'pills',
          id = ns('diagnosticsPanel')
        )
      ),
      
      shiny::tabPanel(
        title = 'Results', 
        shiny::tabsetPanel(
          type = 'pills',
          id = ns('resultsPanel')
        )
      ),
    )
  ) # end conditional panel
  
  )
  
}


#' The module server for exploring estimation studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @family Estimation
#' @return
#' The server for the estimation module
#'
#' @export
estimationServer <- function(
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
      estimationTypes <- getEstimationTypes(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      # add the tabs based on results
      types <- list(
        c("Cohort Method","estimationCmDiagnosticViewer", "estimationCmDiagnostic", "diagnosticsPanel", "Cohort Method"),
        c("SCCS", "estimationSccsDiagnosticViewer", "estimationSccsDiagnostic", "diagnosticsPanel", "SCCS"),
        c("Cohort Method", "estimationCmResultsViewer", "estimationCmResults", "resultsPanel", "Cohort Method Table"),
        c("Cohort Method", "estimationCmPlotsViewer", "estimationCmPlots", "resultsPanel", "Cohort Method Plot"),
        c("SCCS", "estimationSccsResultsViewer", "estimationSccsResults", "resultsPanel", "SCCS Table"),
        c("SCCS", "estimationSccsPlotsViewer", "estimationSccsPlots", "resultsPanel", "SCCS Plot")
      )
      selectValD <- T
      selectValR <- T
      for( type in types){
        if(type[1] %in% estimationTypes){
          shiny::insertTab(
            inputId = type[4],
            tab = shiny::tabPanel(
              title = type[5],
              do.call(what = type[2], args = list(id = session$ns(type[3])))
            ), 
            select = ifelse(type[4] == "diagnosticsPanel", selectValD, selectValR)
          )
          if(type[4] == "diagnosticsPanel"){
            selectValD = F
          } else{
            selectValR = F
          }
        }
        
      }
      
    
      
      # use the function in report-main to get parent Ts with all children Ts, the outcomes for the Ts and the Cs
      options <- getTandOs(
        connectionHandler,
        resultDatabaseSettings,
        includeCharacterization = F,
        includeCohortIncidence = F,
        includeCohortMethod = "Cohort Method" %in% estimationTypes,
        includePrediction = F, 
        includeSccs = "SCCS" %in% estimationTypes # slow so turning off
      )
      
      # Targets
      targets <- lapply(options$groupedTs, function(x) x$cohortId)
      targets <- unlist(targets) 
      
      # initial outcomes for first T
      outcomeDf <- options$tos[[1]]
      outcomes <- shiny::reactiveVal(outcomeDf)
      initialOutcomes <- outcomeDf$outcomeId
      names(initialOutcomes ) <- outcomeDf$outcomeName
      
      shiny::observeEvent(input$targetId,{
        
        outcomes(unique(
          do.call(
            'rbind',
            lapply(
              options$groupedTs[[which(targets == input$targetId)]]$subsets$targetId, 
              function(id){
                if(id %in% names(options$tos)){
                  return(options$tos[[which(id == names(options$tos))]])
                } else{
                  return(NULL)
                }
              }
            )
          )
        ))
        
        
        if(length(outcomes()$outcomeId)>0){
          outcomesVector <- outcomes()$outcomeId
          names(outcomesVector) <- outcomes()$outcomeName
          
          shinyWidgets::updatePickerInput(
            session = session, 
            inputId = 'outcomeId', 
            label = 'Outcome: ', 
            choices = outcomesVector, 
            selected = outcomesVector[1]
          )
        }
      })
      # end observed targetId
      
      output$targetSelection <- shiny::renderUI({
        shiny::fluidRow(
          shiny::div(
            shinyWidgets::pickerInput(
              inputId = session$ns('targetId'),
              label = 'Target: ',
              choices = targets,
              selected = targets[1],
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                dropupAuto = F,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 500
              )
            ), 
            shinyWidgets::pickerInput(
              inputId = session$ns('outcomeId'),
              label = 'Outcome: ',
              choices = initialOutcomes,
              selected = initialOutcomes[1],
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                dropupAuto = F,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 500
              )
            ), 
            style = 'margin-left: 2%; width: 78%; display: inline-block; vertical-align: middle;'
          ),
          shiny::div(
            shiny::actionButton(
              inputId = session$ns('targetSelect'), 
              label = 'Select',
              icon = shiny::icon('redo') 
            ), 
            style = 'display: inline-block; vertical-align: bottom; margin-bottom: 20px'
          )
        )
      })

      
      targetSelected <- shiny::reactiveVal(NULL)
      comparatorIds <- shiny::reactiveVal(NULL)
      targetIds <- shiny::reactiveVal(NULL)
      outcomeId <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$targetSelect, {
        
        targetSelected(
          data.frame( 
            Target = names(targets)[targets == input$targetId],
            Outcome = outcomes()$outcomeName[outcomes()$outcomeId == input$outcomeId]
          )
        )
        inputSelectionDfServer(
          id = 'targetSelected', 
          dataFrameRow = targetSelected,
          ncol = 1
        )
        
        #========================================
        # code to update diagnostics database
        #========================================
        # get all the ids that are children of the id selected
        targetIdsTemp <- options$groupedTs[[which(targets == input$targetId)]]$subsets$targetId

        comparators <- do.call(
          'rbind',
          lapply(
            options$groupedTs[[which(targets == input$targetId)]]$subsets$targetId, 
            function(id){
              if(id %in% names(options$cs)){
                return(options$cs[[which(id == names(options$cs))]])
              } else{
                return(NULL)
              }
            }
          )
        )
        targetIds(targetIdsTemp)
        comparatorIds(comparators$comparatorId)
        outcomeId(input$outcomeId)
      })
      
      #=======================================
      # SERVERS
      #=======================================
      if('Cohort Method' %in% estimationTypes){
        estimationCmDiagnosticServer(
          id = 'estimationCmDiagnostic', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          comparatorIds = comparatorIds,
          outcomeId = outcomeId
        )
        
        cmData <- estimationCmResultsServer(
          id = 'estimationCmResults', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          comparatorIds = comparatorIds,
          outcomeId = outcomeId
        )
        
        estimationCmPlotsServer(
          id = 'estimationCmPlots', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          cmData = cmData
        )
      }
      
      if('SCCS' %in% estimationTypes){
        estimationSccsDiagnosticServer(
          id = 'estimationSccsDiagnostic', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          outcomeId = outcomeId
        )
        
        sccsData <- estimationSccsResultsServer(
          id = 'estimationSccsResults', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          outcomeId = outcomeId
        )
        
        estimationSccsPlotsServer(
          id = 'estimationSccsPlots', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          sccsData = sccsData
        )
      }
      
    }
  )
}




getEstimationTypes <- function(
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
  
  # Cohort Method
  if(paste0(
    resultDatabaseSettings$cmTablePrefix,
    c('result')
  ) %in% tbls){
    results <- c(results, "Cohort Method")
  }
  
  # SCCS
  if(paste0(
    resultDatabaseSettings$sccsTablePrefix,
    'result'
  ) %in% tbls){
    results <- c(results, "SCCS")
  }
  
  # Evidence Synthesis
  if(
    paste0(
    resultDatabaseSettings$esTablePrefix,
    'cm_result'
  ) %in% tbls ||
  paste0(
    resultDatabaseSettings$esTablePrefix,
    'sccs_result'
  ) %in% tbls
  
  ){
    results <- c(results, "Evidence Synthesis")
  }

  return(results)
}
