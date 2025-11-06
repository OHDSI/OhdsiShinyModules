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
    
    shiny::conditionalPanel(
      condition = 'output.outcomeShow == 1', 
      ns = ns,
      shiny::uiOutput(ns("outcomeSelection"))
    ),
    
    # first show diagnostics with:
    # database, analysis, pass/fail, viewResult/viewDiagnostic
    # extracts from SCCS/CM/Evidence Synthesis
  
    shiny::conditionalPanel(
      condition = 'output.tabShow == 1', 
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
      
      # initiate the cohortIds as reactiveVal
      targetIds <- shiny::reactiveVal(NULL)
      outcomeId <- shiny::reactiveVal(NULL)
      
      # initiate what to show
      output$outcomeShow <- shiny::reactive(0)
      shiny::outputOptions(output, "outcomeShow", suspendWhenHidden = FALSE)
      
      output$tabShow <- shiny::reactive(0)
      shiny::outputOptions(output, "tabShow", suspendWhenHidden = FALSE)
      
      
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
      targetTable <- OhdsiReportGenerator::getTargetTable(
        connectionHandler = connectionHandler, 
        schema = resultDatabaseSettings$schema, 
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
        cTablePrefix = resultDatabaseSettings$cTablePrefix, 
        ciTablePrefix = resultDatabaseSettings$ciTablePrefix, 
        cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
        sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix, 
        plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
        databaseTable = resultDatabaseSettings$databaseTable, 
        getIncidenceInclusion = FALSE, 
        getCharacterizationInclusion = FALSE, 
        getPredictionInclusion = FALSE, 
        getCohortMethodInclusion = "Cohort Method" %in% estimationTypes, 
        getSccsInclusion = "SCCS" %in% estimationTypes
        ) %>%
        dplyr::filter(
          .data$cohortMethod == 1 | .data$selfControlledCaseSeries == 1
        )
      
      # Targets
      parentInd <- targetTable$cohortId == targetTable$subsetParent
      targets <- targetTable$cohortId[parentInd]
      names(targets) <- targetTable$cohortName[parentInd]
      targets <- targets[order(names(targets))]
      
      output$targetSelection <- shiny::renderUI({
        shinydashboard::box(
          title = 'Target Option', 
          solidHeader = TRUE,
          width = 12,
          
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
                #size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 500
              )
            ),
          
          shiny::actionButton(
              inputId = session$ns('targetSelect'), 
              label = 'Select',
              icon = shiny::icon('redo') 
            )
          
        )
      })
      
      # if the target input is change hide the outcomes and results
      shiny::observeEvent(input$targetId, {
        output$outcomeShow = shiny::reactive(0)
        output$tabShow = shiny::reactive(0)
      })
      
      outcomes <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$targetSelect,{
        output$outcomeShow = shiny::reactive(1)
        targetIds <- unique(targetTable$cohortId[targetTable$subsetParent == input$targetId ])
        
        outcomeTable <- getEstimationOutcomes(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          estimationTypes = estimationTypes
        )
        
        # TODO fix issue where this may be false so old options show
        if(nrow(outcomeTable) > 0){
          outcomeChoice <- outcomeTable$cohortId
          names(outcomeChoice) <- outcomeTable$cohortName
          
          outcomes(outcomeChoice)
        }
      })
      # end observed targetId
      
      output$outcomeSelection <- shiny::renderUI({
        shinydashboard::box(
          title = 'Outcome Options', 
          solidHeader = TRUE, 
          width = 12,
          
          shinyWidgets::pickerInput(
              inputId = session$ns('outcomeId'),
              label = 'Outcome: ',
              choices = outcomes(),
              selected = outcomes()[1],
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
        
            shiny::actionButton(
              inputId = session$ns('outcomeSelect'), 
              label = 'Select',
              icon = shiny::icon('redo') 
            )
        )
      })
      
      # hide tabs if outcomeId changes
      shiny::observeEvent(input$outcomeId, {
        output$tabShow = shiny::reactive(0)
      })

      shiny::observeEvent(input$outcomeSelect, {
        output$tabShow = shiny::reactive(1)
        
        # reset all the tabs to the first one
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'mainPanel',
          selected = 'Diagnostics'
        )
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'diagnosticsPanel',
          selected = NULL
        )
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'resultsPanel',
          selected = NULL
        )
        
        
        targetIdsTemp <- targetTable$cohortId[targetTable$subsetParent == input$targetId]
          
        targetIds(targetIdsTemp)
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
          outcomeId = outcomeId
        )
        
        cmData <- estimationCmResultsServer(
          id = 'estimationCmResults', 
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
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


getEstimationOutcomes <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetIds,
  estimationTypes
){

  shiny::withProgress(message = 'Finding outcomes for selected target', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting outcomes"))
    
result <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandler, 
  schema = resultDatabaseSettings$schema, 
  cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
  cTablePrefix = resultDatabaseSettings$cTablePrefix, 
  ciTablePrefix = resultDatabaseSettings$ciTablePrefix, 
  cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
  sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix, 
  plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
  databaseTable = resultDatabaseSettings$databaseTable, 
  targetId = targetIds,
  getIncidenceInclusion = FALSE, 
  getCharacterizationInclusion = FALSE, 
  getPredictionInclusion = FALSE, 
  getCohortMethodInclusion = "Cohort Method" %in% estimationTypes, 
  getSccsInclusion = "SCCS" %in% estimationTypes
) %>%
  dplyr::filter(
    .data$cohortMethod == 1 | .data$selfControlledCaseSeries == 1
  ) %>%
  dplyr::arrange("cohortName")

shiny::incProgress(4/4, detail = paste("Done"))

})
  
  return(result )
}
