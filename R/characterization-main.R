# @file characterization-main.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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
#' @family Characterization
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
#' @family Characterization
#' @return
#' The user interface to the characterization viewer module
#'
#' @export
characterizationViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', width = '100%',
    title =  shiny::span( shiny::icon("table"), "Characterization Viewer"),
    solidHeader = TRUE,
    
    # pick a targetId of interest 
    tableSelectionViewer(id = ns('target-table-select')),
    
    shiny::uiOutput(outputId = ns('analysesOptions')),
  
    shiny::uiOutput(outputId = ns('analysesResults'))
    
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
#' @family Characterization
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

      # finds all the targets in every analysis and gets the counts
      # targetId, parentId, targetName, parentName, cohortSize, 
      # incC, incCI, incCM, incSCCS, incPLP
      targetTable <- getTargetsUsedInChar(
        connectionHandler = connectionHandler,
        schema = resultDatabaseSettings$schema,
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
        cTablePrefix = resultDatabaseSettings$cTablePrefix,
        ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix
      )
        
      resultType <- shiny::reactiveVal("")
      reactiveOutcomeTable <- shiny::reactiveVal(NULL)
      reactiveCharacterizationTargetTable <- shiny::reactiveVal(NULL)

      # create reactive that saves selected rowIds for targetTable and outcomeTable
      reactiveTargetRowId <- shiny::reactiveVal(NULL)
      reactiveTargetRow <- shiny::reactiveVal(NULL)
      ##reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      ##reactiveOutcomeRowId <- shiny::reactiveVal(NULL)
      
      # the table with all the possible targetIds
      tableSelectionServer(
        id = 'target-table-select',
        table = shiny::reactive(targetTable), 
        selectedRowId = reactiveTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector'),
        inputColumns = characterizationTargetInputColumns(),
        displayColumns = characterizationTargetDisplayColumns(), 
        selectButtonText = 'Select Target'
        )
      
      # react to the target being set
      shiny::observeEvent(reactiveTargetRowId(),{
        
        reactiveTargetRow(targetTable[reactiveTargetRowId(),])
        
        # get the characterization target ids
        if(!is.null(targetTable$cohortDefinitionId[reactiveTargetRowId()])){
          if(length(targetTable$cohortDefinitionId[reactiveTargetRowId()]) > 0){
            reactiveCharacterizationTargetTable(
              getCharacterizationTargetId(
                connectionHandler = connectionHandler,
                schema = resultDatabaseSettings$schema,
                databaseTable = resultDatabaseSettings$databaseTable,
                targetId = targetTable$cohortDefinitionId[reactiveTargetRowId()],
                cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
                cTablePrefix = resultDatabaseSettings$cTablePrefix
              )
            )
          }
        }
        
        # reset the outcome row id
        #reactiveOutcomeRowId(0)
        
        if(nrow(reactiveTargetRow()) > 0){
          analyses <- c('Database Comparison',
                        'Cohort Comparison',
                        'Dechallenge Rechallenge',
                        'Risk Factors',
                        'Time-to-event',
                        'Case Series',
                        'Cohort Incidence')
          
          # display the result options to select 
          analysesWithResults <- reactiveTargetRow()[c(
            'databaseComparator', 'cohortComparator',
            'dechalRechal', 'riskFactors',
             'timeToEvent', 'caseSeries',
             'cohortIncidence')] == 1
          analysesWithResults <- as.logical(analysesWithResults)
          
          if(sum(analysesWithResults) > 0){
            
            output$analysesOptions <- shiny::renderUI(
              shiny::div(
                shiny::tags$style(
                  '.analysis-tabs > li.disabled > a { color: #8d8d8d !important; background-color: #f1f1f1 !important; border-color: #d9d9d9 !important; cursor: not-allowed !important; pointer-events: none; }\n                   .analysis-tabs > li > a { border: 1px solid #d2d6de; margin-right: 4px; }\n                   .analysis-tabs > li.active > a { background-color: #3c8dbc !important; color: #fff !important; border-color: #367fa9 !important; }'
                ),
                shiny::tags$div(
                  shiny::tags$label('Choose Analysis:'),
                  shiny::tags$ul(
                    class = 'nav nav-pills analysis-tabs',
                    lapply(seq_along(analyses), function(i) {
                      analysisName <- analyses[i]
                      isAvailable <- analysesWithResults[i]
                      isActive <- identical(resultType(), analysisName)

                      if (isAvailable) {
                        shiny::tags$li(
                          class = if (isActive) 'active' else NULL,
                          shiny::tags$a(
                            href = '#',
                            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", session$ns('resultType'), analysisName),
                            analysisName
                          )
                        )
                      } else {
                        shiny::tags$li(
                          class = 'disabled',
                          shiny::tags$a(
                            href = '#',
                            onclick = 'return false;',
                            title = 'analysis not available for selected target cohort id',
                            analysisName
                          )
                        )
                      }
                    })
                  )
                )
              )
            )
            
            # set the resultType to the first 
            resultType(analyses[analysesWithResults][1])
            
          } else{
            # set values to take you back to start
            reactiveOutcomeTable(NULL)
            output$analysesOptions <- NULL
            resultType("") # update resultType to get UI to change 
            output$analysesOptions <- shiny::renderUI(shiny::helpText('No analyses results to show'))
          }
          
          
          # if a case series set the outcome table
          # update the outcomes for the selected target id
          analysesWithResultsOutcome <- reactiveTargetRow()[c(
            'dechalRechal', 'riskFactors',
            'timeToEvent', 'caseSeries',
            'cohortIncidence')] == 1
          
          # TODO - figure out how to handle if CI has diff outcomes
          
          if(sum(analysesWithResultsOutcome) > 0){
          
          reactiveOutcomeTable(getOutcomesUsedInChar(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = reactiveTargetRow()$cohortDefinitionId[1]
          ))
            
        } else{
          reactiveOutcomeTable(NULL)
        }
          
        } else{ # if no target selected set outcome table to null
          reactiveOutcomeTable(NULL)
          output$analysesOptions <- NULL
          resultType("") # update resultType to get UI to change 
        }

      })
      # end react to target id
      
      # update resultType when input changes
      shiny::observeEvent(input$resultType,{
        resultType(input$resultType)
      })
      
      # listen to the radio seleciton 
      shiny::observeEvent(resultType(),{
        
        # check the UI based on the analysis
        if(resultType() == 'Cohort Incidence'){
          output$analysesResults <- shiny::renderUI(
            characterizationIncidenceViewer(
              id = session$ns('incidence')
            )
          )
        } else if(resultType() == 'Database Comparison'){
          output$analysesResults <- shiny::renderUI(
            characterizationDatabaseComparisonViewer(
              id = session$ns('database-comparison')
            )
          )
        } else if(resultType() == 'Cohort Comparison'){
          output$analysesResults <- shiny::renderUI(
            characterizationCohortComparisonViewer(
              id = session$ns('cohort-comparison')
            )
          )
        } else if(resultType() == 'Time-to-event'){
          output$analysesResults <- shiny::renderUI(
            characterizationTimeToEventViewer(
              id = session$ns('time-to-event')
            )
          )
        } else if(resultType() == 'Dechallenge Rechallenge'){
          output$analysesResults <- shiny::renderUI(
            characterizationDechallengeRechallengeViewer(
              id = session$ns('dechal-rechal')
            )
          )
        } else if(resultType() == 'Risk Factors'){
          output$analysesResults <- shiny::renderUI(
            characterizationRiskFactorViewer(
              id = session$ns('risk-factor')
            )
          )
        } else if(resultType() == 'Case Series'){
          output$analysesResults <- shiny::renderUI(
            characterizationCaseSeriesViewer(
              id = session$ns('case-series')
            )
          )
        } else{
          output$analysesResults <- NULL
        }

      })
    
      # add the servers
      characterizationDatabaseComparisonServer(
        id = 'database-comparison',
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable
      )
      characterizationCohortComparisonServer(
        id = 'cohort-comparison', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        targetTable = targetTable,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable
      )
      
      characterizationTimeToEventServer(
        id = 'time-to-event', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable,
        reactiveOutcomeTable = reactiveOutcomeTable
      )
      
      characterizationDechallengeRechallengeServer(
        id = 'dechal-rechal', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable,
        reactiveOutcomeTable = reactiveOutcomeTable
      )
      
      characterizationRiskFactorServer(
        id = 'risk-factor', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable
      )
      
      characterizationCaseSeriesServer(
        id = 'case-series', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveCharacterizationTargetTable = reactiveCharacterizationTargetTable
      )
      
      characterizationIncidenceServer(
        id = 'incidence', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        reactiveOutcomeTable = reactiveOutcomeTable
        )

 
    }
  )
}


characterizationTargetInputColumns <- function(){
  return(
    list(
    
      cohortDefinitionId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
        ),
      cohortName = reactable::colDef(
        name = 'Cohort Name',
        minWidth = 300
      ),
      
      timeToEvent = reactable::colDef(
        name = 'Time To Event',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ), 
      dechalRechal = reactable::colDef(
        name = 'Dechal Rechal',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ), 
      databaseComparator = reactable::colDef(
        name = 'Database Comparator',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ),
      cohortComparator = reactable::colDef(
        name = 'Cohort Comparator',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ),
      riskFactors = reactable::colDef(
        name = 'Risk Factors',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ), 
      caseSeries = reactable::colDef(
        name = 'Case Series',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0) "\u274c No" else "\u2714\ufe0f Yes"
        }
      ),
      cohortIncidence = reactable::colDef(
        name = 'Incidence',
        cell = function(value) {
          # Render as an X mark or check mark
          if (value == 0 ) "\u274c No" else "\u2714\ufe0f Yes"
        }
      )
    )
  )
}


characterizationTargetDisplayColumns <- function(){
  return(
    list(
      cohortDefinitionId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
      ),
      cohortName = reactable::colDef(
        name = 'Cohort Name',
        minWidth = 300
      ),
     
      timeToEvent = reactable::colDef(
        show = FALSE
      ), 
      dechalRechal = reactable::colDef(
        show = FALSE
      ),
      databaseComparator = reactable::colDef(
        show = FALSE
      ),
      cohortComparator = reactable::colDef(
        show = FALSE
      ), 
      riskFactors = reactable::colDef(
        show = FALSE
      ), 
      caseSeries = reactable::colDef(
        show = FALSE
      ),
      cohortIncidence = reactable::colDef(
        show = FALSE
      )
    )
  )
}


characterizationOutcomeDisplayColumns <- function(){
  return(
    list(

      cohortDefinitionId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
      ),
      cohortName = reactable::colDef(
        name = 'Cohort Name',
        minWidth = 300
      ),
      timeToEvent = reactable::colDef(
        show = FALSE
      ), 
      dechalRechal = reactable::colDef(
        show = FALSE
      ),
      riskFactors = reactable::colDef(
        show = FALSE
      ), 
      caseSeries = reactable::colDef(
        show = FALSE
      ),
      cohortIncidence = reactable::colDef(
        show = FALSE
      )
      
    )
  )
}


# this gets the cohort_definition_id, cohort_name, parent_name, parent_id and 
# what modules used the cohortd
getTargetsUsedInChar <- function(
    connectionHandler,
    schema,
    cgTablePrefix = 'cg_',
    cTablePrefix = 'c_',
    ciTablePrefix = 'ci_'
    ){
  
  shiny::withProgress(message = 'Loading targets', value = 0, {
    
    shiny::incProgress(2/4, detail = paste("Extracting targets"))
    
    charTargets <- OhdsiReportGenerator::getTargetsUsedInCharacterization(
      connectionHandler = connectionHandler,
      schema = schema, 
      cTablePrefix = cTablePrefix, 
      cgTablePrefix = cgTablePrefix
    )
    
    incidenceTargets <- OhdsiReportGenerator:::getTargetsUsedInIncidence(
      connectionHandler = connectionHandler,
      schema = schema, 
      ciTablePrefix = ciTablePrefix, 
      cgTablePrefix = cgTablePrefix
    )
    
    result <- merge(
      x = charTargets,
      y = incidenceTargets, 
      all = TRUE, 
      by = c('cohortName', 'cohortDefinitionId')
      )
    
    if(sum(is.na(result$cohortIncidence)) > 0){
      result$cohortIncidence[is.na(result$cohortIncidence)] <- 0
    }
    
    allColsOfInt <- colnames(result)[!colnames(result) %in% c('cohortName', 'cohortDefinitionId')]
      
    # replace NA with 0
    result <- result %>%
      dplyr::mutate(dplyr::across(allColsOfInt, ~ tidyr::replace_na(.x, 0)))
    
    shiny::incProgress(4/4, detail = paste("Done"))
    
  })
  
  return(result)
}

# This gets the characterizationTargetIds for the specific targetId
getCharacterizationTargetId <- function(
    connectionHandler,
    schema,
    targetId,
    databaseTable = 'database_meta_data',
    cgTablePrefix = 'cg_',
    cTablePrefix = 'c_'
){
  
  shiny::withProgress(message = 'Loading targets', value = 0, {
    
    shiny::incProgress(2/4, detail = paste("Extracting targets"))
    
    result <- OhdsiReportGenerator::getCharacterizationTargetSettings(
      connectionHandler = connectionHandler,
      schema = schema, 
      cTablePrefix = cTablePrefix, 
      cgTablePrefix = cgTablePrefix,
      targetIds = targetId, 
      addDatabaseDetails = TRUE, 
      databaseTable = databaseTable
    )
  
      
    shiny::incProgress(4/4, detail = paste("Done"))
    
  })
  
  return(result)
}



# adding progress report around outcome extraction as it can be slow
getOutcomesUsedInChar <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId
){
  
  shiny::withProgress(message = 'Loading outcomes for selected target', value = 0, {
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    cOutcomes <- OhdsiReportGenerator::getOutcomesUsedInCharacterization(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      targetId = targetId
    )
    
    ciOutcomes <- OhdsiReportGenerator::getOutcomesUsedInIncidence(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
      targetId = targetId
    )
    
    result <- merge(
      x = cOutcomes, 
      y = ciOutcomes, 
      by = c('cohortName', 'cohortDefinitionId'),
      all = TRUE
    )
    
    # Note: may need to add missing columns if either queries return 0 rows
    
    message(paste0('Extracted ',nrow(result),' outcomes'))
  
    
    shiny::incProgress(4/4, detail = paste("Done"))
    
  })
  
  return(result)
  
}
