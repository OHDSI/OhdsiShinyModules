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
        targetRowId <- reactiveTargetRowId()

        hasValidTargetRowId <- !is.null(targetRowId) &&
          length(targetRowId) == 1 &&
          !is.na(targetRowId) &&
          targetRowId > 0 &&
          targetRowId <= nrow(targetTable)

        if (!hasValidTargetRowId) {
          reactiveTargetRow(data.frame())
          reactiveCharacterizationTargetTable(NULL)
          reactiveOutcomeTable(NULL)
          output$analysesOptions <- NULL
          output$analysesResults <- NULL
          resultType('')
          return(invisible(NULL))
        }
         
        selectedTargetRow <- targetTable[targetRowId, , drop = FALSE]
        reactiveTargetRow(selectedTargetRow)

        selectedTargetId <- selectedTargetRow$cohortDefinitionId[1]

        # get the characterization target ids
        if (!is.null(selectedTargetId) && length(selectedTargetId) == 1 && !is.na(selectedTargetId)) {
          reactiveCharacterizationTargetTable(
            getCharacterizationTargetId(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              databaseTable = resultDatabaseSettings$databaseTable,
              targetId = selectedTargetId,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              cTablePrefix = resultDatabaseSettings$cTablePrefix
            )
          )
        } else {
          reactiveCharacterizationTargetTable(NULL)
        }
        
        # reset the outcome row id
        #reactiveOutcomeRowId(0)
        
        if (nrow(selectedTargetRow) > 0) {
          analyses <- c('Database Comparison',
                        'Cohort Comparison',
                        'Dechallenge Rechallenge',
                        'Risk Factors',
                        'Time-to-event',
                        'Case Series',
                        'Cohort Incidence')
          
          # display the result options to select 
          analysisToColMap <- c(
            'Database Comparison' = 'databaseComparator',
            'Cohort Comparison' = 'cohortComparator',
            'Dechallenge Rechallenge' = 'dechalRechal',
            'Risk Factors' = 'riskFactors',
            'Time-to-event' = 'timeToEvent',
            'Case Series' = 'caseSeries',
            'Cohort Incidence' = 'cohortIncidence'
          )
          analysesWithResults <- sapply(analysisToColMap, function(col) {
            ifelse(col %in% colnames(selectedTargetRow), 
                   as.logical(selectedTargetRow[[col]][1] == 1), 
                   FALSE)
          })
          
          if(sum(analysesWithResults) > 0){
            
            output$analysesOptions <- shiny::renderUI(
              shiny::div(
                shiny::tags$style(
                  '
                  .analysis-selector {
                    margin: 8px 0 14px 0;
                  }
                  .analysis-selector-header {
                    margin-bottom: 12px;
                  }
                  .analysis-selector-title {
                    font-size: 16px;
                    font-weight: 700;
                    color: #1f2937;
                    margin: 0 0 4px 0;
                  }
                  .analysis-selector-subtitle {
                    color: #6b7280;
                    margin: 0;
                  }
                  .analysis-grid {
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
                    gap: 14px;
                  }
                  .analysis-card {
                    position: relative;
                    display: flex;
                    flex-direction: column;
                    gap: 10px;
                    width: 100%;
                    min-height: 164px;
                    padding: 16px 16px 14px 16px;
                    border-radius: 18px;
                    border: 1px solid #d7dde5;
                    background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
                    box-shadow: 0 10px 28px rgba(15, 23, 42, 0.08);
                    text-align: left;
                    transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
                  }
                  .analysis-card.available {
                    cursor: pointer;
                  }
                  .analysis-card.available:hover {
                    transform: translateY(-2px);
                    box-shadow: 0 16px 34px rgba(15, 23, 42, 0.12);
                  }
                  .analysis-card.active {
                    border-color: var(--analysis-accent);
                    box-shadow: 0 18px 40px rgba(15, 23, 42, 0.15);
                    transform: translateY(-1px);
                  }
                  .analysis-card.disabled {
                    background: linear-gradient(180deg, #fafbfc 0%, #f1f4f7 100%);
                    opacity: 0.72;
                    cursor: not-allowed;
                  }
                  .analysis-card-top {
                    display: flex;
                    align-items: flex-start;
                    gap: 12px;
                  }
                  .analysis-card-icon {
                    flex: 0 0 auto;
                    width: 46px;
                    height: 46px;
                    border-radius: 15px;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    color: #ffffff;
                    background: linear-gradient(135deg, var(--analysis-accent-start), var(--analysis-accent-end));
                    box-shadow: 0 10px 18px rgba(15, 23, 42, 0.16);
                  }
                  .analysis-card-name {
                    font-size: 17px;
                    font-weight: 700;
                    color: #132238;
                    line-height: 1.25;
                    margin: 0;
                  }
                  .analysis-card-text {
                    color: #556271;
                    line-height: 1.45;
                    margin: 0;
                    flex: 1 1 auto;
                  }
                  .analysis-card-footer {
                    display: flex;
                    align-items: center;
                    justify-content: space-between;
                    gap: 10px;
                    margin-top: auto;
                  }
                  .analysis-card-badge {
                    display: inline-flex;
                    align-items: center;
                    padding: 5px 10px;
                    border-radius: 999px;
                    font-size: 12px;
                    font-weight: 700;
                    letter-spacing: 0.02em;
                  }
                  .analysis-card-badge.available {
                    background: rgba(34, 197, 94, 0.12);
                    color: #15803d;
                  }
                  .analysis-card-badge.disabled {
                    background: rgba(148, 163, 184, 0.18);
                    color: #64748b;
                  }
                  '
                ),
                shiny::div(
                  class = 'analysis-selector',
                  shiny::div(
                    class = 'analysis-selector-header',
                    shiny::tags$div(class = 'analysis-selector-title', 'Choose an analysis to explore'),
                    shiny::tags$div(
                      class = 'analysis-selector-subtitle',
                      'Each card explains what the result type answers.'
                    )
                  ),
                  shiny::div(
                    class = 'analysis-grid',
                    lapply(seq_along(analyses), function(i) {
                      analysisName <- analyses[i]
                      isAvailable <- analysesWithResults[analysisName]
                      isActive <- identical(resultType(), analysisName)

                      analysisMeta <- switch(
                        analysisName,
                        'Database Comparison' = list(
                          icon = 'database',
                          accentStart = '#0f766e',
                          accentEnd = '#14b8a6',
                          description = 'Compare characteristics of the target population between databases.'
                        ),
                        'Cohort Comparison' = list(
                          icon = 'users',
                          accentStart = '#2563eb',
                          accentEnd = '#60a5fa',
                          description = 'Compare characteristics of two populations side by side.'
                        ),
                        'Dechallenge Rechallenge' = list(
                          icon = 'redo',
                          accentStart = '#7c3aed',
                          accentEnd = '#a855f7',
                          description = 'See how outcomes change when exposure is withdrawn and restarted.'
                        ),
                        'Risk Factors' = list(
                          icon = 'user-shield',
                          accentStart = '#db2777',
                          accentEnd = '#f472b6',
                          description = 'Answer who is at risk for the outcome.'
                        ),
                        'Time-to-event' = list(
                          icon = 'clock',
                          accentStart = '#ea580c',
                          accentEnd = '#fb923c',
                          description = 'See when the outcome occurs relative to the target index.'
                        ),
                        'Case Series' = list(
                          icon = 'table',
                          accentStart = '#0891b2',
                          accentEnd = '#22d3ee',
                          description = 'Explore how people with the outcome change over key time points.'
                        ),
                        'Cohort Incidence' = list(
                          icon = 'chart-line',
                          accentStart = '#16a34a',
                          accentEnd = '#4ade80',
                          description = 'See how often the outcome occurs within a population.'
                        )
                      )

                      cardClasses <- c('analysis-card')
                      if (isAvailable) {
                        cardClasses <- c(cardClasses, 'available')
                      } else {
                        cardClasses <- c(cardClasses, 'disabled')
                      }
                      if (isActive) {
                        cardClasses <- c(cardClasses, 'active')
                      }

                      cardAttributes <- list(
                        class = paste(cardClasses, collapse = ' '),
                        style = paste0(
                          '--analysis-accent: ', analysisMeta$accentEnd, '; ',
                          '--analysis-accent-start: ', analysisMeta$accentStart, '; ',
                          '--analysis-accent-end: ', analysisMeta$accentEnd, ';'
                        )
                      )

                      if (isAvailable) {
                        cardAttributes$onclick <- sprintf(
                          "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                          session$ns('resultType'),
                          analysisName
                        )
                        cardAttributes$onkeydown <- sprintf(
                          "if(event.key === 'Enter' || event.key === ' ') { Shiny.setInputValue('%s', '%s', {priority: 'event'}); event.preventDefault(); }",
                          session$ns('resultType'),
                          analysisName
                        )
                        cardAttributes$tabindex <- '0'
                        cardAttributes$role <- 'button'
                      } else {
                        cardAttributes$title <- 'analysis not available for selected target cohort id'
                        cardAttributes$role <- 'note'
                      }

                      do.call(
                        shiny::tags$div,
                        c(
                          cardAttributes,
                          list(
                            shiny::tags$div(
                              class = 'analysis-card-top',
                              shiny::tags$div(
                                class = 'analysis-card-icon',
                                shiny::icon(analysisMeta$icon)
                              ),
                              shiny::tags$div(
                                style = 'min-width: 0; flex: 1 1 auto;',
                                shiny::tags$div(class = 'analysis-card-name', analysisName),
                                shiny::tags$div(class = 'analysis-card-text', analysisMeta$description)
                              )
                            ),
                            shiny::tags$div(
                              class = 'analysis-card-footer',
                              shiny::tags$span(
                                class = paste('analysis-card-badge', if (isAvailable) 'available' else 'disabled'),
                                if (isAvailable) 'Available' else 'Unavailable'
                              ),
                              if (isActive && isAvailable) {
                                shiny::tags$span(
                                  class = 'analysis-card-badge available',
                                  'Selected'
                                )
                              }
                            )
                          )
                        )
                      )
                    })
                  )
                )
              )
            )
            
            availableAnalyses <- analyses[analysesWithResults]

            # only reset to first available analysis when current one is not available
            if (!(resultType() %in% availableAnalyses)) {
              resultType(availableAnalyses[1])
            }
            
          } else{
            # set values to take you back to start
            reactiveOutcomeTable(NULL)
            output$analysesOptions <- NULL
            resultType("") # update resultType to get UI to change 
            output$analysesOptions <- shiny::renderUI(shiny::helpText('No analyses results to show'))
          }
          
          
          # if a case series set the outcome table
          # update the outcomes for the selected target id
          colsToSelectOutcome <- intersect(
            c('dechalRechal', 'riskFactors',
              'timeToEvent', 'caseSeries',
              'cohortIncidence'),
            colnames(selectedTargetRow)
          )
          analysesWithResultsOutcome <- selectedTargetRow[colsToSelectOutcome] == 1
          
          # TODO - figure out how to handle if CI has diff outcomes
          
          if(sum(analysesWithResultsOutcome) > 0){
          
          reactiveOutcomeTable(getOutcomesUsedInChar(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = selectedTargetId
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
