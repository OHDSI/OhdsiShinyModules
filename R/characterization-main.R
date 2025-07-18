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
    
    shiny::conditionalPanel(
      condition = 'output.showOutcomeSelector == true', 
      ns = ns,
      #shiny::h1('made up')
      tableSelectionViewer(id = ns('outcome-table-select'))
    ),
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
      targetTable <- OhdsiReportGenerator::getTargetTable(
        connectionHandler = connectionHandler, 
        schema = resultDatabaseSettings$schema,
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
        cTablePrefix = resultDatabaseSettings$cTablePrefix,
        ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
        cmTablePrefix = resultDatabaseSettings$cmTablePrefix,
        sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
        plpTablePrefix = resultDatabaseSettings$plpTablePrefix,
        databaseTable = resultDatabaseSettings$databaseTable
      )
      
      resultType <- shiny::reactiveVal("")
      outcomeTable <- shiny::reactiveVal(NULL)
      outcomeTableReset <- shiny::reactive(0)
      output$showOutcomeSelector <- shiny::reactive(FALSE)
      shiny::outputOptions(output, "showOutcomeSelector", suspendWhenHidden = FALSE)
      
      reactiveTargetRow <- tableSelectionServer(
        id = 'target-table-select',
        table = shiny::reactive(targetTable %>%
          dplyr::select(-"cohortMethod", -"selfControlledCaseSeries", -"prediction") %>%
          dplyr::relocate("parentName", .before = "cohortName") %>%
          dplyr::relocate("cohortId", .after = "cohortName")
          ), 
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector'),
        inputColumns = characterizationTargetInputColumns(),
        displayColumns = characterizationTargetDisplayColumns(), 
        selectButtonText = 'Select Target'
        )
      
      # react to the target being set
      shiny::observeEvent(reactiveTargetRow(),{
        
        # change the outcomeTableReset to reset the outcome selection
        outcomeTableReset <- shiny::reactive(outcomeTableReset()+1)
        
        if(nrow(reactiveTargetRow()) > 0){
          
          # display the result options to select 
          analysesWithResults <- reactiveTargetRow()[c(
            'databaseComparator', 'cohortComparator',
            'dechalRechal', 'riskFactors',
             'timeToEvent', 'caseSeries',
             'cohortIncidence')] == 1
          
          if(sum(analysesWithResults) > 0){
            
            output$analysesOptions <- shiny::renderUI(
              shiny::div(
                shinyWidgets::radioGroupButtons(
                  inputId = session$ns("resultType"), 
                  label = "Choose Analysis:", 
                  direction = "horizontal",
                  choices = c('Database Comparison',
                              'Cohort Comparison',
                              'Dechallenge Rechallenge',
                              'Risk Factors',
                              'Time-to-event',
                              'Case Series',
                              'Cohort Incidence'
                              )[analysesWithResults]
                ),
                # add a note showing what analyses are not available
                shiny::helpText(
                  ifelse(sum(analysesWithResults) != 7,
                  paste0('Note: ', paste0(c('Database Comparison',
                                            'Cohort Comparison',
                                            'Dechallenge Rechallenge',
                                            'Risk Factors',
                                            'Time-to-event',
                                            'Case Series',
                                            'Cohort Incidence'
                  )[analysesWithResults == 0], collapse = '/') ,' not available.'),
                  ''
                  )
                )
              )
            )
            
            # set the resultType to the first 
            resultType(c('Database Comparison',
                         'Cohort Comparison',
                         'Dechallenge Rechallenge',
                         'Risk Factors',
                         'Time-to-event',
                         'Case Series',
                         'Cohort Incidence'
            )[analysesWithResults][1])
            
          } else{
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
 
          outcomeTable(OhdsiReportGenerator::getOutcomeTable(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema, 
            cTablePrefix = resultDatabaseSettings$cTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
            targetId = reactiveTargetRow()$cohortId[1]
          ) %>%
            dplyr::select(-"cohortMethod", -"selfControlledCaseSeries", -"prediction") %>%
            dplyr::relocate("parentName", .before = "cohortName"))
            
        } else{
          outcomeTable(NULL)
        }
          
        } else{ # if no target selected set outcome table to null
          outcomeTable(NULL)
          output$analysesOptions <- NULL
          output$showOutcomeSelector <- shiny::reactive(FALSE)
          resultType("") # update resultType to get UI to change 
        }

      })
      
      # update resultType when input changes
      shiny::observeEvent(input$resultType,{
        resultType(input$resultType)
      })
      
      # listen to the radio seleciton 
      shiny::observeEvent(resultType(),{
        
        # show/hide outcome input depending on what 
        # analysis is selected 
        if(resultType() %in% c(
          'Dechallenge Rechallenge',
          'Risk Factors',
          'Time-to-event',
          'Case Series'
        )){
          output$showOutcomeSelector <- shiny::reactive(TRUE)
        } else{
          output$showOutcomeSelector <- shiny::reactive(FALSE)
        }
        
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

      
      # observe outcomeRow for the exposed cases summary
      # outcomeTable is reactive
      reactiveOutcomeRow <- tableSelectionServer(
          id = 'outcome-table-select',
          table = outcomeTable, 
          selectButtonText = 'Select Outcome', 
          inputColumns = characterizationOutcomeDisplayColumns(),
          tableReset = outcomeTableReset
        )
      
      # find tars and washout when outcome is selected
      reactiveOutcomeTar <- shiny::reactiveVal(
        list(
        tarList = c(),
        tarInds = c()
      ))
      reactiveOutcomeWashout <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        eventExpr = reactiveOutcomeRow(), {
          
          if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow())){
            reactiveOutcomeTar(list(
              tarList = c(),
              tarInds = c()
            ))
            reactiveOutcomeWashout(NULL)
          } else{
            
            if(nrow(reactiveTargetRow()) == 0 | nrow(reactiveOutcomeRow()) == 0){
              reactiveOutcomeTar(list(
                tarList = c(),
                tarInds = c()
              ))
              reactiveOutcomeWashout(NULL)
            } else{
            reactiveOutcomeTar(
              characterizationGetCaseSeriesTars(
                connectionHandler = connectionHandler,
                resultDatabaseSettings = resultDatabaseSettings,
                targetId = reactiveTargetRow()$cohortId,
                outcomeId = reactiveOutcomeRow()$cohortId
              ))
              reactiveOutcomeWashout(
                characterizationGetCaseSeriesWashout(
                  connectionHandler = connectionHandler,
                  resultDatabaseSettings = resultDatabaseSettings,
                  targetId = reactiveTargetRow()$cohortId,
                  outcomeId = reactiveOutcomeRow()$cohortId
                )
              )
            }
          }
        }
        )
    
      # add the servers
      characterizationDatabaseComparisonServer(
        id = 'database-comparison', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow
        )
      characterizationCohortComparisonServer(
        id = 'cohort-comparison', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        targetTable = targetTable,
        reactiveTargetRow = reactiveTargetRow
      )
      
      characterizationTimeToEventServer(
        id = 'time-to-event', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        reactiveOutcomeRow = reactiveOutcomeRow
      )
      
      characterizationDechallengeRechallengeServer(
        id = 'dechal-rechal', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        reactiveOutcomeRow = reactiveOutcomeRow
      )
      
      characterizationRiskFactorServer(
        id = 'risk-factor', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        reactiveOutcomeRow = reactiveOutcomeRow,
        reactiveOutcomeTar = reactiveOutcomeTar,
        reactiveOutcomeWashout = reactiveOutcomeWashout
      )
      
      characterizationCaseSeriesServer(
        id = 'case-series', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        reactiveOutcomeRow = reactiveOutcomeRow,
        reactiveOutcomeTar = reactiveOutcomeTar
      )
      
      characterizationIncidenceServer(
        id = 'incidence', 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings,
        reactiveTargetRow = reactiveTargetRow,
        outcomeTable = outcomeTable
        )
 
    }
  )
}


characterizationTargetInputColumns <- function(){
  return(
    list(
      databaseString  = reactable::colDef(show = FALSE),
      numDatabase = reactable::colDef(show = FALSE),
      minSubjectCount = reactable::colDef(show = FALSE),
      maxSubjectCount = reactable::colDef(show = FALSE),
      minEntryCount = reactable::colDef(show = FALSE),
      maxEntryCount = reactable::colDef(show = FALSE),
      subsetDefinitionJson = reactable::colDef(show = FALSE),
      subsetCohortId = reactable::colDef(show = FALSE),
      cohortId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
        ),
      
      parentName = reactable::colDef(
        name = 'Target',
        minWidth = 150
      ),
      cohortName = reactable::colDef(
        name = 'Subset',
        minWidth = 300
      ),
      subsetParent = reactable::colDef(show = FALSE),
      subsetDefinitionId = reactable::colDef(show = FALSE),
      
      databaseIdString = reactable::colDef(show = FALSE),
      
      databaseStringCount  = reactable::colDef(
        show = FALSE,
        name = "Database Counts", 
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
      )
    )
  )
}


characterizationTargetDisplayColumns <- function(){
  return(
    list(
      databaseString  = reactable::colDef(show = FALSE),
      numDatabase = reactable::colDef(show = FALSE),
      minSubjectCount = reactable::colDef(show = FALSE),
      maxSubjectCount = reactable::colDef(show = FALSE),
      minEntryCount = reactable::colDef(show = FALSE),
      maxEntryCount = reactable::colDef(show = FALSE),
      subsetDefinitionJson = reactable::colDef(show = FALSE),
      subsetCohortId = reactable::colDef(show = FALSE),
      cohortId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
      ),
      
      parentName = reactable::colDef(
        name = 'Target',
        minWidth = 150
      ),
      cohortName = reactable::colDef(
        name = 'Subset',
        minWidth = 300
      ),
      subsetParent = reactable::colDef(show = FALSE),
      subsetDefinitionId = reactable::colDef(show = FALSE),
      
      databaseIdString = reactable::colDef(show = FALSE),
      
      databaseStringCount  = reactable::colDef(
        show = FALSE
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
      ),
      databaseComparator = reactable::colDef(
        show = FALSE
      ),
      cohortComparator = reactable::colDef(
        show = FALSE
      )
    )
  )
}


characterizationOutcomeDisplayColumns <- function(){
  return(
    list(
      databaseString  = reactable::colDef(show = FALSE),
      numDatabase = reactable::colDef(show = FALSE),
      minSubjectCount = reactable::colDef(show = FALSE),
      maxSubjectCount = reactable::colDef(show = FALSE),
      minEntryCount = reactable::colDef(show = FALSE),
      maxEntryCount = reactable::colDef(show = FALSE),
      cohortId = reactable::colDef(
        show = TRUE,
        name = 'Cohort ID'
      ),
      
      parentName = reactable::colDef(
        name = 'Outcome',
        minWidth = 150
      ),
      cohortName = reactable::colDef(
        name = 'Subset',
        minWidth = 300
      ),
      subsetParent = reactable::colDef(show = FALSE),
      subsetDefinitionId = reactable::colDef(show = FALSE),
      
      databaseIdString = reactable::colDef(show = FALSE),
      
      databaseStringCount  = reactable::colDef(
        show = FALSE
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
