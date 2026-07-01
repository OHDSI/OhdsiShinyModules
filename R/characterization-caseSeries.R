# @file characterization-aggregateFeatures.R
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

characterizationCaseSeriesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::helpText('View features that occur before target index, between target index and outcome and after outcome for patients with the outcome during the time-at-risk.'),
    
    # module that does input selection for a single row DF
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    
    shiny::conditionalPanel(
      condition = 'output.showCaseSeries != 0',
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      shinydashboard::tabBox(
        width = "100%",
        # Title can include an icon
        title = shiny::tagList(shiny::icon("gear"), "Case Series"),
        shiny::tabPanel("Binary Feature Table",
                        shiny::uiOutput(outputId = ns('helpTextBinary')),
                        resultTableViewer(ns('binaryTable'))
        ),
        shiny::tabPanel("Continuous Feature Table", 
                        shiny::uiOutput(outputId = ns('helpTextCont')),
                        resultTableViewer(ns('continuousTable'))
        )
      )
    )
  )

}



characterizationCaseSeriesServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    reactiveCharacterizationTargetTable,
    reactiveCharacterizationTargetRowId,
    reactiveOutcomeTable,
    reactiveOutcomeRowId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        targetTable <- reactiveCharacterizationTargetTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(targetTable) || nrow(targetTable) == 0) {
          return(data.frame())
        }
        
        targetTable[rowId, , drop = FALSE]
      })
      
      outcomeTableForSelect <- shiny::reactive({
        out <- reactiveOutcomeTable()
        
        if (is.null(out) || nrow(out) == 0) {
          return(data.frame())
        }
        
        if (!("cohortId" %in% colnames(out)) && ("cohortDefinitionId" %in% colnames(out))) {
          out$cohortId <- out$cohortDefinitionId
        }
        
        out
      })
      
      reactiveSelectedOutcomeRow <- shiny::reactive({
        rowId <- reactiveOutcomeRowId()
        out <- outcomeTableForSelect()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(out) || nrow(out) == 0) {
          return(data.frame())
        }
        
        out[rowId, , drop = FALSE]
      })
      
      tableSelectionServer(
        id = 'char-pop-select-cs',
        table = reactiveCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-cs'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      
      output$showCaseSeries <- shiny::reactive(0)
      shiny::outputOptions(output, "showCaseSeries", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showCaseSeries <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveSelectedOutcomeRow(), {
        output$showCaseSeries <- shiny::reactive(0)
      })
      shiny::observeEvent(input$databaseName, {
        output$showCaseSeries <- shiny::reactive(0)
      })

      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-cs',
        table = outcomeTableForSelect,
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector-cs'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
      
      # query case_settings for char_t_id and outcome_id to get 
      # washout and TAR options (this will give us a case_id to select)
      reactiveOutcomeOptions <- shiny::reactive({
        outcomeRow <- reactiveSelectedOutcomeRow()
        
        if(nrow(reactiveTargetRow()) > 0 && nrow(outcomeRow) > 0 && "cohortId" %in% colnames(outcomeRow)){
          getCharacterizationCaseSettings(
            characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
            outcomeId = outcomeRow$cohortId,
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          )
        } else {
          data.frame()
        }
      })
      
      reactiveOutcomeOptionsRowId <- shiny::reactiveVal(NULL)
      tableSelectionServer(
        id = 'outcome-table-options-cs',
        table = reactiveOutcomeOptions, 
        selectedRowId = reactiveOutcomeOptionsRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('outcome-table-options-cs'),
        #inputColumns = characterizationTargetsColumns(),
        #displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Outcome Options'
      )
      
      
      # get databases
      databaseNames <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = reactiveCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseString, split = ', '))
      })
      databaseIds <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = reactiveCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseIdString, split = ', '))
      })
      
      
      output$inputs <- shiny::renderUI({ # need to make reactive?
        targetRow <- reactiveTargetRow()
        outcomeRow <- reactiveSelectedOutcomeRow()
        outcomeOptions <- reactiveOutcomeOptions()
        outcomeOptionsRowId <- reactiveOutcomeOptionsRowId()

        hasTarget <- !is.null(targetRow) && nrow(targetRow) > 0
        hasOutcome <- !is.null(outcomeRow) && nrow(outcomeRow) > 0
        hasOutcomeOptions <- !is.null(outcomeOptionsRowId) &&
          length(outcomeOptionsRowId) > 0 &&
          all(outcomeOptionsRowId > 0) &&
          !is.null(outcomeOptions) &&
          nrow(outcomeOptions) >= max(outcomeOptionsRowId)
        hasDatabase <- !is.null(input$databaseName) && nzchar(input$databaseName)
        canGenerate <- hasTarget && hasOutcome && hasOutcomeOptions && hasDatabase
        
        shiny::div(
          
          tableSelectionViewer(id = session$ns('char-pop-select-cs')),
          
          tableSelectionViewer(id = session$ns('outcome-table-select-cs')),
          
          tableSelectionViewer(id = session$ns('outcome-table-options-cs')),
          
          shinyWidgets::pickerInput(
            inputId = session$ns('databaseName'),
            label = 'Database: ',
            choices = databaseNames(),
            selected = databaseNames()[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shiny::tags$button(
            id = session$ns('generate'),
            type = 'button',
            class = if (canGenerate) 'btn btn-primary action-button' else 'btn btn-default action-button',
            disabled = if (!canGenerate) 'disabled' else NULL,
            'Generate'
          )
        )
        
      })
      
  
      shiny::observeEvent(input$generate, {
        
        # add target, outcome, database and tar check
        outcomeRow <- reactiveSelectedOutcomeRow()
        outcomeOptions <- reactiveOutcomeOptions()
        outcomeOptionsRowId <- reactiveOutcomeOptionsRowId()
        
        hasOutcomeOptions <- !is.null(outcomeOptionsRowId) &&
          length(outcomeOptionsRowId) > 0 &&
          !is.null(outcomeOptions) &&
          nrow(outcomeOptions) >= max(outcomeOptionsRowId)
        
        if (hasOutcomeOptions) {
          selectedOutcomeOptions <- outcomeOptions[outcomeOptionsRowId, , drop = FALSE]
        } else {
          selectedOutcomeOptions <- data.frame()
        }
        
        
        if(is.null(reactiveTargetRow()) | is.null(outcomeRow) |
           is.null(input$databaseName) | !hasOutcomeOptions
        ){
          
          output$showCaseSeries  <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else if(nrow(reactiveTargetRow()) == 0 | nrow(outcomeRow) == 0 ){
          output$showCaseSeries  <- shiny::reactive(0)
          shiny::showNotification('Need to pick a target and outcome')
        } else{
            
            output$showCaseSeries  <- shiny::reactive(1)
            
            allData <- characterizationGetCaseSeriesData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetIds =  reactiveTargetRow()$characterizationTargetId,
              outcomeIds = outcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              riskWindowStart = selectedOutcomeOptions$riskWindowStart,
              riskWindowEnd = selectedOutcomeOptions$riskWindowEnd,
              startAnchor = selectedOutcomeOptions$startAnchor,
              endAnchor = selectedOutcomeOptions$endAnchor
            )
            
            # !!TODO - replace this?get case count
            counts <- characterizationGetCaseSeriesCounts(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
              outcomeId = outcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              riskWindowStart = selectedOutcomeOptions$riskWindowStart,
              riskWindowEnd = selectedOutcomeOptions$riskWindowEnd,
              startAnchor = selectedOutcomeOptions$startAnchor,
              endAnchor = selectedOutcomeOptions$endAnchor
            )
            N <- counts$personCount[1]
            
            # get the settings to show in the help text
            # minPriorObservation,casePostOutcomeDuration,casePreTargetDuration
            
            # TODO replace this with settings function
            casePreTargetDuration <- 365
            casePostOutcomeDuration <- 365
            
            helpTextValue <- paste0('A summary of what the ',N,' cases had ',casePreTargetDuration,' days before target index ',
                                    ' and up to target index (pre-exposure), after target index and before outcome index (between exposure and outcome) and',
                                    ' from outcome index up to ', casePostOutcomeDuration, 
                                    ' days after outcome index (post-outcome).',
                                    ' Cases are patients in the target cohort',
                                    ' who had the outcome recorded during the time-at-risk period.')
            output$helpTextBinary <- shiny::renderUI(shiny::helpText(helpTextValue))
            output$helpTextCont <- shiny::renderUI(shiny::helpText(helpTextValue))
            
            
            # create the column groups based on data
            caseColGroupsBinary <- list()
            for(colType in c('Before', 'During', 'After')){
              
              colsOfIntTemp <- grep(pattern = colType, x = colnames(allData$binary))
              if( length(colsOfIntTemp) > 0 ){
                if(colType == 'Before'){
                  tempname <- 'Pre-exposure'
                }else if(colType == 'During'){
                  tempname <- 'Between exposure & outcome'
                } else{
                  tempname <- 'Post-outcome'
                }
                caseColGroupsBinary[[length(caseColGroupsBinary) + 1]] <- reactable::colGroup(
                  name = tempname, 
                  columns = colnames(allData$binary)[colsOfIntTemp]
                )
              }
            }
            
            resultTableServer(
              id = "binaryTable", 
              df = tryCatch({allData$binary},  # need to make sure correct case id
              error = function(e){return(NULL)}), 
              details = data.frame(
                Database = input$databaseName,
                TimeAtRisk = selectedOutcomeOptions$tar,
                target = reactiveTargetRow()$cohortName,
                outcome = outcomeRow$cohortName,
                #minPriorObservation = minPriorObservation,
                #casePostOutcomeDuration = casePostOutcomeDuration,
                #casePreTargetDuration = casePreTargetDuration,
                #outcomeWashoutDays = input$outcomeWashout,
                caseN = N,
                description = "Case series binary features before target index, during exposure and after outcome index"
              ),
              downloadedFileName = 'case_series_binary',
              colDefsInput = colDefsBinary(
                elementId = session$ns('binary-table-filter')
              ), # function below
              addActions = NULL,
              elementId = session$ns('binary-table-filter'), 
              
              # only add groups that exist
              columnGroups = caseColGroupsBinary
            )
            
            # create the column groups based on data
            caseColGroupsContinuous <- list()
            for(colType in c('Before', 'During', 'After')){
              
              colsOfIntTemp <- grep(pattern = colType, x = colnames(allData$continuous))
              if( length(colsOfIntTemp) > 0 ){
                if(colType == 'Before'){
                  tempname <- 'Pre-exposure'
                }else if(colType == 'During'){
                  tempname <- 'Between exposure & outcome'
                } else{
                    tempname <- 'Post-outcome'
                  }
                caseColGroupsContinuous[[length(caseColGroupsContinuous) + 1]] <- reactable::colGroup(
                  name = tempname, 
                  columns = colnames(allData$continuous)[colsOfIntTemp]
                )
              }
            }
            
            resultTableServer(
              id = "continuousTable", 
              df = tryCatch({allData$continuous},
                error = function(e){return(NULL)}), 
              details = data.frame(
                Database = input$databaseName,
                TimeAtRisk = selectedOutcomeOptions$tar,
                target = reactiveTargetRow()$cohortName,
                outcome = outcomeRow$cohortName,
                #minPriorObservation = minPriorObservation,
                #casePostOutcomeDuration = casePostOutcomeDuration,
                #casePreTargetDuration = casePreTargetDuration,
                #outcomeWashoutDays = input$outcomeWashout,
                caseN = N,
                description = "Case series continuous features before target index, during exposure and after outcome index"
              ),
              downloadedFileName = 'case_series_continuous',
              colDefsInput = colDefsContinuous(), 
              columnGroups = caseColGroupsContinuous,
              addActions = NULL,
              elementId = session$ns('continuous-table-filter')
            )
            
          }
        
      })
   
  return(invisible(NULL))
    }
  )
}



characterizationGetCaseSeriesData <- function(
  connectionHandler,
  resultDatabaseSettings,
  characterizationTargetIds,
  outcomeIds,
  databaseId,
  riskWindowStart,
  riskWindowEnd,
  startAnchor,
  endAnchor
){
  
  
  shiny::withProgress(message = 'Getting case series data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
    binary <-   OhdsiReportGenerator::getBinaryCaseSeries(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      characterizationTargetId = characterizationTargetIds,
      outcomeId = outcomeIds,
      riskWindowStart = riskWindowStart,
      riskWindowEnd = riskWindowEnd,
      startAnchor = startAnchor,
      endAnchor = endAnchor,
      databaseIds = databaseId
    )
    
    binary <- binary %>%
      dplyr::select(-dplyr::any_of(
        c("databaseId","databaseName",
        "targetName","targetCohortId", 
        "outcomeName", "outcomeCohortId",
        "riskWindowStart", "riskWindowEnd",
        "startAnchor", "endAnchor"
        ))
        ) %>%
      dplyr::relocate(.data$covariateName)
    
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  continuous <- OhdsiReportGenerator::getContinuousCaseSeries(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cTablePrefix = resultDatabaseSettings$cTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    characterizationTargetId = characterizationTargetIds,
    outcomeId = outcomeIds,
    riskWindowStart = riskWindowStart,
    riskWindowEnd = riskWindowEnd,
    startAnchor = startAnchor,
    endAnchor = endAnchor,
    databaseIds = databaseId
  )
  
  continuous <- continuous %>%
    dplyr::select(-dplyr::any_of(
    c("databaseId","databaseName",
      "targetName","targetCohortId", 
      "outcomeName", "outcomeCohortId",
      "riskWindowStart", "riskWindowEnd",
      "startAnchor", "endAnchor",
      "covariateId"
    ))
  ) %>%
    dplyr::relocate(.data$covariateName)
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      binary = binary,
      continuous = continuous
    )
  )
}


colDefsBinary <- function(
    elementId
    ){
  result <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = FALSE
    ),
    targetId = reactable::colDef(
      show = FALSE
    ),
    limitToFirstInNDays = reactable::colDef(show = FALSE),    
    minPriorObservation= reactable::colDef(show = FALSE),
    nestingCohortId = reactable::colDef(show = FALSE),
    nestingName = reactable::colDef(show = FALSE),
    minAge = reactable::colDef(show = FALSE),
    maxAge = reactable::colDef(show = FALSE),
    studyStart= reactable::colDef(show = FALSE),
    studyEnd = reactable::colDef(show = FALSE),
    genderConceptIds= reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(
      show = FALSE
    ),
    limitToFirstInNDays = reactable::colDef(
      show = TRUE
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE,
      header = withTooltip("Min Prior Observation",
                           "Minimum prior observation time (days)"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    outcomeWashoutDays = reactable::colDef(
      show = FALSE,
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    casePostOutcomeDuration = reactable::colDef(
      show = FALSE,
      header = withTooltip("Days Post-outcome Covariate Window",
                           "Number of days after the outcome we look for the covariate"),
      filterable = TRUE
    ), 
    casePreTargetDuration = reactable::colDef(
      show = FALSE,
      header = withTooltip("Days Pre-exposure Covariate Window",
                           "Number of days before the exposure we look for the covariate"),
      filterable = TRUE
    ),
    sumValueBefore = reactable::colDef(
      name = "No.",
      header = withTooltip("No.",
                           "Number of cases with the covariate prior to exposure"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('<', abs(value))
      }
    ), 
    averageValueBefore = reactable::colDef(
      name = "Percent",
      header = withTooltip("Percent",
                           "Percent of cases with the covariate prior to exposure"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    sumValueDuring = reactable::colDef(
      name = "No.",
      header = withTooltip("No.",
                           "Number of cases with the covariate between the exposure and outcome"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('<', abs(value))
      }
    ), 
    averageValueDuring = reactable::colDef(
      name = "Percent",
      header = withTooltip("Percent",
                           "Percent of cases with the covariate between the exposure and outcome"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    sumValueAfter = reactable::colDef(
      name = "No.",
      header = withTooltip("No.",
                           "Number of cases with the covariate after the outcome"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('<', abs(value))
      }
    ), 
    averageValueAfter = reactable::colDef(
      name = "Percent",
      header = withTooltip("Percent",
                           "Percent of cases with the covariate after the outcome"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    
    analysisName = reactable::colDef(
      name = 'Analysis',
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    )
  )
  return(result)
}

colDefsContinuous <- function(){
  result <- list(
    limitToFirstInNDays = reactable::colDef(show = FALSE),    
    minPriorObservation= reactable::colDef(show = FALSE),
    nestingCohortId = reactable::colDef(show = FALSE),
    nestingName = reactable::colDef(show = FALSE),
    minAge = reactable::colDef(show = FALSE),
    maxAge = reactable::colDef(show = FALSE),
    studyStart= reactable::colDef(show = FALSE),
    studyEnd = reactable::colDef(show = FALSE),
    genderConceptIds= reactable::colDef(show = FALSE),
    cohortDefinitionId = reactable::colDef(
      show = FALSE,
      header = withTooltip("Cohort ID",
                           "Unique identifier of the cohort"),
      filterable = TRUE
    ),
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      "Covariate ID",
      show = TRUE
    ),
    targetId = reactable::colDef(
      show = FALSE
    ),
    outcomeId = reactable::colDef(
      show = FALSE
    ),
    limitToFirstInNDays = reactable::colDef(
      show = TRUE
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ), 
    outcomeWashoutDays = reactable::colDef(
      show = FALSE
    ),
    casePostOutcomeDuration = reactable::colDef(
      show = FALSE
    ), 
    casePreTargetDuration = reactable::colDef(
      show = FALSE
    ),
    
    # After
    countValueAfter = reactable::colDef(
      name = "# Cases with Feature",
      header = withTooltip("# Cases with Feature",
                           "Number of cases with the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(!is.null(value)){
          if( value < 0 ){paste("<", abs(value))}else{abs(value)}
        }
      }
    ), 
    minValueAfter = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValueAfter = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValueAfter = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviationAfter = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValueAfter = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    
    countValueDuring = reactable::colDef(
      name = "# Cases with Feature",
      header = withTooltip("# Cases with Feature",
                           "Number of cases with the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE),
      cell = function(value){
        if(!is.null(value)){
         if( value < 0 ){paste("<", abs(value))}else{abs(value)}
        }
      }
    ), 
    minValueDuring = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValueDuring = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValueDuring = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviationDuring = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValueDuring = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    
    countValueBefore = reactable::colDef(
      name = "# Cases with Feature",
      header = withTooltip("# Cases with Feature",
                           "Number of cases with the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE),
      cell = function(value){
        if(!is.null(value)){
          if( value < 0 ){paste("<", abs(value))}else{abs(value)}
        }
      }
    ), 
    minValueBefore = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValueBefore = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValueBefore = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviationBefore = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValueBefore = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    
    
    p10ValueBefore = reactable::colDef(
      name = "10% Value",
      header = withTooltip("10% Value",
                           "10% Value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p10ValueDuring = reactable::colDef(
      name = "10% Value",
      header = withTooltip("10% Value",
                           "10% Value of the covariate during target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p10ValueAfter = reactable::colDef(
      name = "10% Value",
      header = withTooltip("10% Value",
                           "10% Value of the covariate after target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p25ValueBefore = reactable::colDef(
      name = "25% Value",
      header = withTooltip("25% Value",
                           "25% Value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p25ValueDuring = reactable::colDef(
      name = "25% Value",
      header = withTooltip("25% Value",
                           "25% Value of the covariate during target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p25ValueAfter = reactable::colDef(
      name = "25% Value",
      header = withTooltip("25% Value",
                           "25% Value of the covariate after target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p75ValueBefore = reactable::colDef(
      name = "75% Value",
      header = withTooltip("75% Value",
                           "75% Value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p75ValueDuring = reactable::colDef(
      name = "75% Value",
      header = withTooltip("75% Value",
                           "75% Value of the covariate during target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p75ValueAfter = reactable::colDef(
      name = "75% Value",
      header = withTooltip("75% Value",
                           "75% Value of the covariate after target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p90ValueBefore = reactable::colDef(
      name = "90% Value",
      header = withTooltip("90% Value",
                           "90% Value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p90ValueDuring = reactable::colDef(
      name = "90% Value",
      header = withTooltip("90% Value",
                           "90% Value of the covariate during target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    p90ValueAfter = reactable::colDef(
      name = "90% Value",
      header = withTooltip("90% Value",
                           "90% Value of the covariate after target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    )
    
  )
  return(result)
}


characterizationGetCaseSeriesCounts <- function(
    connectionHandler,
    resultDatabaseSettings,
    characterizationTargetId,
    outcomeId,
    databaseId,
    riskWindowStart, 
    riskWindowEnd, 
    startAnchor, 
    endAnchor
){
  
  result <- OhdsiReportGenerator::getCaseCounts(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    cTablePrefix =  resultDatabaseSettings$cTablePrefix, 
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    characterizationTargetIds = characterizationTargetId, 
    outcomeIds = outcomeId, 
    databaseIds = databaseId, 
    riskWindowStart = riskWindowStart, 
    riskWindowEnd = riskWindowEnd, 
    startAnchor = startAnchor, 
    endAnchor = endAnchor
    ) 
  
  if(nrow(result) > 0){
    result <- result %>%
      dplyr::select("minPriorObservation", "outcomeWashoutDays",
                    "rowCount", "personCount")
  }
  
  return(result)
  
}



