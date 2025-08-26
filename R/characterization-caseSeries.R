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
    reactiveTargetRow,
    outcomeTable,
    reactiveOutcomeRowId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$showCaseSeries <- shiny::reactive(0)
      shiny::outputOptions(output, "showCaseSeries", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showCaseSeries <- shiny::reactive(0)
      })
      
      
      reactiveOutcomeTar <- shiny::reactiveVal(NULL)
      reactiveOutcomeTarValues <- shiny::reactiveVal(NULL)
      reactiveOutcomeWashout <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(reactiveOutcomeRowId(), {
        output$showCaseSeries <- shiny::reactive(0)
        
        if(reactiveOutcomeRowId() != 0){
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 'character')){
            tarNames <- strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 
              split = ':'
            )[[1]]
          } else{
            tarNames <- NULL
          }
          reactiveOutcomeTar(tarNames)
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$tarStrings, 'character')){
            tarStrings <- lapply(strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$tarStrings, 
              split = ':'
            )[[1]], function(x){
              vals <- strsplit(x = x, split = '/')[[1]]
              list(
                riskWindowStart = vals[1],
                startAnchor = vals[2],
                riskWindowEnd = vals[3],
                endAnchor = vals[4]
              )
            })
          } else{
            tarStrings <- NULL
          }
          reactiveOutcomeTarValues(tarStrings)
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$outcomeWashoutDays, 'character')){
            outcomeWashoutDays <- strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$outcomeWashoutDays, 
              split = ':'
            )[[1]]
          } else{
            outcomeWashoutDays <- NULL
          }
          reactiveOutcomeWashout(outcomeWashoutDays)
          
        } else{
          reactiveOutcomeTar(NULL)
          reactiveOutcomeWashout(NULL)
        }
        
      })
      
      # get databases
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
      
      output$inputs <- shiny::renderUI({ # need to make reactive?
        
        shiny::div(
          
          tableSelectionViewer(id = session$ns('outcome-table-select-case')),
          
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('tarInd'),
            label = 'Time-at-risk: ',
            choices = reactiveOutcomeTar(),
            selected = reactiveOutcomeTar()[1],
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
          
          shiny::selectInput(
            inputId = session$ns('outcomeWashout'),
            label = 'Outcome washout: ',
            choices = reactiveOutcomeWashout(),
            selected = reactiveOutcomeWashout()[1],
            multiple = F
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-case',
        table = shiny::reactive(outcomeTable() %>%
                                  dplyr::select("parentName", "cohortName", "cohortId") %>%
                                  dplyr::relocate("parentName", .before = "cohortName") %>%
                                  dplyr::relocate("cohortId", .after = "cohortName")
        ), 
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
      
      # save the selections
      selected <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$generate, {
        
        reactiveOutcomeRow <- outcomeTable()[reactiveOutcomeRowId(),]
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow) |
           is.null(input$tarInd) | is.null(input$databaseName) |
           ifelse(is.null(input$outcomeWashout),"",input$outcomeWashout) == ""){
          
          output$showCaseSeries  <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else if(nrow(reactiveTargetRow()) == 0 | nrow(reactiveOutcomeRow) == 0){
          output$showCaseSeries  <- shiny::reactive(0)
          shiny::showNotification('Need to pick a target and outcome')
        } else if(
          is.na(reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$startAnchor) |
          is.na(reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$riskWindowStart) |
          is.na(reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$endAnchor) |
          is.na(reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$riskWindowEnd)
        ){
          output$showRiskFactors <- shiny::reactive(0)
          shiny::showNotification('No valid TAR')
        } else{
            
            output$showCaseSeries  <- shiny::reactive(1)
            
            selected(data.frame( 
              Target = reactiveTargetRow()$cohortName,
              Outcome = reactiveOutcomeRow$cohortName,
              Database = input$databaseName,
              `Time-at-risk` = input$tarInd,
              OutcomeWashoutDays = input$outcomeWashout
            ))
            
            inputSelectionDfServer(
              id = 'inputSelected', 
              dataFrameRow = selected,
              ncol = 1
            )
            
            allData <- characterizationGetCaseSeriesData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]
            )
            
            # !!TODO - replace this?get case count
            counts <- characterizationGetCaseSeriesCounts(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]
            )
            N <- counts$personCount[1]
            
            # get the settings to show in the help text
            # minPriorObservation,casePostOutcomeDuration,casePreTargetDuration
            
            minPriorObservation <- allData$binary$minPriorObservation[1]
            casePostOutcomeDuration <- allData$binary$casePostOutcomeDuration[1]
            casePreTargetDuration <- allData$binary$casePreTargetDuration[1]
            
            helpTextValue <- paste0('A summary of what the ',N,' cases had ',casePreTargetDuration,' days before target index ',
                                    ' and up to target index (pre-exposure), after target index and before outcome index (between exposure and outcome) and',
                                    ' from outcome index up to ', casePostOutcomeDuration, 
                                    ' days after outcome index (post-outcome).',
                                    ' Cases are patients in the target cohort for the first time, with',
                                    ' a minimun of ', minPriorObservation, ' days observation prior to target index',
                                    ' and who had the outcome recorded during the time-at-risk period.')
            output$helpTextBinary <- shiny::renderUI(shiny::helpText(helpTextValue))
            output$helpTextCont <- shiny::renderUI(shiny::helpText(helpTextValue))
            
            binTableOutputs <- resultTableServer(
              id = "binaryTable", 
              df = tryCatch({allData$binary %>%
                dplyr::filter(.data$minPriorObservation == !!minPriorObservation) %>%
                dplyr::filter(.data$casePostOutcomeDuration == !!casePostOutcomeDuration) %>%
                dplyr::filter(.data$casePreTargetDuration == !!casePreTargetDuration)},
              error = function(e){return(NULL)}), 
              details = data.frame(
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow$cohortName,
                minPriorObservation = minPriorObservation,
                casePostOutcomeDuration = casePostOutcomeDuration,
                casePreTargetDuration = casePreTargetDuration,
                outcomeWashoutDays = input$outcomeWashout,
                caseN = N,
                description = "Case series binary features before target index, during exposure and after outcome index"
              ),
              downloadedFileName = 'case_series_binary',
              colDefsInput = colDefsBinary(
                elementId = session$ns('binary-table-filter')
              ), # function below
              addActions = NULL,
              elementId = session$ns('binary-table-filter'), 
              columnGroups = list(
                reactable::colGroup(
                  name = paste0('Pre-exposure'), 
                  columns = c(
                    'sumValue_Before',
                    'averageValue_Before'
                    )
              ),
              reactable::colGroup(
                name = paste0('Between exposure & outcome'), 
                columns = c(
                  'sumValue_During',
                  'averageValue_During'
                )
              ),
              reactable::colGroup(
                name = paste0('Post-outcome'), 
                columns = c(
                  'sumValue_After',
                  'averageValue_After'
                )
              )
            )
            )
            
            conTableOutputs <- resultTableServer(
              id = "continuousTable", 
              df = tryCatch({allData$continuous %>%
                dplyr::filter(.data$minPriorObservation == !!minPriorObservation) %>%
                dplyr::filter(.data$casePostOutcomeDuration == !!casePostOutcomeDuration) %>%
                dplyr::filter(.data$casePreTargetDuration == !!casePreTargetDuration)},
                error = function(e){return(NULL)}), 
              details = data.frame(
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow$cohortName,
                minPriorObservation = minPriorObservation,
                casePostOutcomeDuration = casePostOutcomeDuration,
                casePreTargetDuration = casePreTargetDuration,
                outcomeWashoutDays = input$outcomeWashout,
                caseN = N,
                description = "Case series continuous features before target index, during exposure and after outcome index"
              ),
              downloadedFileName = 'case_series_continuous',
              colDefsInput = colDefsContinuous(), 
              columnGroups = list(
                reactable::colGroup(
                  name = paste0('Pre-exposure'), 
                  columns = c(
                    'countValue_Before',
                    'averageValue_Before',
                    'standardDeviation_Before',
                    'medianValue_Before',
                    'minValue_Before',
                    'maxValue_Before'
                  )
                ),
                reactable::colGroup(
                  name = paste0('Between exposure & outcome'), 
                  columns = c(
                    'countValue_During',
                    'averageValue_During',
                    'standardDeviation_During',
                    'medianValue_During',
                    'minValue_During',
                    'maxValue_During'
                  )
                ),
                reactable::colGroup(
                  name = paste0('Post-outcome'), 
                  columns = c(
                    'countValue_After',
                    'averageValue_After',
                    'standardDeviation_After',
                    'medianValue_After',
                    'minValue_After',
                    'maxValue_After'
                  )
                )
              ),
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
  targetId,
  outcomeId,
  databaseId,
  tar
){
  
  
  shiny::withProgress(message = 'Getting case series data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
    binary <-   OhdsiReportGenerator::getBinaryCaseSeries(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      targetId = targetId,
      outcomeId = outcomeId,
      riskWindowStart = tar$riskWindowStart,
      riskWindowEnd = tar$riskWindowEnd,
      startAnchor = tar$startAnchor,
      endAnchor = tar$endAnchor,
      databaseIds = databaseId
    )
    
    binary <- binary %>% tidyr::pivot_wider(
      id_cols = c('covariateName', 'covariateId', 
                  'minPriorObservation', 'outcomeWashoutDays', 'casePostOutcomeDuration',
                  'casePreTargetDuration'), 
      names_from = 'type', 
      values_from = c('sumValue', 'averageValue'), 
      values_fill = 0
        )
  
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  continuous <- OhdsiReportGenerator::getContinuousCaseSeries(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cTablePrefix = resultDatabaseSettings$cTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    targetId = targetId,
    outcomeId = outcomeId,
    riskWindowStart = tar$riskWindowStart,
    riskWindowEnd = tar$riskWindowEnd,
    startAnchor = tar$startAnchor,
    endAnchor = tar$endAnchor,
    databaseIds = databaseId
  )
  
  continuous <- continuous %>% tidyr::pivot_wider(
    id_cols = c('covariateName', 'covariateId', 
                'minPriorObservation', 'outcomeWashoutDays', 'casePostOutcomeDuration',
                'casePreTargetDuration'), 
    names_from = 'type', 
    values_from = c('countValue', 'minValue', 'maxValue',
                    'averageValue', 'standardDeviation', 'medianValue'
    ), 
    values_fill = 0
  )
  
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
    sumValue_Before = reactable::colDef(
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
    averageValue_Before = reactable::colDef(
      name = "Percent",
      header = withTooltip("Percent",
                           "Percent of cases with the covariate prior to exposure"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    sumValue_During = reactable::colDef(
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
    averageValue_During = reactable::colDef(
      name = "Percent",
      header = withTooltip("Percent",
                           "Percent of cases with the covariate between the exposure and outcome"),
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    sumValue_After = reactable::colDef(
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
    averageValue_After = reactable::colDef(
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
    countValue_After = reactable::colDef(
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
    minValue_After = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValue_After = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValue_After = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviation_After = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValue_After = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate after outcome"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    
    countValue_During = reactable::colDef(
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
    minValue_During = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValue_During = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValue_During = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviation_During = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValue_During = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate between target and outcome index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    
    countValue_Before = reactable::colDef(
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
    minValue_Before = reactable::colDef(
      name = "Min Value",
      header = withTooltip("Min Value",
                           "Minimum value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    maxValue_Before = reactable::colDef(
      name = "Max Value",
      header = withTooltip("Max Value",
                           "Maximum value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    averageValue_Before = reactable::colDef(
      name = "Average Value",
      header = withTooltip("Average Value",
                           "Average value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    standardDeviation_Before = reactable::colDef(
      name = "SD",
      header = withTooltip("SD",
                           "Standard deviation of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    medianValue_Before = reactable::colDef(
      name = "Median Value",
      header = withTooltip("Median Value",
                           "Median value of the covariate before target index"),
      format = reactable::colFormat(digits = 2, percent = FALSE)
    )
    
  )
  return(result)
}


# TODO: replace with OhdsiShinyModules
characterizationGetCaseSeriesCounts <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    outcomeId,
    databaseId,
    tar
){
  
  sql <- "SELECT 
          min_prior_observation,	
          outcome_washout_days,
          row_count,
          person_count 
          
          from
          @schema.@c_table_prefixcohort_counts
          where database_id = '@database_id'
          and target_cohort_id = @target_id
          and outcome_cohort_id in (@outcome_id)
          and (risk_window_start = @risk_window_start)
          and (risk_window_end = @risk_window_end)
          and (start_anchor = '@start_anchor')
          and (end_anchor = '@end_anchor')
          and cohort_type in ('Cases')
  ;"
  
  counts <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    database_id = databaseId,
    target_id = targetId,
    outcome_id = outcomeId,
    risk_window_start = tar$riskWindowStart,
    start_anchor = tar$startAnchor,
    risk_window_end = tar$riskWindowEnd,
    end_anchor = tar$endAnchor
  )
  
  return(counts)
  
}



