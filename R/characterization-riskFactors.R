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



characterizationRiskFactorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::helpText('View features that are associated with having or not having the outcome during the time-at-risk.'),
    
    # module that does input selection for a single row DF
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    
    
    shiny::conditionalPanel(
      condition = 'output.showRiskFactors != 0',
      ns = ns,
    
      shinydashboard::tabBox(
        width = "100%",
        # Title can include an icon
        title = shiny::tagList(shiny::icon("gear"), "Risk Factors"),
        
        shiny::tabPanel("Binary Features", 
                        shiny::uiOutput(outputId = ns('helpTextBinary')),
                        resultTableViewer(ns('binaryTable'))
        ),
        shiny::tabPanel("Continuous Features", 
                        shiny::uiOutput(outputId = ns('helpTextContinuous')),
                        resultTableViewer(ns('continuousTable'))
        )
      )
    )
  )

}



characterizationRiskFactorServer <- function(
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
        id = 'char-pop-select-rf',
        table = reactiveCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-rf'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      output$showRiskFactors <- shiny::reactive(0)
      shiny::outputOptions(output, "showRiskFactors", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveSelectedOutcomeRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      shiny::observeEvent(input$databaseName, {
        output$showRiskFactors <- shiny::reactive(0)
      })
      

      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-risk',
        table = outcomeTableForSelect,
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
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
        id = 'outcome-table-options',
        table = reactiveOutcomeOptions, 
        selectedRowId = reactiveOutcomeOptionsRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('outcome-table-options'),
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
        
        shiny::div( # TODO make this an options box that can be collapsed
          tableSelectionViewer(id = session$ns('char-pop-select-rf')),
          
          tableSelectionViewer(id = session$ns('outcome-table-select-risk')),
          
          tableSelectionViewer(id = session$ns('outcome-table-options')),
            
          shiny::selectInput(
            inputId = session$ns('databaseName'),
            label = 'Database: ',
            choices = databaseNames(),
            selected = databaseNames()[1],
            multiple = F
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
          
          output$showRiskFactors <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else if(nrow(reactiveTargetRow()) == 0 | nrow(outcomeRow) == 0 ){
            output$showRiskFactors <- shiny::reactive(0)
            shiny::showNotification('Need to pick a target and outcome')
        } else{
            
            output$showRiskFactors <- shiny::reactive(1)
            
            caseCount <- OhdsiReportGenerator::getCaseCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              characterizationTargetIds =  reactiveTargetRow()$characterizationTargetId,
              outcomeIds = outcomeRow$cohortId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()], 
              riskWindowStart = selectedOutcomeOptions$riskWindowStart,
              riskWindowEnd = selectedOutcomeOptions$riskWindowEnd,
              startAnchor = selectedOutcomeOptions$startAnchor,
              endAnchor = selectedOutcomeOptions$endAnchor
            )
            
            targetCount <- OhdsiReportGenerator::getNonCaseCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              characterizationTargetIds =  reactiveTargetRow()$characterizationTargetId,
              outcomeIds = outcomeRow$cohortId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()]
            )
            
            output$helpTextBinary <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts stratified by whether they had the outcome during the time-at-risk with a history of each binary features across databases."))
            )
            output$helpTextContinuous <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts stratified by whether they had the outcome during the time-at-risk with a history of each continuous features across databases."))
            )
            
            caseCount <- caseCount %>% 
              dplyr::filter(
                .data$outcomeWashoutDays == !!selectedOutcomeOptions$outcomeWashoutDays
                )
            caseN <- caseCount$personCount[1]
            
            nonCaseN <- targetCount$personCount[1]

            groupColumns <- list(
              reactable::colGroup(
                name = paste0('Case ', ' (N = ',caseN,')'), 
                columns = c(
                  paste0('caseCount'), 
                  paste0('caseAverage'))
              ),
              reactable::colGroup(
                name = paste0('Non Case ', ' (N = ',nonCaseN,')'), 
                columns = c(
                  paste0('nonCaseCount'), 
                  paste0('nonCaseAverage'))
              )
            )
            
             allData <- characterizationGetRiskFactorData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
              outcomeId = outcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = selectedOutcomeOptions
            )
            
            resultTableServer(
              id = "binaryTable", 
              df = tryCatch({allData$binary %>%
                dplyr::filter(.data$outcomeWashoutDays == !!selectedOutcomeOptions$outcomeWashoutDays)},
              error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = outcomeRow$cohortName,
                caseN = caseN,
                nonCaseN = nonCaseN,
                Database = input$databaseName,
                TimeAtRisk = selectedOutcomeOptions$tar,
                Analysis = 'Exposed Cases Summary - Risk Factor'
              ),
              downloadedFileName = 'risk_factor_binary',
              colDefsInput = characteriationRiskFactorColDefs(
                elementId = session$ns('binary-table-filter')
              ), # function below
              addActions = NULL,
              columnGroups = groupColumns,
              elementId = session$ns('binary-table-filter'), 
            )
            
            groupColumnsContinuous <- list(
              reactable::colGroup(
                name = paste0('Case ', ' (N = ',caseN ,')'), 
                columns = c(
                  paste0('caseCountValue'), 
                  paste0('caseMinValue'), 
                  paste0('caseMaxValue'), 
                  paste0('caseAverageValue'),
                  paste0('caseStandardDeviation'),
                  paste0('caseMedianValue')
                  )
              ),
              reactable::colGroup(
                name = paste0('Target ', ' (N = ',nonCaseN,')'), 
                columns = c(
                  paste0('targetCountValue'), 
                  paste0('targetMinValue'), 
                  paste0('targetMaxValue'), 
                  paste0('targetAverageValue'),
                  paste0('targetStandardDeviation'),
                  paste0('targetMedianValue')
                )
            )
            )
            
            resultTableServer(
              id = "continuousTable", 
              df = tryCatch({allData$continuous %>%
                dplyr::filter(.data$outcomeWashoutDays == !!selectedOutcomeOptions$outcomeWashoutDays)},
                error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = outcomeRow$cohortName,
                caseN = caseN,
                nonCaseN = nonCaseN,
                Database = input$databaseName,
                TimeAtRisk = selectedOutcomeOptions$tar,
                Analysis = 'Exposed Cases Summary - Risk Factor continuous'
              ),
              downloadedFileName = 'risk_factor_continuous',
              colDefsInput = characteriationRiskFactorContColDefs(),
              addActions = NULL,
              columnGroups = groupColumnsContinuous, 
              elementId = session$ns('risk_factor_continuous')
            )
            
          }
        
      })
   
  return(invisible(NULL))
    }
  )
}


getCharacterizationCaseSettings <- function(
    characterizationTargetId,
    outcomeId,
    connectionHandler,
    resultDatabaseSettings
    ){
    
  caseSettings <- OhdsiReportGenerator::getCharacterizationCaseSettings(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    cTablePrefix = resultDatabaseSettings$cTablePrefix, 
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    characterizationTargetIds = characterizationTargetId,
    outcomeIds = outcomeId # need to add as input
  )
  
  caseSettings <- caseSettings %>%
    dplyr::select("characterizationCaseId", "outcomeWashoutDays",
                  "riskWindowStart", "startAnchor", "riskWindowEnd",
                  "endAnchor") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tar = paste0("(", .data$startAnchor ,"+", .data$riskWindowStart, ") - (",
                   .data$endAnchor ,"+", .data$riskWindowEnd, ')' )
    ) 
  
  
  return(caseSettings)
}


characterizationGetRiskFactorData <- function(
  connectionHandler,
  resultDatabaseSettings,
  characterizationTargetId,
  outcomeId,
  databaseId,
  tar
){
  
  shiny::withProgress(message = 'Getting risk factor data', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
    binary <- OhdsiReportGenerator::getBinaryRiskFactors(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      characterizationTargetId = characterizationTargetId, 
      outcomeId = outcomeId, 
      databaseId = databaseId, 
      analysisIds = NULL,
      riskWindowStart = tar$riskWindowStart,
      riskWindowEnd = tar$riskWindowEnd,
      startAnchor = tar$startAnchor,
      endAnchor = tar$endAnchor
    )
    
  message(paste0('Extracted ',nrow(binary),' binary RF rows'))
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  continuous <- OhdsiReportGenerator::getContinuousRiskFactors(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    cTablePrefix = resultDatabaseSettings$cTablePrefix, 
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    characterizationTargetId = characterizationTargetId, 
    outcomeId = outcomeId, 
    databaseId = databaseId,
    riskWindowStart = tar$riskWindowStart,
    riskWindowEnd = tar$riskWindowEnd,
    startAnchor = tar$startAnchor,
    endAnchor = tar$endAnchor
  ) 
  
  message(paste0('Extracted ',nrow(continuous),' continuous RF rows'))
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      binary = binary,
      continuous = continuous
    )
  )
}


characteriationRiskFactorColDefs <- function(
    elementId
    ){
  result <- list(
    
    databaseName = reactable::colDef(
      show = FALSE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(
      show = FALSE
    ),
    targetCohortId = reactable::colDef(
      show = FALSE
    ),
    outcomeName = reactable::colDef(
      show = FALSE
    ),
    outcomeCohortId = reactable::colDef(
      show = FALSE
    ),
    riskWindowStart = reactable::colDef(
      show = FALSE
    ),
    riskWindowEnd = reactable::colDef(
      show = FALSE
    ),
    startAnchor = reactable::colDef(
      show = FALSE
    ),
    endAnchor = reactable::colDef(
      show = FALSE
    ),
    
    covariateId = reactable::colDef(
      show = FALSE
    ),
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = TRUE,
      minWidth = 300
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
    outcomeWashoutDays = reactable::colDef(
      show = FALSE
    ),
    nonCaseCount = reactable::colDef(
      name = "# Non-cases with Feature Before Exposure",
      header = withTooltip("# Non-cases with Feature Before Exposure",
                           "Number of non-cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
        ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    ), 
    caseCount = reactable::colDef(
      name = "# Cases with Feature Before Exposure",
      header = withTooltip("# Cases with Feature Before Exposure",
                           "Number of cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(
        separators = TRUE, 
        percent = FALSE
        ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    ), 
    nonCaseAverage = reactable::colDef(
      name = "% Non-cases with Feature Before Exposure",
      header = withTooltip("% Non-cases with Feature Before Exposure",
                           "Percent of non-cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    caseAverage = reactable::colDef(
      name = "% Cases with Feature Before Exposure",
      header = withTooltip("% Cases with Feature Before Exposure",
                           "Percent of Cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute value of standardized mean difference"),
      format = reactable::colFormat(digits = 2, percent = FALSE),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }"),
      filterInput = function(values, name) {
        oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", elementId, name)
        shiny::tags$input(
          type = "range",
          min = floor(min(values, na.rm = TRUE)),
          max = ceiling(max(values, na.rm = TRUE)),
          value = floor(min(values, na.rm = TRUE)),
          oninput = oninput,
          onchange = oninput, # For IE11 support
          "aria-label" = sprintf("Filter by minimum %s", name)
        )
      }
    )
  )
  return(result)
}



characteriationRiskFactorContColDefs <- function(
    ){
  result <- list(
    databaseName = reactable::colDef(
      show = FALSE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(
      show = FALSE
    ),
    targetCohortId = reactable::colDef(
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
    outcomeName = reactable::colDef(
      show = FALSE
    ),
    outcomeCohortId = reactable::colDef(
      show = FALSE
    ),
    riskWindowStart = reactable::colDef(
      show = FALSE
    ),
    riskWindowEnd = reactable::colDef(
      show = FALSE
    ),
    startAnchor = reactable::colDef(
      show = FALSE
    ),
    endAnchor = reactable::colDef(
      show = FALSE
    ),

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
    outcomeWashoutDays = reactable::colDef(
      show = FALSE
    ),
    
    caseCountValue = reactable::colDef(
      name = "Number",
        header = withTooltip("Number",
                             "Case number with feature"),
        filterable = TRUE
      , 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >=0) value else paste0('< ', abs(value))
      }
    ),
    targetCountValue = reactable::colDef(
      name = "Number",
      header = withTooltip("Number",
                           "Non-case number with feature"),
      filterable = TRUE
      , 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >=0) value else paste0('< ', abs(value))
      }
    ),
    
    caseAverageValue = reactable::colDef(
      name = "Mean",
      header = withTooltip("Mean",
                           "Mean value of the feature in the case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    targetAverageValue = reactable::colDef(
      name = "Mean",
      header = withTooltip("Mean",
                           "Mean value of the feature in the non-case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    caseStandardDeviation = reactable::colDef(
      name = "StDev",
      header = withTooltip("StDev",
                           "Standard deviation of the feature value in the case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    targetStandardDeviation = reactable::colDef(
      name = "StDev",
      header = withTooltip("StDev",
                           "Standard deviation of the feature value in the non-case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    caseMedianValue  = reactable::colDef(
      name = "Median",
      header = withTooltip("Median",
                           "Median of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMedianValue  = reactable::colDef(
      name = "Median",
      header = withTooltip("Median",
                           "Median of the feature value in the non-cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    
    caseP10Value  = reactable::colDef(
      show = FALSE
    ),
    targetP10Value  = reactable::colDef(
      show = FALSE
    ),
    caseP25Value  = reactable::colDef(
      show = FALSE
    ),
    targetP25Value  = reactable::colDef(
      show = FALSE
    ),
    caseP75Value  = reactable::colDef(
      show = FALSE
    ),
    targetP75Value  = reactable::colDef(
      show = FALSE
    ),
    caseP90Value  = reactable::colDef(
      show = FALSE
    ),
    targetP90Value  = reactable::colDef(
      show = FALSE
    ),
    caseMaxValue  = reactable::colDef(
      name = "Max",
      header = withTooltip("Max",
                           "Maximum of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMaxValue  = reactable::colDef(
      name = "Max",
      header = withTooltip("Max",
                           "Maximum of the feature value in the non-cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    caseMinValue  = reactable::colDef(
      name = "Min",
      header = withTooltip("Min",
                           "Minimum of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMinValue  = reactable::colDef(
      name = "Min",
      header = withTooltip("Min",
                           "Minimum of the feature value in the non-cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    
    #targetBoxPlot = reactable::colDef(cell = function(value, index) {
    #  ggplot2::ggplot() +
    #        ggplot2::geom_boxplot(
    #              ggplot2::aes(
    #                x = 1, 
    #                ymin = data$targetMinValue[index], 
    #                lower = data$targetP10Value[index], 
    #                middle = data$targetMedianValue[index], 
    #                upper = data$targetP90Value[index], 
    #                ymax = data$targetMaxValue[index]
    #                ),
    #              stat = "identity"
    #          )
    #}), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #    sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #  sparkline::sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    
    # low_outlier, low_whisker, q1, median, q3, high_whisker, high_outlier
    #sparkline::spk_chr(c(data$targetMinValue[index], data$targetP10Value[index], data$targetP25Value[index], data$targetMedianValue[index], 3, 6, 6), type="box", raw = TRUE, width = 200)
    
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute value of the standardized mean difference"), 
      format = reactable::colFormat(digits = 2, percent = FALSE),
      filterable = TRUE
    )
  )
  return(result)
}

