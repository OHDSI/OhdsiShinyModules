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
    shiny::tags$style(
      '
      .rf-viewer-shell {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .rf-hero {
        border-radius: 22px;
        padding: 22px 24px;
        background: linear-gradient(135deg, #f8fbff 0%, #eef6ff 45%, #fdfcff 100%);
        border: 1px solid #dbe6f3;
        box-shadow: 0 14px 30px rgba(15, 23, 42, 0.08);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .rf-hero-top {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 8px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        flex-wrap: wrap;
      }
      .rf-hero-icon {
        width: 52px;
        height: 52px;
        border-radius: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #ffffff;
        background: linear-gradient(135deg, #db2777, #f472b6);
        box-shadow: 0 12px 20px rgba(219, 39, 119, 0.24);
        flex: 0 0 auto;
      }
      .rf-hero-title {
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102033;
        margin: 0;
        display: inline-block;
        white-space: nowrap;
      }
      .rf-hero-copy {
        color: #526173;
        margin: 0;
        line-height: 1.5;
        max-width: 880px;
        overflow-wrap: anywhere;
      }
      .rf-hero-top > div:last-child {
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
      }
      .rf-options-box.box {
        border-radius: 18px;
        border-top: 4px solid #2563eb;
        box-shadow: 0 12px 26px rgba(15, 23, 42, 0.06);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .rf-options-box .box-header,
      .rf-results-wrap .box-header {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .rf-options-box .box-title,
      .rf-results-wrap .box-title {
        display: block;
        white-space: normal;
        word-break: break-word;
        overflow-wrap: anywhere;
        max-width: 100%;
      }
      .rf-options-box .box-body {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        background: #f8fbff;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .rf-options-card {
        background: linear-gradient(180deg, #f8fbff 0%, #eef6ff 100%);
        border: 1px solid #dbe6f3;
        border-radius: 16px;
        padding: 14px 16px 8px 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .rf-options-card > div,
      .rf-options-card .shiny-html-output,
      .rf-options-card .table-responsive,
      .rf-options-card .reactable,
      .rf-options-card .rt-table,
      .rf-options-card table {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .rf-options-card .form-group,
      .rf-options-card .form-control,
      .rf-options-card .bootstrap-select,
      .rf-options-card .bootstrap-select > .dropdown-toggle,
      .rf-options-card .dropdown-menu {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        box-sizing: border-box;
      }
      .rf-options-card .bootstrap-select,
      .rf-options-card .bootstrap-select > .dropdown-toggle {
        width: 100% !important;
        max-width: 100% !important;
      }
      .rf-options-card .bootstrap-select .dropdown-menu {
        max-width: 100% !important;
      }
      .rf-options-card .bootstrap-select .dropdown-toggle {
        overflow: hidden;
      }
      .rf-options-card .bootstrap-select .dropdown-toggle .filter-option,
      .rf-options-card .bootstrap-select .dropdown-toggle .filter-option-inner,
      .rf-options-card .bootstrap-select .dropdown-toggle .filter-option-inner-inner {
        max-width: 100% !important;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .rf-results-wrap {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .rf-results-wrap .box {
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 16px 32px rgba(15, 23, 42, 0.08);
        border: 1px solid #dbe5f1;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .rf-results-wrap .box-header {
        background: linear-gradient(135deg, #123a63 0%, #1d4ed8 100%);
        color: #ffffff;
        border-bottom: none;
      }
      .rf-results-wrap .box-title {
        font-weight: 700;
      }
      .rf-results-wrap .nav-tabs,
      .rf-results-wrap .nav-pills {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        border-bottom: none;
      }
      .rf-results-wrap .nav > li {
        float: none;
        margin: 0;
      }
      .rf-results-wrap .nav > li > a {
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        white-space: nowrap;
      }
      .rf-results-wrap .nav-pills > li.active > a,
      .rf-results-wrap .nav-pills > li.active > a:focus,
      .rf-results-wrap .nav-pills > li.active > a:hover {
        background: linear-gradient(135deg, #2563eb 0%, #7c3aed 100%);
        box-shadow: 0 10px 18px rgba(37, 99, 235, 0.22);
      }
      .rf-results-wrap .tab-content,
      .rf-results-wrap .tab-pane {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .rf-results-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      '
    ),
    shiny::div(
      class = 'rf-viewer-shell',
      shiny::div(
        class = 'rf-hero',
        shiny::div(
          class = 'rf-hero-top',
          shiny::div(
            class = 'rf-hero-icon',
            shiny::icon('gear')
          ),
          shiny::div(
            shiny::tags$h2(class = 'rf-hero-title', 'Risk factors'),
            shiny::tags$p(
              class = 'rf-hero-copy',
              'Explore features associated with having or not having the outcome during the time-at-risk, with a cleaner layout for selecting inputs and reviewing binary and continuous results.'
            )
          )
        )
      ),
      shinydashboard::box(
        collapsible = TRUE,
        title = shiny::tagList(shiny::icon('sliders'), 'Analysis options'),
        width = '100%',
        class = 'rf-options-box',
        shiny::div(
          class = 'rf-options-card',
          shiny::uiOutput(ns('inputs'))
        )
      ),
      shiny::conditionalPanel(
        condition = 'output.showRiskFactors != 0',
        ns = ns,
        shiny::div(
          class = 'rf-results-wrap',
          shinydashboard::tabBox(
            width = '100%',
            title = shiny::tagList(shiny::icon('gear'), 'Results'),
            shiny::tabPanel(
              'Binary Features',
              shiny::div(
                class = 'rf-results-panel',
                shiny::uiOutput(outputId = ns('helpTextBinary')),
                resultTableViewer(ns('binaryTable'))
              )
            ),
            shiny::tabPanel(
              'Continuous Features',
              shiny::div(
                class = 'rf-results-panel',
                shiny::uiOutput(outputId = ns('helpTextContinuous')),
                resultTableViewer(ns('continuousTable'))
              )
            )
          )
        )
      )
    )
  )

}



characterizationRiskFactorServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    reactiveCharacterizationTargetTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # moving the selections within module rather than shared across
      # there are two tables in this module population selection to give characterizationTargetId
      # and outcome selection given characterizationTargetId to give characterizationCaseId
      reactiveOutcomeCaseRowId <- shiny::reactiveVal(NULL)
      reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      
      # restrict to populations with risk factor data
      moduleCharacterizationTargetTable <- shiny::reactive({
        targetTable <- reactiveCharacterizationTargetTable()
        if(!is.null(targetTable)){
          targetTable %>%
            dplyr::filter(as.integer(.data$riskFactors) == 1)
        } else{
          NULL
        }
      })
      
      # Reset targetRowId when the table changes to prevent stale indices
      shiny::observeEvent(moduleCharacterizationTargetTable(), {
        reactiveCharacterizationTargetRowId(NULL)
        reactiveOutcomeCaseRowId(NULL)

      })
      
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        targetTable <- moduleCharacterizationTargetTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(targetTable) || nrow(targetTable) == 0) {
          return(data.frame())
        }
        
        targetTable[rowId, , drop = FALSE]
      })

      reactiveOutcomesUsed <- shiny::reactive({
        targetRow <- reactiveTargetRow()

        if (is.null(targetRow) || nrow(targetRow) == 0) {
          return(data.frame())
        }

        caseSettings <- OhdsiReportGenerator::getCharacterizationCaseSettings(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          cTablePrefix = resultDatabaseSettings$cTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          characterizationTargetIds = targetRow$characterizationTargetId
        )

        if (is.null(caseSettings) || nrow(caseSettings) == 0) {
          return(data.frame())
        }

        if (!"outcomeName" %in% colnames(caseSettings)) {
          if ("cohortName" %in% colnames(caseSettings)) {
            caseSettings$outcomeName <- caseSettings$cohortName
          } else {
            caseSettings$outcomeName <- NA_character_
          }
        }

        if (!"outcomeWashoutDays" %in% colnames(caseSettings)) {
          caseSettings$outcomeWashoutDays <- NA_real_
        }

        if (!"characterizationCaseId" %in% colnames(caseSettings)) {
          caseSettings$characterizationCaseId <- NA_real_
        }

        riskWindowStart <- if ("riskWindowStart" %in% colnames(caseSettings)) {
          caseSettings$riskWindowStart
        } else if ("riskWindowStarts" %in% colnames(caseSettings)) {
          caseSettings$riskWindowStarts
        } else {
          NA
        }

        riskWindowEnd <- if ("riskWindowEnd" %in% colnames(caseSettings)) {
          caseSettings$riskWindowEnd
        } else if ("riskWindowEnds" %in% colnames(caseSettings)) {
          caseSettings$riskWindowEnds
        } else {
          NA
        }

        startAnchor <- if ("startAnchor" %in% colnames(caseSettings)) caseSettings$startAnchor else NA
        endAnchor <- if ("endAnchor" %in% colnames(caseSettings)) caseSettings$endAnchor else NA

        caseSettings$tar <- paste0(
          "(", startAnchor, " + ", riskWindowStart,
          ") - (", endAnchor, " + ", riskWindowEnd, ")"
        )

        caseSettings[, c("outcomeName", "outcomeWashoutDays", "tar", "characterizationCaseId"), drop = FALSE]
      })

      # Reset outcomeRowId when the outcomes table changes to prevent stale indices
      shiny::observeEvent(reactiveOutcomesUsed(), {
        reactiveOutcomeCaseRowId(NULL)
      })

      reactiveSelectedOutcomeCaseRow <- shiny::reactive({
        rowId <- reactiveOutcomeCaseRowId()
        outcomesUsed <- reactiveOutcomesUsed()

        if (is.null(rowId) || length(rowId) == 0 || is.null(outcomesUsed) || nrow(outcomesUsed) == 0) {
          return(data.frame())
        }

        outcomesUsed[rowId, , drop = FALSE]
      })
      
      tableSelectionServer(
        id = 'char-pop-select-rf',
        table = moduleCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-rf'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationSelectedTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      output$showRiskFactors <- shiny::reactive(0)
      shiny::outputOptions(output, "showRiskFactors", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showRiskFactors <- shiny::reactive(0)
        reactiveOutcomeCaseRowId(NULL)
      })
      shiny::observeEvent(reactiveSelectedOutcomeCaseRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      shiny::observeEvent(input$databaseName, {
        output$showRiskFactors <- shiny::reactive(0)
      })
      

      # server for outcome + case selection table
      tableSelectionServer(
        id = 'outcome-table-select-risk',
        table = reactiveOutcomesUsed,
        selectedRowId = reactiveOutcomeCaseRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationRiskFactorOutcomeColumns(),
        displayColumns = characterizationRiskFactorOutcomeColumns(),
        selectButtonText = 'Select Outcome / Time-at-risk'
      )
      
      # get databases
      databaseNames <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = moduleCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseString, split = ', '))
      })
      databaseIds <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = moduleCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseIdString, split = ', '))
      })
      
      output$inputs <- shiny::renderUI({ # need to make reactive?
        targetRow <- reactiveTargetRow()
        selectedOutcomeCase <- reactiveSelectedOutcomeCaseRow()
        databaseChoices <- databaseNames()
        selectedDatabase <- shiny::isolate(input$databaseName)

        if (is.null(selectedDatabase) || !(selectedDatabase %in% databaseChoices)) {
          selectedDatabase <- if (length(databaseChoices) > 0) databaseChoices[1] else NULL
        }

        hasTarget <- !is.null(targetRow) && nrow(targetRow) > 0
        hasOutcomeCase <- !is.null(selectedOutcomeCase) && nrow(selectedOutcomeCase) > 0
        hasDatabase <- !is.null(selectedDatabase) && nzchar(selectedDatabase)
        canGenerate <- hasTarget && hasOutcomeCase && hasDatabase
        
        shiny::div( # TODO make this an options box that can be collapsed
          style = 'width: 100%; max-width: 100%; min-width: 0; overflow-x: auto; box-sizing: border-box;',
          tableSelectionViewer(id = session$ns('char-pop-select-rf')),
          
          if (hasTarget) {
            tableSelectionViewer(id = session$ns('outcome-table-select-risk'))
          },

          if (hasOutcomeCase) {
            shiny::selectInput(
              inputId = session$ns('databaseName'),
              label = 'Database: ',
              choices = databaseChoices,
              selected = selectedDatabase,
              multiple = F
            )
          },

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
        # add target, selected case, and database checks
        selectedOutcomeCase <- reactiveSelectedOutcomeCaseRow()
        hasOutcomeCase <- !is.null(selectedOutcomeCase) &&
          nrow(selectedOutcomeCase) > 0 &&
          "characterizationCaseId" %in% colnames(selectedOutcomeCase)

        selectedTimeAtRisk <- NA_character_
        if (hasOutcomeCase) {
          if ("tar" %in% colnames(selectedOutcomeCase)) {
            selectedTimeAtRisk <- selectedOutcomeCase$tar
          } else if (all(c("startAnchor", "riskWindowStart", "endAnchor", "riskWindowEnd") %in% colnames(selectedOutcomeCase))) {
            selectedTimeAtRisk <- paste0(
              "(", selectedOutcomeCase$startAnchor, "+", selectedOutcomeCase$riskWindowStart,
              ") - (", selectedOutcomeCase$endAnchor, "+", selectedOutcomeCase$riskWindowEnd, ")"
            )
          }
        }

        selectedOutcomeName <- NA_character_
        if (hasOutcomeCase) {
          if ("cohortName" %in% colnames(selectedOutcomeCase)) {
            selectedOutcomeName <- selectedOutcomeCase$cohortName
          } else if ("outcomeName" %in% colnames(selectedOutcomeCase)) {
            selectedOutcomeName <- selectedOutcomeCase$outcomeName
          }
        }
        
        if(is.null(reactiveTargetRow()) |
           is.null(input$databaseName) | !hasOutcomeCase
           ){
          
          output$showRiskFactors <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else if(nrow(reactiveTargetRow()) == 0 | nrow(selectedOutcomeCase) == 0 ){
            output$showRiskFactors <- shiny::reactive(0)
            shiny::showNotification('Need to pick a target and outcome/time-at-risk')
        } else{
            
            output$showRiskFactors <- shiny::reactive(1)
            
            caseCount <- OhdsiReportGenerator::getCaseCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              characterizationCaseIds = selectedOutcomeCase$characterizationCaseId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()]
            )
            
            targetCount <- OhdsiReportGenerator::getNonCaseCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              characterizationCaseIds = selectedOutcomeCase$characterizationCaseId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()]
            )
            
            output$helpTextBinary <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts stratified by whether they had the outcome during the time-at-risk with a history of each binary features across databases."))
            )
            output$helpTextContinuous <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts stratified by whether they had the outcome during the time-at-risk with a history of each continuous features across databases."))
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
              characterizationCaseId = selectedOutcomeCase$characterizationCaseId,
              databaseId = databaseIds()[input$databaseName == databaseNames()]
            )
            
            resultTableServer(
              id = "binaryTable", 
              df = tryCatch({allData$binary},
              error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = selectedOutcomeName,
                caseN = caseN,
                nonCaseN = nonCaseN,
                Database = input$databaseName,
                TimeAtRisk = selectedTimeAtRisk,
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
              df = tryCatch({allData$continuous},
                error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = selectedOutcomeName,
                caseN = caseN,
                nonCaseN = nonCaseN,
                Database = input$databaseName,
                TimeAtRisk = selectedTimeAtRisk,
                Analysis = 'Exposed Cases Summary - Risk Factor continuous'
              ),
              downloadedFileName = 'risk_factor_continuous',
              colDefsInput = characteriationRiskFactorContColDefs(
                elementId = session$ns('continuous-table-filter')
              ),
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


characterizationRiskFactorOutcomeColumns <- function() {
  list(
    outcomeName = reactable::colDef(
      name = "Outcome",
      minWidth = 250
    ),
    outcomeWashoutDays = reactable::colDef(
      name = "Outcome Washout Days"
    ),
    tar = reactable::colDef(
      name = "Time-at-risk"
    ),
    characterizationCaseId = reactable::colDef(
      show = FALSE
    )
  )
}


characterizationGetRiskFactorData <- function(
  connectionHandler,
  resultDatabaseSettings,
  characterizationCaseId,
  databaseId
){
  
  shiny::withProgress(message = 'Getting risk factor data', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
    binary <- OhdsiReportGenerator::getBinaryRiskFactors(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      characterizationCaseId = characterizationCaseId,
      databaseId = databaseId, 
      analysisIds = NULL
    )
    
  message(paste0('Extracted ',nrow(binary),' binary RF rows'))
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  continuous <- OhdsiReportGenerator::getContinuousRiskFactors(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    cTablePrefix = resultDatabaseSettings$cTablePrefix, 
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    characterizationCaseId = characterizationCaseId,
    databaseIds = databaseId
  ) 
  
  message(paste0('Extracted ',nrow(continuous),' continuous RF rows'))
  
  binary <- binary %>%
    parseRiskFactorCovariates() %>%
    sortRiskFactorRows()

  continuous <- continuous %>%
    parseRiskFactorCovariates() %>%
    sortRiskFactorRows()
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      binary = binary,
      continuous = continuous
    )
  )
}


sortRiskFactorRows <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"absSmd" %in% colnames(df)) {
    return(df)
  }

  df[order(-df$absSmd, na.last = TRUE), , drop = FALSE]
}


parseRiskFactorCovariates <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"covariateName" %in% colnames(df)) {
    return(df)
  }

  extractDayWindow <- function(covariateNames) {
    start <- rep(NA_real_, length(covariateNames))
    end <- rep(NA_real_, length(covariateNames))

    # FeatureExtraction style: "during day X through Y"
    patternThrough <- "day\\s*(-?[0-9]+)\\s*through\\s*(-?[0-9]+)"
    matchesThrough <- regexec(patternThrough, covariateNames, ignore.case = TRUE)
    capturesThrough <- regmatches(covariateNames, matchesThrough)
    hasThrough <- lengths(capturesThrough) >= 3
    if (any(hasThrough)) {
      start[hasThrough] <- suppressWarnings(as.numeric(vapply(capturesThrough[hasThrough], `[[`, character(1), 2)))
      end[hasThrough] <- suppressWarnings(as.numeric(vapply(capturesThrough[hasThrough], `[[`, character(1), 3)))
    }

    # Alternate wording: "day X to Y"
    patternTo <- "day\\s*(-?[0-9]+)\\s*to\\s*(-?[0-9]+)"
    matchesTo <- regexec(patternTo, covariateNames, ignore.case = TRUE)
    capturesTo <- regmatches(covariateNames, matchesTo)
    hasTo <- lengths(capturesTo) >= 3 & is.na(start)
    if (any(hasTo)) {
      start[hasTo] <- suppressWarnings(as.numeric(vapply(capturesTo[hasTo], `[[`, character(1), 2)))
      end[hasTo] <- suppressWarnings(as.numeric(vapply(capturesTo[hasTo], `[[`, character(1), 3)))
    }

    list(start = start, end = end)
  }

  hasPattern <- !is.na(df$covariateName) & grepl(": ", df$covariateName)
  dayWindow <- extractDayWindow(df$covariateName)
  df$domain <- ifelse(
    hasPattern,
    sub("^([^ ]+).*$", "\\1", df$covariateName),
    NA_character_
  )
  df$start <- ifelse(
    hasPattern,
    dayWindow$start,
    NA_real_
  )
  df$end <- ifelse(
    hasPattern,
    dayWindow$end,
    NA_real_
  )
  df$Covariate <- ifelse(
    hasPattern,
    sub("^.*?:\\s*", "", df$covariateName),
    df$covariateName
  )

  df %>%
    dplyr::relocate(.data$Covariate, .data$domain, .data$start, .data$end, .before = .data$covariateName)
}


characteriationRiskFactorColDefs <- function(
    elementId
    ){
  result <- list(
    Covariate = reactable::colDef(
      name = "Covariate",
      header = withTooltip("Covariate",
                           "Concept name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    domain = reactable::colDef(
      name = "Domain",
      header = withTooltip("Domain",
                           "Clinical domain for the covariate"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          shiny::tags$option(value = "", "All"),
          lapply(sort(unique(values)), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    start = reactable::colDef(
      name = "Start",
      header = withTooltip("Start",
                           "Start of the observed time window when available"),
      filterable = TRUE
    ),
    end = reactable::colDef(
      name = "End",
      header = withTooltip("End",
                           "End of the observed time window when available"),
      filterable = TRUE
    ),
    
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
  elementId
  ){
  result <- list(
    Covariate = reactable::colDef(
      name = "Covariate",
      header = withTooltip("Covariate",
                           "Concept name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    domain = reactable::colDef(
      name = "Domain",
      header = withTooltip("Domain",
                           "Clinical domain for the covariate"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          shiny::tags$option(value = "", "All"),
          lapply(sort(unique(values)), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    start = reactable::colDef(
      name = "Start",
      header = withTooltip("Start",
                           "Start of the observed time window when available"),
      filterable = TRUE
    ),
    end = reactable::colDef(
      name = "End",
      header = withTooltip("End",
                           "End of the observed time window when available"),
      filterable = TRUE
    ),
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
      show = FALSE
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

