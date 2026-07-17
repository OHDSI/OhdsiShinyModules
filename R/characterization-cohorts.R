# @file characterization-timeToEvent.R
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


# view two cohorts and compare
characterizationCohortComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::tags$style(
      '
      .cohort-viewer-shell {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .cohort-hero {
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
      .cohort-hero-top {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 8px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        flex-wrap: wrap;
      }
      .cohort-hero-icon {
        width: 52px;
        height: 52px;
        border-radius: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #ffffff;
        background: linear-gradient(135deg, #2563eb, #60a5fa);
        box-shadow: 0 12px 20px rgba(37, 99, 235, 0.24);
        flex: 0 0 auto;
      }
      .cohort-hero-title {
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102033;
        margin: 0;
        display: inline-block;
        white-space: nowrap;
      }
      .cohort-hero-copy {
        color: #526173;
        margin: 0;
        line-height: 1.5;
        max-width: 880px;
        overflow-wrap: anywhere;
      }
      .cohort-hero-top > div:last-child {
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
      }
      .cohort-options-box.box {
        border-radius: 18px;
        border-top: 4px solid #2563eb;
        box-shadow: 0 12px 26px rgba(15, 23, 42, 0.06);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .cohort-options-box .box-header,
      .cohort-results-wrap .box-header {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .cohort-options-box .box-title,
      .cohort-results-wrap .box-title {
        display: block;
        white-space: normal;
        word-break: break-word;
        overflow-wrap: anywhere;
        max-width: 100%;
      }
      .cohort-options-box .box-body {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        background: #f8fbff;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .cohort-options-card {
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
      .cohort-options-card > div,
      .cohort-options-card .shiny-html-output,
      .cohort-options-card .table-responsive,
      .cohort-options-card .reactable,
      .cohort-options-card .rt-table,
      .cohort-options-card table {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .cohort-options-card .form-group,
      .cohort-options-card .bootstrap-select,
      .cohort-options-card .bootstrap-select > .dropdown-toggle,
      .cohort-options-card .dropdown-menu {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        box-sizing: border-box;
      }
      .cohort-options-card .bootstrap-select,
      .cohort-options-card .bootstrap-select > .dropdown-toggle {
        width: 100% !important;
        max-width: 100% !important;
      }
      .cohort-options-card .bootstrap-select .dropdown-menu {
        max-width: 100% !important;
      }
      .cohort-options-card .bootstrap-select .dropdown-toggle {
        overflow: hidden;
      }
      .cohort-options-card .bootstrap-select .dropdown-toggle .filter-option,
      .cohort-options-card .bootstrap-select .dropdown-toggle .filter-option-inner,
      .cohort-options-card .bootstrap-select .dropdown-toggle .filter-option-inner-inner {
        max-width: 100% !important;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .cohort-results-wrap {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .cohort-results-wrap .nav-tabs,
      .cohort-results-wrap .nav-pills {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        border-bottom: none;
      }
      .cohort-results-wrap .nav > li {
        float: none;
        margin: 0;
      }
      .cohort-results-wrap .nav > li > a {
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        white-space: nowrap;
      }
      .cohort-results-wrap .nav-pills > li.active > a,
      .cohort-results-wrap .nav-pills > li.active > a:focus,
      .cohort-results-wrap .nav-pills > li.active > a:hover {
        background: linear-gradient(135deg, #2563eb 0%, #7c3aed 100%);
        box-shadow: 0 10px 18px rgba(37, 99, 235, 0.22);
      }
      .cohort-results-wrap .tab-content,
      .cohort-results-wrap .tab-pane {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .cohort-results-wrap .box {
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 16px 32px rgba(15, 23, 42, 0.08);
        border: 1px solid #dbe5f1;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .cohort-results-wrap .box-header {
        background: linear-gradient(135deg, #8b5a12 0%, #f59e0b 100%);
        color: #ffffff;
        border-bottom: none;
      }
      .cohort-results-wrap .box-title {
        font-weight: 700;
      }
      .cohort-results-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      '
    ),
    shiny::div(
      class = 'cohort-viewer-shell',
      shiny::div(
        class = 'cohort-hero',
        shiny::div(
          class = 'cohort-hero-top',
          shiny::div(
            class = 'cohort-hero-icon',
            shiny::icon('users')
          ),
          shiny::div(
            shiny::tags$h2(class = 'cohort-hero-title', 'Cohort comparison'),
            shiny::tags$p(
              class = 'cohort-hero-copy',
              'Compare covariates at index between two cohorts within the same database in a cleaner, easier-to-scan layout.'
            )
          )
        )
      ),
      shinydashboard::box(
        collapsible = TRUE,
        title = shiny::tagList(shiny::icon('sliders'), 'Analysis options'),
        width = '100%',
        class = 'cohort-options-box',
        shiny::div(
          class = 'cohort-options-card',
          shiny::uiOutput(ns('inputs'))
        )
      ),
      shiny::conditionalPanel(
        condition = 'output.showResults != 0', 
        ns = ns,
        shiny::div(
          class = 'cohort-results-wrap',
          shiny::tabsetPanel(
            type = 'pills',
            shiny::tabPanel(
              title = 'Binary Covariates',
              shiny::div(
                class = 'cohort-results-panel',
                shiny::uiOutput(outputId = ns('helpTextBinary')),
                resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
              )
            ),
            shiny::tabPanel(
              title = 'Continuous Covariates',
              shiny::div(
                class = 'cohort-results-panel',
                shiny::uiOutput(outputId = ns('helpTextContinuous')),
                resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
              )
            )
          )
        )
      )
    )
    )
}



characterizationCohortComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    targetTable,
    reactiveCharacterizationTargetTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # restrict to populations with cohort comp data
      moduleCharacterizationTargetTable <- shiny::reactive({
        if(!is.null(reactiveCharacterizationTargetTable())){
          reactiveCharacterizationTargetTable() %>%
            dplyr::filter(.data$cohortComparator == 1)
        } else{
          NULL
        }
      })
      
    # have the targetRowId be per analysis
      reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      
      # target reactive
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        cTargetTable <- moduleCharacterizationTargetTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(cTargetTable) || nrow(cTargetTable) == 0) {
          return(data.frame())
        }
        
        cTargetTable[rowId, , drop = FALSE]
      })
      
      # comparator reactives:
      #=======================
      comparatorTable <- shiny::reactive({
        targetTable %>%
          dplyr::filter(.data$cohortComparator == 1) %>%
          dplyr::select("cohortName", "cohortDefinitionId")
      })
      reactiveComparatorRowId <- shiny::reactiveVal(NULL)
      
      reactiveComparatorTargetId <- shiny::reactive({
        rowId <- reactiveComparatorRowId()
        cTargetTable <- comparatorTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(cTargetTable) || nrow(cTargetTable) == 0) {
          return(NULL)
        }
        
        cTargetTable$cohortDefinitionId[rowId]
      })
      
      comparatorCharacterizationTable <- shiny::reactiveVal(NULL)
      comparatorCharacterizationTableRowId <- shiny::reactiveVal(NULL)
      shiny::observeEvent(reactiveComparatorTargetId(), {
        comparatorTargetId <- reactiveComparatorTargetId()

        if (!is.null(comparatorTargetId) && length(comparatorTargetId) == 1) {
          res <- getCharacterizationTargetId(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema,
            databaseTable = resultDatabaseSettings$databaseTable,
            targetId = comparatorTargetId,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            cTablePrefix = resultDatabaseSettings$cTablePrefix
          ) %>%
            dplyr::filter(.data$cohortComparator == 1)
          
          comparatorCharacterizationTable(res)
        } else {
          comparatorCharacterizationTable(data.frame())
          comparatorCharacterizationTableRowId(NULL)
        }
      })
      
      reactiveComparatorRow <- shiny::reactive({
        rowId <- comparatorCharacterizationTableRowId()
        cTargetTable <- comparatorCharacterizationTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(cTargetTable) || nrow(cTargetTable) == 0) {
          return(data.frame())
        }
        
        cTargetTable[rowId, , drop = FALSE]
      })
      #=======================
      
      
      # Conditional results updater:
      #=======================
      # initially do not show results
      output$showResults <- shiny::reactive(0)
      shiny::outputOptions(output, "showResults", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showResults <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveComparatorRow(), {
        output$showResults <- shiny::reactive(0)
      })
      shiny::observeEvent(input$databaseName, {
        output$showResults <- shiny::reactive(0)
      })
      #=======================
      
      tableSelectionServer(
        id = 'char-pop-select-cohorts',
        table = moduleCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-cohorts'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      # get the databases that the target cohort has data in
      databaseNames <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = moduleCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseString, split = ', '))
      })
      databaseIds <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = moduleCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseIdString, split = ', '))
      })
      
      # add the server for the comparator table select

      tableSelectionServer(
        id = 'comparator-selector', 
        table = comparatorTable, 
        selectedRowId = reactiveComparatorRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('comp-selector'),
        inputColumns = list(
          cohortName = reactable::colDef(
            name = 'Cohort Name',
            minWidth = 300
          ),
          cohortDefinitionId = reactable::colDef(
            show = TRUE,
            name = 'Cohort ID'
          )
        ),
        selectButtonText = 'Select Comparator'
      )
      
      tableSelectionServer(
        id = 'comparator-pop-selector', 
        table = comparatorCharacterizationTable, 
        selectedRowId = comparatorCharacterizationTableRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('comp-pop-selector'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Comparator Population'
      )
      
      
      # initial comp chilren
      output$inputs <- shiny::renderUI({
        hasTarget <- nrow(reactiveTargetRow()) > 0
        comparatorSelected <- reactiveComparatorRowId()
        showComparatorPopulation <- !is.null(comparatorSelected) &&
          length(comparatorSelected) > 0 &&
          all(comparatorSelected > 0)
        hasComparator <- nrow(reactiveComparatorRow()) > 0
        hasDatabase <- !is.null(input$databaseName) && nzchar(input$databaseName)
        canGenerate <- hasTarget && hasComparator && hasDatabase
        
        shiny::div(
          style = 'width: 100%; max-width: 100%; min-width: 0; overflow-x: auto; box-sizing: border-box;',
          
          tableSelectionViewer(id = session$ns('char-pop-select-cohorts')),

          if (hasTarget) {
            tableSelectionViewer(id = session$ns('comparator-selector'))
          },

          if (hasTarget && showComparatorPopulation) {
            tableSelectionViewer(id = session$ns('comparator-pop-selector'))
          },

          if (hasTarget && hasComparator) {
            shinyWidgets::pickerInput(
              inputId = session$ns('databaseName'),
              label = 'Database: ',
              choices = databaseNames(),
              selected = input$databaseName,
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                dropupAuto = FALSE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 500
              )
            )
          },

          shiny::tags$button(
            id = session$ns('generate'),
            type = 'button',
            class = if (canGenerate) 'btn btn-primary action-button' else 'btn btn-default action-button',
            disabled = if (!canGenerate) 'disabled' else NULL,
            'Generate'
          ),

          if (!canGenerate) {
            shiny::helpText('Select a population, comparator, and database to enable Generate.')
          }
        )
      
      })
      
      #get results
      shiny::observeEvent(input$generate,{
        hasTarget <- nrow(reactiveTargetRow()) > 0
        hasComparator <- nrow(reactiveComparatorRow()) > 0
        hasDatabase <- !is.null(input$databaseName) && nzchar(input$databaseName)

        if (!hasTarget || !hasComparator || !hasDatabase) {
          output$showResults <- shiny::reactive(0)
          shiny::showNotification('Must select a population, comparator, and database')
          return(invisible(NULL))
        }
      
        # TODO update logic for running 
        if (nrow(reactiveTargetRow()) == 0 || nrow(reactiveComparatorRow()) == 0) {
          output$showResults <- shiny::reactive(0)
          shiny::showNotification('Must select a comparison')
        } else{
          if (nrow(reactiveTargetRow()) > 0 && nrow(reactiveComparatorRow()) > 0) {
            
            result <- characterizatonGetCohortData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetIds = c(
                reactiveTargetRow()$characterizationTargetId,
                reactiveComparatorRow()$characterizationTargetId
              ),
              databaseIds = databaseIds()[databaseNames() == input$databaseName],
              minThreshold = 0
            )
            resultTable <- result$covariates %>%
              parseCohortComparisonCovariates()
            countTable <- result$covRef
            
            # if no results in database
            if(is.null(countTable)){
              shiny::showNotification('No covariate data for selected database')
              output$showResults <- shiny::reactive(0)
            } else if(nrow(countTable) == 1){
              shiny::showNotification(paste0('Unable to compare as only cohort ', unique(countTable$cohortName) ,' has covariate data in selected database.'))
              output$showResults <- shiny::reactive(0)
            } else{
              output$showResults <- shiny::reactive(1)
          
            output$helpTextBinary <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts with a history of each binary features across databases."))
            )
            output$helpTextContinuous <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts with a history of each continuous features across databases."))
            )
            
            continuous <- characterizatonGetCohortComparisonDataContinuous(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetIds = c(
                reactiveTargetRow()$characterizationTargetId,
                reactiveComparatorRow()$characterizationTargetId
              ),
              databaseIds = databaseIds()[databaseNames() == input$databaseName]
            )
            
            continuousTable <- continuous$covariates %>%
              parseCohortComparisonCovariates()
            
            getDbCount <- function(characterizationTargetId){
              countOfInt <- countTable %>% 
                dplyr::filter(.data$characterizationTargetId == !!characterizationTargetId)
              
              return(countOfInt)
            }
            
            groupColumns <- list()
            
            targetRows <- getDbCount(reactiveTargetRow()$characterizationTargetId)
            for (j in seq_len(nrow(targetRows))) {
              # only group columns that exist
              incGroup <- paste0('sumValue_',targetRows$id[j]) %in% colnames(resultTable)
              if(incGroup){
                groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
                  name = paste0('Target (N = ',targetRows$n[j],')'), 
                  columns = c(
                    paste0('sumValue_',targetRows$id[j]), 
                    paste0('averageValue_',targetRows$id[j]))
                )
              }
            }
            compRows <- getDbCount(reactiveComparatorRow()$characterizationTargetId)
            for (j in seq_len(nrow(compRows))) {
              # only group columns that exist
              incGroup <- paste0('sumValue_',compRows$id[j]) %in% colnames(resultTable)
              if(incGroup){
                groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
                  name = paste0('Comparator (N = ',compRows$n[j],')'), 
                  columns = c(
                    paste0('sumValue_',compRows$id[j]), 
                    paste0('averageValue_',compRows$id[j]))
                )
              }
            }
            
            # figure out the column names and how to present them to reactable
            binColumns <- list()
            
            # Only add column definitions for columns that exist in resultTable
            if ("averageValue_1" %in% colnames(resultTable)) {
              binColumns$averageValue_1 <- reactable::colDef(
                name = '%',
                header = withTooltip(
                  paste0('%'),
                  paste0("The percentage of the target population in database who had the covariate prior.")
                ),
                cell = function(value) {
                  if(is.null(value)){value <- -1}
                  if(is.na(value)){value <- -1}
                  if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
                }
              )
            }
            
            if ("averageValue_2" %in% colnames(resultTable)) {
              binColumns$averageValue_2 <- reactable::colDef(
                name = '%',
                header = withTooltip(
                  paste0('%'),
                  paste0("The percentage of the comparator population in database who had the covariate prior.")
                ),
                cell = function(value) {
                  if(is.null(value)){value <- -1}
                  if(is.na(value)){value <- -1}
                  if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
                }
              )
            }
            
            if ("sumValue_1" %in% colnames(resultTable)) {
              binColumns$sumValue_1 <- reactable::colDef(
                name = 'Count',
                header = withTooltip(
                  paste0("Count"),
                  paste0("The number of people in the target cohort in database who have the covariate prior.")
                ),
                cell = function(value) {
                  if(is.null(value)){value <- -1}
                  if(is.na(value)){value <- -1}
                  if (value >= 0) value else '< min threshold'
                }
              )
            }
            
            if ("sumValue_2" %in% colnames(resultTable)) {
              binColumns$sumValue_2 <- reactable::colDef(
                name = 'Count',
                header = withTooltip(
                  paste0("Count"),
                  paste0("The number of people in the comparator cohort in database who have the covariate prior.")
                ),
                cell = function(value) {
                  if(is.null(value)){value <- -1}
                  if(is.na(value)){value <- -1}
                  if (value >= 0) value else '< min threshold'
                }
              )
            }
            
            resultTableServer(
              id = 'mainTable',
              df = resultTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_binary',
              colDefsInput = append(
                characterizationCohortsColumns(elementId = session$ns('main-table-filter')),
                binColumns
              ), 
              columnGroups = groupColumns,
              elementId = session$ns('main-table-filter')
            ) 
            
            
            # column formatting for continuous
            # create group columns for continuous
            groupColumnsContinuous <- list()
            
            for (k in seq_len(nrow(countTable))) {
              # Build expected column names for this cohort
              cohortCols <- c(
                paste0('countValue_', k),
                paste0('averageValue_', k),
                paste0('standardDeviation_', k),
                paste0('medianValue_', k),
                paste0('minValue_', k),
                paste0('maxValue_', k)
              )
              
              # Only add colGroup if columns exist in continuousTable
              if (all(cohortCols %in% colnames(continuousTable))) {
                groupColumnsContinuous[[length(groupColumnsContinuous) + 1]] <- reactable::colGroup(
                  name = paste0(ifelse(k == 1, 'Target', 'Comparator'), ' (N = ', countTable$n[k], ')'),
                  columns = cohortCols
                )
              }
            }
              
              continuousCols <- characterizationCohortsColumnsContinuous(
                elementId = session$ns('continuous-table-filter')
              )
              
              for(i in seq_len(nrow(countTable))){
                # Check if columns for this cohort exist in continuousTable
                
                # Only add column defs if main columns exist
                if (all(c(
                  paste0('countValue_', i),
                  paste0('averageValue_', i),
                  paste0('standardDeviation_', i),
                  paste0('medianValue_', i),
                  paste0('minValue_', i),
                  paste0('maxValue_', i)
                ) %in% colnames(continuousTable))) {
                  
                  newCols <- list(
                    countValue = reactable::colDef(
                      name = 'Count',
                      header = withTooltip("Count",
                                           "Number of people with the covariate in the cohort."),
                      cell = function(value) {
                        if(is.null(value)){value <- -1}
                        if(is.na(value)){value <- -1}
                        if (value >= 0) value else paste0('< ', abs(value))
                      },
                      filterable = T
                    ),
                    averageValue = reactable::colDef(
                      name = 'Mean',
                      header = withTooltip("Mean",
                                           "The mean value of the covariate in the cohort"),
                      cell = function(value) {
                        if(is.null(value)){value <- -1}
                        if(is.na(value)){value <- -1}
                        if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
                      }
                    ),
                    standardDeviation = reactable::colDef(
                      name = 'StDev',
                      header = withTooltip("StDev",
                                           "The standard deviation value of the covariate in the cohort"),
                      cell = function(value) {
                        if(is.null(value)){value <- -1}
                        if(is.na(value)){value <- -1}
                        if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
                      }
                    ),
                    medianValue = reactable::colDef(
                      name = 'Median',
                      header = withTooltip("Median",
                                           "The median value of the covariate in the cohort."),
                      cell = function(value) {
                        round(value, digits = 3)
                      }
                    ),
                    minValue = reactable::colDef(
                      name = 'Min Value',
                      header = withTooltip("Min Value",
                                           "Minimum value of the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    ),
                    maxValue = reactable::colDef(
                      name = 'Max Value',
                      header = withTooltip("Max Value",
                                           "Maximum value the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    ),
                    p25Value = reactable::colDef(
                      show = FALSE,
                      header = withTooltip("25th %tile",
                                           "25th percentile value of the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    ),
                    p75Value = reactable::colDef(
                      show = FALSE,
                      header = withTooltip("75th %tile",
                                           "75th percentile value of the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    ),
                    p10Value = reactable::colDef(
                      show = FALSE,
                      header = withTooltip("10th %tile",
                                           "10th percentile value of the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    ),
                    p90Value = reactable::colDef(
                      show = FALSE,
                      header = withTooltip("90th %tile",
                                           "90th percentile value of the covariate in the cohort"),
                      format = reactable::colFormat(digits = 3)
                    )
                  )
                  names(newCols) <- paste0(names(newCols),'_',i)
                  
                  continuousCols <- append(
                    continuousCols, 
                    newCols
                  )
                }
              }
              
            resultTableServer(
              id = 'continuousTable',
              df = continuousTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_cont',
              colDefsInput = continuousCols, 
              columnGroups = groupColumnsContinuous,
              elementId = session$ns('continuous-table-filter')
            ) 

          } # end if counts not NULL
          } else{
            shiny::showNotification('Must select a comparison and target cohort')
            output$showCohortComp <- shiny::reactive(0)
          }
        } 
        
      })
      
      return(invisible(NULL))
      
    })
  
}


characterizationCohortsColumns <- function(
    elementId 
    ){
  
  res <- list(
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
    covariateName = reactable::colDef(
      show = FALSE
    ),
    covariateId = reactable::colDef(
      show = FALSE,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ), 
    limitToFirstInNDays = reactable::colDef(
      show = FALSE
    ), 
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3)
    ),
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
    ),
    analysisName = reactable::colDef(
      name = "Covariate Class",
      header = withTooltip(
        "Covariate Class",
        "Class/type of the covariate"
      )
    )
  )

  return(res)
}


characterizationCohortsColumnsContinuous <- function(elementId){
  res <- list(
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
    covariateName = reactable::colDef(
      show = FALSE
    ),
    covariateId = reactable::colDef(
      show = FALSE,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ),
    limitToFirstInNDays = reactable::colDef(
      show = FALSE,
      filterable = TRUE
    ), 
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    ),
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute standardized mean difference"),
      format = reactable::colFormat(digits = 3),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
    )
  )
  
  return(res)
}


parseCohortComparisonCovariates <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"covariateName" %in% colnames(df)) {
    return(df)
  }

  extractDayWindow <- function(covariateNames) {
    start <- rep(NA_real_, length(covariateNames))
    end <- rep(NA_real_, length(covariateNames))

    patternThrough <- "day\\s*(-?[0-9]+)\\s*through\\s*(-?[0-9]+)"
    matchesThrough <- regexec(patternThrough, covariateNames, ignore.case = TRUE)
    capturesThrough <- regmatches(covariateNames, matchesThrough)
    hasThrough <- lengths(capturesThrough) >= 3
    if (any(hasThrough)) {
      start[hasThrough] <- suppressWarnings(as.numeric(vapply(capturesThrough[hasThrough], `[[`, character(1), 2)))
      end[hasThrough] <- suppressWarnings(as.numeric(vapply(capturesThrough[hasThrough], `[[`, character(1), 3)))
    }

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


characterizatonGetCohortData <- function(
    connectionHandler,
    resultDatabaseSettings,
    characterizationTargetIds,
    databaseIds,
    minThreshold = 0.01
){
  
  shiny::withProgress(message = 'characterizatonGetCohortData', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Checking inputs"))
    
  
  if(is.null(characterizationTargetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
   return(NULL)
  }
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    result <- OhdsiReportGenerator::characterizationCompareBinary(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      characterizationTargetIds = characterizationTargetIds,
      databaseIds = databaseIds,
      minThreshold = minThreshold
    )

    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
    return(result)
  
}


characterizatonGetCohortComparisonDataContinuous <- function(
  connectionHandler,
  resultDatabaseSettings,
  characterizationTargetIds,
  databaseIds,
  minThreshold = 0.01
){
  
  shiny::withProgress(message = 'characterizatonGetCohortDataContinuous', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Checking inputs"))
    

  if(is.null(characterizationTargetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
    return(NULL)
  }
    
    characterizationTargetIds <- unique(characterizationTargetIds)
    databaseIds <- unique(databaseIds)
  
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    result <- OhdsiReportGenerator::characterizationCompareContinuous(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      characterizationTargetIds = characterizationTargetIds,
      databaseIds = databaseIds,
      minThreshold = minThreshold
    )
    
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(result)
}

