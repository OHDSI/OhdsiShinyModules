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
characterizationDatabaseComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::tags$style(
      '
      .db-viewer-shell {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .db-hero {
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
      .db-hero-top {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 8px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        flex-wrap: wrap;
      }
      .db-hero-icon {
        width: 52px;
        height: 52px;
        border-radius: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #ffffff;
        background: linear-gradient(135deg, #0f766e, #14b8a6);
        box-shadow: 0 12px 20px rgba(20, 184, 166, 0.24);
        flex: 0 0 auto;
      }
      .db-hero-title {
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102033;
        margin: 0;
        display: inline-block;
        white-space: nowrap;
      }
      .db-hero-copy {
        color: #526173;
        margin: 0;
        line-height: 1.5;
        max-width: 880px;
        overflow-wrap: anywhere;
      }
      .db-hero-top > div:last-child {
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
      }
      .db-options-box.box {
        border-radius: 18px;
        border-top: 4px solid #2563eb;
        box-shadow: 0 12px 26px rgba(15, 23, 42, 0.06);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .db-options-box .box-header,
      .db-results-card .box-header {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .db-options-box .box-title,
      .db-results-card .box-title {
        display: block;
        white-space: normal;
        word-break: break-word;
        overflow-wrap: anywhere;
        max-width: 100%;
      }
      .db-options-box .box-body {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        background: #f8fbff;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .db-options-card {
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
      .db-options-card > div,
      .db-options-card .shiny-html-output,
      .db-options-card .table-responsive,
      .db-options-card .reactable,
      .db-options-card .rt-table,
      .db-options-card table {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .db-options-card .form-group,
      .db-options-card .bootstrap-select,
      .db-options-card .bootstrap-select > .dropdown-toggle,
      .db-options-card .dropdown-menu {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        box-sizing: border-box;
      }
      .db-options-card .bootstrap-select,
      .db-options-card .bootstrap-select > .dropdown-toggle {
        width: 100% !important;
        max-width: 100% !important;
      }
      .db-options-card .bootstrap-select .dropdown-menu {
        max-width: 100% !important;
      }
      .db-options-card .bootstrap-select .dropdown-toggle {
        overflow: hidden;
      }
      .db-options-card .bootstrap-select .dropdown-toggle .filter-option,
      .db-options-card .bootstrap-select .dropdown-toggle .filter-option-inner,
      .db-options-card .bootstrap-select .dropdown-toggle .filter-option-inner-inner {
        max-width: 100% !important;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .db-results-wrap {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .db-results-wrap .nav-tabs,
      .db-results-wrap .nav-pills {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        border-bottom: none;
      }
      .db-results-wrap .nav > li {
        float: none;
        margin: 0;
      }
      .db-results-wrap .nav > li > a {
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        white-space: nowrap;
      }
      .db-results-wrap .nav-pills > li.active > a,
      .db-results-wrap .nav-pills > li.active > a:focus,
      .db-results-wrap .nav-pills > li.active > a:hover {
        background: linear-gradient(135deg, #2563eb 0%, #7c3aed 100%);
        box-shadow: 0 10px 18px rgba(37, 99, 235, 0.22);
      }
      .db-results-wrap .tab-content,
      .db-results-wrap .tab-pane {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .db-results-card {
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 16px 32px rgba(15, 23, 42, 0.08);
        border: 1px solid #dbe5f1;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .db-results-card .box-header {
        background: linear-gradient(135deg, #123a63 0%, #1d4ed8 100%);
        color: #ffffff;
        border-bottom: none;
      }
      .db-results-card .box-title {
        font-weight: 700;
      }
      .db-results-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .db-plot-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      '
    ),
    shiny::div(
      class = 'db-viewer-shell',
      shiny::div(
        class = 'db-hero',
        shiny::div(
          class = 'db-hero-top',
          shiny::div(
            class = 'db-hero-icon',
            shiny::icon('database')
          ),
          shiny::div(
            shiny::tags$h2(class = 'db-hero-title', 'Database comparison'),
            shiny::tags$p(
              class = 'db-hero-copy',
              'Compare covariates at index between two databases for the same cohort in a cleaner, more polished layout.'
            )
          )
        )
      ),
      shinydashboard::box(
        collapsible = TRUE,
        title = shiny::tagList(shiny::icon('sliders'), 'Analysis options'),
        width = '100%',
        class = 'db-options-box',
        shiny::div(
          class = 'db-options-card',
          shiny::uiOutput(ns('inputs'))
        )
      ),
      shiny::conditionalPanel(
        condition = "output.showResults != 0",
        ns = ns,
        shiny::div(
          class = 'db-results-wrap',
          shiny::tabsetPanel(
            type = 'pills',
            shiny::tabPanel(
              title = 'Binary Table',
              shiny::div(
                class = 'db-results-panel',
                shiny::uiOutput(outputId = ns('helpTextBinary')),
                shinydashboard::box(
                  class = 'db-results-card',
                  width = '100%',
                  title = '',
                  resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
                )
              )
            ),
            shiny::tabPanel(
              title = 'Binary Plot',
              shiny::div(
                class = 'db-plot-panel',
                shiny::helpText('Pick two databases and compare binary features across the databases.'),
                shiny::uiOutput(ns('plotInputs')),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns('scatterPlot'))
                )
              )
            ),
            shiny::tabPanel(
              title = 'Continuous Table',
              shiny::div(
                class = 'db-results-panel',
                shiny::uiOutput(outputId = ns('helpTextContinuous')),
                shinydashboard::box(
                  class = 'db-results-card',
                  width = '100%',
                  title = '',
                  resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
                )
              )
            )
          )
        )
      )
    )
  )
}



characterizationDatabaseComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    reactiveCharacterizationTargetTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # restrict to populations with cohort comp data
      moduleCharacterizationTargetTable <- shiny::reactive({
        if(!is.null(reactiveCharacterizationTargetTable())){
          reactiveCharacterizationTargetTable() %>%
            dplyr::filter(as.integer(.data$databaseComparator) == 1)
        } else{
          NULL
        }
      })
      
      # have the targetRowId be per analysis
      reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      
      # Reset targetRowId when the table changes to prevent stale indices
      shiny::observeEvent(moduleCharacterizationTargetTable(), {
        reactiveCharacterizationTargetRowId(NULL)
      })
      
      
      plotResult <- shiny::reactiveVal(NULL)

      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        targetTable <- moduleCharacterizationTargetTable()

        if (is.null(rowId) || length(rowId) == 0 || is.null(targetTable) || nrow(targetTable) == 0) {
          return(data.frame())
        }

        targetTable[rowId, , drop = FALSE]
      })

      # initially do not show results
      output$showResults <- shiny::reactive(0)
      shiny::outputOptions(output, "showResults", suspendWhenHidden = FALSE)
      
      # if target or other inputs changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showResults <- shiny::reactive(0)
      })
      shiny::observeEvent(input$databaseNames, {
        output$showResults <- shiny::reactive(0)
      })
      
      tableSelectionServer(
        id = 'char-pop-select-db',
        table = moduleCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-db'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationSelectedTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      databaseNames <- shiny::reactive({
        targetTable <- moduleCharacterizationTargetTable()
        rowId <- reactiveCharacterizationTargetRowId()

        if (is.null(targetTable) || nrow(targetTable) == 0 || is.null(rowId) || length(rowId) == 0) {
          return(NULL)
        }

        databaseString <- targetTable[rowId, ]$databaseString
        if (is.null(databaseString) || length(databaseString) == 0 || is.na(databaseString)) {
          return(NULL)
        }

        unlist(strsplit(x = as.character(databaseString), split = ', '))
      })
      databaseIds <- shiny::reactive({
        targetTable <- moduleCharacterizationTargetTable()
        rowId <- reactiveCharacterizationTargetRowId()

        if (is.null(targetTable) || nrow(targetTable) == 0 || is.null(rowId) || length(rowId) == 0) {
          return(NULL)
        }

        databaseIdString <- targetTable[rowId, ]$databaseIdString
        if (is.null(databaseIdString) || length(databaseIdString) == 0 || is.na(databaseIdString)) {
          return(NULL)
        }

        unlist(strsplit(x = as.character(databaseIdString), split = ', '))
      })
      
      # get min char value:
      # set this to the min threshold used in analysis: covariates.min_characterization_mean
      defaultMinThreshold <- 0
      #minCharVal <- 0#getMinCovaraiteThreshold(
        #connectionHandler = connectionHandler,
        #resultDatabaseSettings = resultDatabaseSettings
      #)
      
      # need to add char tar id selection
      output$inputs <- shiny::renderUI({
        hasTarget <- nrow(reactiveTargetRow()) > 0
        databaseChoices <- databaseNames()
        selectedDatabases <- shiny::isolate(input$databaseNames)

        if (!is.null(selectedDatabases) && !is.null(databaseChoices)) {
          selectedDatabases <- selectedDatabases[selectedDatabases %in% databaseChoices]
        }

        if (is.null(selectedDatabases) || length(selectedDatabases) == 0) {
          selectedDatabases <- if (!is.null(databaseChoices) && length(databaseChoices) > 0) {
            databaseChoices[1]
          } else {
            NULL
          }
        }

        hasDatabases <- !is.null(databaseChoices) && length(databaseChoices) > 0
        canGenerate <- hasTarget && hasDatabases
        
        shiny::div(
          style = 'width: 100%; max-width: 100%; min-width: 0; overflow-x: auto; box-sizing: border-box;',
                    
          tableSelectionViewer(session$ns(id = 'char-pop-select-db')),

          if (hasDatabases) {
            shinyWidgets::pickerInput(
              inputId = session$ns('databaseNames'), 
              label = 'Databases: ',
              choices = databaseChoices,
              selected = selectedDatabases,
              multiple = TRUE,
              width = '100%',
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
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
            shiny::helpText('Select a population and at least one database to enable Generate.')
          }
        )
        
      })
      
      
      #get results
      shiny::observeEvent(input$generate,{
        hasTarget <- nrow(reactiveTargetRow()) > 0
        hasDatabases <- !is.null(input$databaseNames) && length(input$databaseNames) > 0

        if (!hasTarget || !hasDatabases) {
          output$showResults <- shiny::reactive(0)

          missingSelections <- c()
          if (!hasTarget) {
            missingSelections <- c(missingSelections, 'population')
          }
          if (!hasDatabases) {
            missingSelections <- c(missingSelections, 'database')
          }

          shiny::showNotification(
            paste0('Please select ', paste(missingSelections, collapse = ' and '), ' before generating.')
          )
          return(invisible(NULL))
        }

        #get results
        selectedDatabases <- input$databaseNames
        
        result <- characterizatonGetCohortData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          characterizationTargetIds = reactiveTargetRow()$characterizationTargetId,
          databaseIds = databaseIds()[databaseNames() %in% input$databaseNames],
          minThreshold = defaultMinThreshold
        )
        
        resultTable <- result$covariates
        resultTable <- resultTable %>%
          parseDatabaseComparisonCovariates()
        countTable <- result$covRef
        
        
        if(is.null(countTable)){
          shiny::showNotification('No covariate data for selected database/s')
          output$showResults <- shiny::reactive(0)
        } else{
          
          # add databaseNames to countTable
          countTable <- merge( 
            x = countTable, 
            y = data.frame(
              databaseId = databaseIds(),
              databaseName = databaseNames()
              ), 
            by = 'databaseId'
          )
          
        output$showResults <- shiny::reactive(1)
          
          output$helpTextBinary <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first exposure in ",reactiveTargetRow()$limitToFirstInNDays[1]," days and requiring ",
                                   reactiveTargetRow()$minPriorObservation[1]," days observation prior to index) with a history of each binary features across databases."))
          )
          output$helpTextContinuous <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first exposure in ",reactiveTargetRow()$limitToFirstInNDays[1]," days and requiring ",
                                   reactiveTargetRow()$minPriorObservation[1]," days observation prior to index) with a history of each continuous features across databases."))
          )
          
          # this will pivot now and needs addressing
          continuous <- characterizatonGetCohortComparisonDataContinuous(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            characterizationTargetIds = reactiveTargetRow()$characterizationTargetId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames]
          )
          
          continuousTable <- continuous$covariates
          continuousTable <- continuousTable %>%
            parseDatabaseComparisonCovariates()
          
          #databaseNamesResult <- result$databaseNames
          
          # figure out the column names and how to present them to reactable
          meanColumns <- lapply(1:nrow(countTable), function(i){
            reactable::colDef(
              name = '%',
              header = withTooltip(
                paste0('%'),
                paste0("The percentage of the target population in database ", countTable$databaseName[i], ' who had the covariate prior.')
              ),
              cell = function(value) {
                if(is.null(value)){value <- -1}
                if(is.na(value)){value <- -1}
                if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
              }
            )
          })
          names(meanColumns) <- unlist(lapply(countTable$id, function(i) paste0('averageValue_',i)))
          
          sumColumns <- lapply(1:nrow(countTable), function(i){
            reactable::colDef(
              name = 'Count',
              header = withTooltip(
                paste0("Count"),
                paste0("The number of people in the target cohort in database ", countTable$databaseName[i], ' who have the covariate prior.')
              ),
              cell = function(value) {
                if(is.null(value)){value <- -1}
                if(is.na(value)){value <- -1}
                if (value >= 0) value else '< min threshold'
              }
            )
          })
          names(sumColumns) <- unlist(lapply(countTable$id, function(i) paste0('sumValue_',i)))
        
          # group columns with the counts
          
          
          groupColumns <- list()
          
          for(i in 1:nrow(countTable)){
            # check columns exist
            columnInResultTable <- length(grep(paste0('sumValue_',countTable$id[i]), colnames(resultTable))) > 0 
            if(columnInResultTable){
              groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
                name = paste0(countTable$databaseName[i], '(N = ',countTable$n[i],')'), 
                columns = c(
                  paste0('sumValue_',countTable$id[i]), 
                  paste0('averageValue_',countTable$id[i]))
              )
            }
          }
          
          # how to add counts here - in details?
          resultTableServer(
            id = 'mainTable',
            df = resultTable,
            details = data.frame(
              Target = reactiveTargetRow()$cohortName,
              Databases = selectedDatabases,
              Analysis = 'Cohort comparison across databases'
            ),
            downloadedFileName = 'database_comparison_binary',
            colDefsInput = append(
              characterizationDatabaseCohortsColumns(
                elementId = session$ns('main-table-filter')
              ),
              append(
                sumColumns,
                meanColumns
              )
            ),
            columnGroups = groupColumns,
            elementId = session$ns('main-table-filter')
          )
          
          
          # create group columns for continuous
          groupColumnsContinuous <- list()
          continuousCols <- characterizationDatabaseCohortsColumnsContinuous(
            elementId = session$ns('continuous-table-filter')
          )
          
          for(i in 1:nrow(countTable)){
            
            # check column is in continousTable
            columnInContinuousTable <- length(grep(paste0('countValue_',countTable$id[i]), colnames(continuousTable))) > 0 
            if(columnInContinuousTable){
              groupColumnsContinuous[[length(groupColumnsContinuous) + 1]] <- reactable::colGroup(
                name = paste0(countTable$databaseName[i], ' (N = ',countTable$n[i],')'), 
                columns = c(
                  paste0('countValue_',countTable$id[i]), 
                  paste0('averageValue_',countTable$id[i]),
                  paste0('standardDeviation_',countTable$id[i]),
                  paste0('medianValue_',countTable$id[i]),
                  paste0('minValue_',countTable$id[i]),
                  paste0('maxValue_',countTable$id[i])
                )
              )
            }
            
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
                filterable = TRUE
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
                name = "StDev",
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
                name = "Min Value",
                header = withTooltip("Min Value",
                                     "Minimum value of the covariate in the cohort"),
                format = reactable::colFormat(digits = 3)
              ),
              maxValue = reactable::colDef(
                name = "Max Value",
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
            names(newCols) <- paste0(names(newCols),'_',countTable$id[i])
            
            continuousCols <- append(
              continuousCols, 
              newCols
              )

          }
          
          
          
          resultTableServer(
            id = 'continuousTable',
            df = continuousTable, 
            details = data.frame(
              Target = reactiveTargetRow()$cohortName,
              Databases = selectedDatabases,
              Analysis = 'Cohort comparison across databases'
            ),
            downloadedFileName = 'database_comparison_cont',
            colDefsInput = continuousCols, 
            columnGroups = groupColumnsContinuous,
            elementId = session$ns('continuous-table-filter')
          )
          
          plotResult(result)
        } # if countTable not NULL
      })
      
      
          
          #scatterplots
          output$plotInputs <- shiny::renderUI({
            shiny::div(
              shiny::fluidRow(
                shiny::column(width = 5,
                              shinyWidgets::pickerInput(
                                inputId = session$ns('xAxis'), 
                                label = 'X-Axis Database: ',
                                choices = unique(plotResult()$covRef$databaseName),
                                selected = unique(plotResult()$covRef$databaseName)[1],
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
                              )
                ),
                shiny::column(width = 5,
                              shinyWidgets::pickerInput(
                                inputId = session$ns('yAxis'), 
                                label = 'Y-Axis Database: ',
                                choices = unique(plotResult()$covRef$databaseName),
                                selected = unique(plotResult()$covRef$databaseName)[2],
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
                              )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                  inputId = session$ns('generatePlot'), 
                  label = 'Generate Plot'
                  )
                )
              )
            )
          })
          
          #plot when generate plot is pressed
          output$scatterPlot <- NULL
          shiny::observeEvent(input$generatePlot,{
            
            # TODO add a check to make sure plotResult() has results
            
            countInd1 <- which.max(plotResult()$covRef$databaseName == input$xAxis)
            countInd2 <- which.max(plotResult()$covRef$databaseName == input$yAxis)
            
            plotData <- plotResult()$covariates %>%
                dplyr::mutate(domain = dplyr::case_when(
                  grepl("condition_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "condition" ~ "Condition",
                  grepl("drug_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "drug" ~ "Drug",
                  grepl("procedure_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "procedure" ~ "Procedure",
                  grepl("measurement_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "measurement" ~ "Measurement",
                  grepl("observation_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "observation" ~ "Observation",
                  grepl("device_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "device" ~ "Device",
                  grepl("cohort_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "cohort" ~ "Cohort",
                  grepl("visit_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "visit" ~ "Visit",
                  .default = "Demographic"
                ))
          
          #plot
          output$scatterPlot <- plotly::renderPlotly({
              
              # TODO - edit this to jsut use plotly...
            
              # Create hover text for plotly
              plotData$hoverText <- paste(
                "Covariate Name:", plotData$covariateName, 
                "<br>", plotResult()$covRef$databaseName[countInd1], ":", scales::percent(plotData[[paste0("averageValue_",plotResult()$covRef$id[countInd1])]]), 
                "<br>", plotResult()$covRef$databaseName[countInd2], ":", scales::percent(plotData[[paste0("averageValue_",plotResult()$covRef$id[countInd2])]])
              )
              
              # Create the scatter plot with the diagonal line (x = y)
              p <- ggplot2::ggplot(plotData, ggplot2::aes_string(x = paste0("averageValue_",plotResult()$covRef$id[countInd1]),
                                                                 y = paste0("averageValue_",plotResult()$covRef$id[countInd2]),
                                                                 color = "domain",
                                                                 text = "hoverText")) +  # Use hoverText for hover labels
                ggplot2::geom_point(size = 2) +    # Smaller point size
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal x=y line in black
                ggplot2::labs(
                  x = paste0(plotResult()$covRef$databaseName[countInd1], " %"),
                  y = paste0(plotResult()$covRef$databaseName[countInd2], " %"),
                  color = "Domain"
                ) +
                ggplot2::theme_minimal() +          # Optional: use a clean theme
                ggplot2::theme(
                  legend.position = "right",        # Position legend as needed
                  axis.title = ggplot2::element_text(size = 12),  # Adjust axis title size
                  axis.text = ggplot2::element_text(size = 10)    # Adjust axis text size
                ) +
                ggplot2::scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as percentage
                ggplot2::scale_y_continuous(labels = scales::percent_format())    # Format y-axis as percentage
              
              # Convert to a plotly object for interactivity
              plotly::ggplotly(p, tooltip = "text")  # Use the custom hover text
            })
          
          
          }) # end generate plot observe event
          
      return(invisible(NULL))
      
    }) #end server
  
}



getMinCovaraiteThreshold <- function(
  connectionHandler,
  resultDatabaseSettings ){
  
  sql <- "select top 1  min_characterization_mean val from
     @schema.@c_table_prefixcovariates;"
  
  res <- tryCatch({connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix
  )}, error = function(e){print(e);return(list(val = 0))})
  
  return(res$val)
}


parseDatabaseComparisonCovariates <- function(df) {
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


characterizationDatabaseCohortsColumns <- function(elementId) {
  res <- list(
    Covariate = reactable::colDef(
      name = "Covariate",
      header = withTooltip("Covariate", "Concept name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    domain = reactable::colDef(
      name = "Domain",
      header = withTooltip("Domain", "Clinical domain for the covariate"),
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
      header = withTooltip("Start", "Start of the observed time window when available"),
      filterable = TRUE
    ),
    end = reactable::colDef(
      name = "End",
      header = withTooltip("End", "End of the observed time window when available"),
      filterable = TRUE
    ),
    covariateName = reactable::colDef(show = FALSE),
    covariateId = reactable::colDef(show = FALSE),
    minPriorObservation = reactable::colDef(show = FALSE),
    limitToFirstInNDays = reactable::colDef(show = FALSE),
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD", "Standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3)
    ),
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD", "Absolute standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) { return rows.filter(function(row) { return row.values[columnId] >= filterValue }) }")
    ),
    analysisName = reactable::colDef(
      name = "Covariate Class",
      header = withTooltip("Covariate Class", "Class/type of the covariate")
    )
  )

  return(res)
}


characterizationDatabaseCohortsColumnsContinuous <- function(elementId) {
  res <- list(
    Covariate = reactable::colDef(
      name = "Covariate",
      header = withTooltip("Covariate", "Concept name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    domain = reactable::colDef(
      name = "Domain",
      header = withTooltip("Domain", "Clinical domain for the covariate"),
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
      header = withTooltip("Start", "Start of the observed time window when available"),
      filterable = TRUE
    ),
    end = reactable::colDef(
      name = "End",
      header = withTooltip("End", "End of the observed time window when available"),
      filterable = TRUE
    ),
    covariateName = reactable::colDef(show = FALSE),
    covariateId = reactable::colDef(show = FALSE),
    minPriorObservation = reactable::colDef(show = FALSE),
    limitToFirstInNDays = reactable::colDef(show = FALSE),
    smd = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD", "Standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    ),
    absSmd = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD", "Absolute standardized mean difference"),
      format = reactable::colFormat(digits = 3),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) { return rows.filter(function(row) { return row.values[columnId] >= filterValue }) }")
    )
  )

  return(res)
}



characterizationTargetsColumns <- function(){
  
  list(
    parentCohortDefinitionId = reactable::colDef(show = FALSE),
    parentCohortName = reactable::colDef(show = FALSE),
    settingId = reactable::colDef(show = FALSE),
    characterizationTargetId = reactable::colDef(show = FALSE),
    cohortDefinitionId = reactable::colDef(name = 'Cohort ID'),
    cohortName = reactable::colDef(name = 'Cohort Name', minWidth = 150),
    limitToFirstInNDays = reactable::colDef(name = 'limitToFirstInNDays'),
    minPriorObservation = reactable::colDef(name = 'minPriorObservation'),
    nestingCohortId = reactable::colDef(name = 'nestingCohortId'),
    nestingName = reactable::colDef(name = 'nestingName', minWidth = 150),
    minAge = reactable::colDef(name = 'minAge'),
    maxAge = reactable::colDef(name = 'maxAge'),
    studyStart = reactable::colDef(name = 'studyStart'),
    studyEnd = reactable::colDef(name = 'studyEnd'),
    genderConceptIds = reactable::colDef(name = 'genderConceptIds'),
    timeToEvent = reactable::colDef(show = FALSE),
    dechalRechal = reactable::colDef(show = FALSE),
    databaseComparator = reactable::colDef(show = FALSE),
    cohortComparator = reactable::colDef(show = FALSE),
    riskFactors = reactable::colDef(show = FALSE),
    caseSeries = reactable::colDef(show = FALSE),
    databaseString = reactable::colDef(show = FALSE),
    databaseIdString = reactable::colDef(show = FALSE)
  )
  
}


characterizationSelectedTargetsColumns <- function(){
  
  list(
    cohortDefinitionId = reactable::colDef(name = 'Cohort ID'),
    cohortName = reactable::colDef(name = 'Cohort Name', minWidth = 150),
    limitToFirstInNDays = reactable::colDef(name = 'limitToFirstInNDays'),
    minPriorObservation = reactable::colDef(name = 'minPriorObservation'),
    nestingCohortId = reactable::colDef(name = 'nestingCohortId'),
    nestingName = reactable::colDef(name = 'nestingName', minWidth = 150),
    minAge = reactable::colDef(name = 'minAge'),
    maxAge = reactable::colDef(name = 'maxAge'),
    studyStart = reactable::colDef(name = 'studyStart'),
    studyEnd = reactable::colDef(name = 'studyEnd'),
    genderConceptIds = reactable::colDef(name = 'genderConceptIds')
  )
  
}

