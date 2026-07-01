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
  
    # module that does input selection for a single row DF
    shiny::div(
      
      shiny::helpText('Compare covariates at index between two cohorts within the same database.'),
      
      # UI for inputs
      # summary table
      shinydashboard::box(
        collapsible = TRUE,
        title = "Options",
        width = "100%",
        shiny::uiOutput(ns("inputs"))
      ),
      
      # displayed inputs
      shiny::conditionalPanel(
        condition = "output.showResults != 0", 
        ns = ns,
        
        # add basic table 
        shiny::tabsetPanel(
          type = 'pills',
          
          shiny::tabPanel(
            title = 'Binary Covariates',
            shiny::uiOutput(outputId = ns('helpTextBinary')),
            resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
          ),
          
          shiny::tabPanel(
            title = 'Continuous Covariates',
            shiny::uiOutput(outputId = ns('helpTextContinuous')),
            resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
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
    reactiveCharacterizationTargetTable,
    reactiveCharacterizationTargetRowId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # target reactive
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        cTargetTable <- reactiveCharacterizationTargetTable()
        
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
          )
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
        table = reactiveCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-cohorts'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Target Population'
      )
      
      # get the databases that the target cohort has data in
      databaseNames <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = reactiveCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseString, split = ', '))
      })
      databaseIds <- shiny::reactive({
        if(length(reactiveCharacterizationTargetRowId()) == 0){return(NULL)}
        unlist(strsplit(x = reactiveCharacterizationTargetTable()[reactiveCharacterizationTargetRowId(),]$databaseIdString, split = ', '))
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
        comparatorSelected <- reactiveComparatorRowId()
        showComparatorPopulation <- !is.null(comparatorSelected) &&
          length(comparatorSelected) > 0 &&
          all(comparatorSelected > 0)
        
        shiny::div(
          
          tableSelectionViewer(id = session$ns('char-pop-select-cohorts')),
          tableSelectionViewer(id = session$ns('comparator-selector')),
          if (showComparatorPopulation) {
            tableSelectionViewer(id = session$ns('comparator-pop-selector'))
          },
          
        shinyWidgets::pickerInput(
          inputId = session$ns('databaseName'),
          label = 'Database: ',
          choices = databaseNames(),
          selected = databaseNames(),
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
        ),
        
        shiny::actionButton(
          inputId = session$ns('generate'), 
          label = 'Generate'
            )
      )
      
      })
      
      #get results
      shiny::observeEvent(input$generate,{
      
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
            resultTable <- result$covariates
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
            
            continuousTable <- continuous$covariates
            
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
              
              continuousCols <- characterizationCohortsColumnsContinuous()
              
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
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      minWidth = 300
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


characterizationCohortsColumnsContinuous <- function(){
  res <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      filterable = T, 
      minWidth = 300,
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

