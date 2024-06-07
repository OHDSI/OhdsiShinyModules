# @file characterization-timeToEvent.R
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


# view two cohorts and compare
characterizationDatabaseComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
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
      condition = "input.generate != 0",
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      # add basic table 
      shiny::tabsetPanel(
        type = 'pills',
        shiny::tabPanel(
          title = 'Counts',
          resultTableViewer(id = ns('countTable'), boxTitle = 'Counts')
        ),
        shiny::tabPanel(
          title = 'Binary',
          resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
        ),
        shiny::tabPanel(
          title = 'Continuous',
          resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
        )
      )
    )
  )
}



characterizationDatabaseComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    options,
    parents,
    parentIndex, # reactive
    subTargetId # reactive
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # TODO react to subTargetId
      inputVals <- shiny::reactive({
        characterizationGetCohortsInputs(
        connectionHandler,
        resultDatabaseSettings,
        targetId = subTargetId
      )})
      
      output$inputs <- shiny::renderUI({
        
        shiny::div(
          shiny::selectInput(
            inputId = session$ns('databaseIds'), 
            label = 'Databases: ',
            choices = inputVals()$databaseIds,
            selected = inputVals()$databaseIds[1],
            multiple = T
          ),
          
          shiny::sliderInput(
            inputId = session$ns('minThreshold'), 
            label = 'Covariate Threshold', 
            min = 0, 
            max = 1, 
            value = 0.01, 
            step = 0.01, 
            ticks = F
            ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      
      # show selected inputs to user
      inputSelectionDfServer(
        id = 'inputSelected', 
        dataFrameRow = selected,
        ncol = 1
      )
      
      #get results
      selected <- shiny::reactiveVal()
      shiny::observeEvent(input$generate,{
        
        if(is.null(input$databaseIds)){
          shiny::showNotification('No databases selected')
          return(NULL)
        }
        if(length(input$databaseIds) == 0 ){
          shiny::showNotification('No databases selected')
          return(NULL)
        }
        
        selectedDatabases <- paste0(
          names(inputVals()$databaseIds)[which(inputVals()$databaseIds %in% input$databaseIds)], 
          collapse =  ','
          )
        
        selected(
          data.frame(
            Databases = selectedDatabases,
            `Minimum Covariate Threshold` = input$minThreshold
          )
        )


        #get results
        results <- list(
          table = data.frame(),
          databaseNames = data.frame(
            id = 1,
            databaseName = 'None'
          )
          )
        continuousTable <- data.frame()
        countTable <- data.frame()
        
        if(length(input$databaseIds) > 0){
          
          countTable <- characterizatonGetCohortCounts(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = subTargetId(),
            databaseIds = input$databaseIds
          )
          
          result <- characterizatonGetDatabaseComparisonData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = subTargetId(),
            databaseIds = input$databaseIds,
            minThreshold = input$minThreshold
          )
          
          continuousTable <- characterizatonGetCohortComparisonDataContinuous(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = subTargetId(),
            databaseIds = input$databaseIds,
            pivot = F
          )
          
        } else{
          shiny::showNotification('No results')
        }
        
        
          databaseNames <- result$databaseNames
          
          meanColumns <- lapply(1:nrow(databaseNames), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0(databaseNames$databaseName[i], ' %'),
                paste0("The percentage of the target population in database ", databaseNames$databaseName[i], ' who had the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
              }
            )
          })
          names(meanColumns) <- unlist(lapply(1:nrow(databaseNames), function(i) paste0('averageValue_',databaseNames$id[i])))
          
          sumColumns <- lapply(1:nrow(databaseNames), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0(databaseNames$databaseName[i], " Count"),
                paste0("The number of people in the target cohort in database ", databaseNames$databaseName[i], ' who have the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) value else '< min threshold'
              }
            )
          })
          names(sumColumns) <- unlist(lapply(1:nrow(databaseNames), function(i) paste0('sumValue_',databaseNames$id[i])))
        
          
          resultTableServer(
            id = 'countTable',
            df = countTable, 
            colDefsInput = characteriationCountTableColDefs(
              elementId = session$ns('count-table-filter')
            ),
            elementId = session$ns('count-table-filter')
          ) 
          resultTableServer(
            id = 'mainTable',
            df = result$table,
            colDefsInput = append(
              characterizationCohortsColumns(
                elementId = session$ns('main-table-filter')
              ),
              append(
                sumColumns,
                meanColumns
              )
            ),
            elementId = session$ns('main-table-filter')
          )
          
          resultTableServer(
            id = 'continuousTable',
            df = continuousTable,
            colDefsInput = characterizationCohortsColumnsContinuous(
              elementId = session$ns('continuous-table-filter')
            ),
            elementId = session$ns('continuous-table-filter')
          )
      })
      
    
      return(invisible(NULL))
      
    })
  
}

characterizatonGetDatabaseComparisonData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    minThreshold
){
  
  result <- characterizatonGetCohortData(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds = targetIds,
    databaseIds = databaseIds,
    minThreshold = minThreshold,
    addSMD = length(databaseIds) == 2
  )
  
  databaseNames <- connectionHandler$queryDb(
    sql = "select cdm_source_abbreviation as database_name, database_id
     from @schema.@database_table;",
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  databaseNames <- merge(
    databaseNames,
    data.frame(
      id = 1:length(databaseIds),
      databaseId = databaseIds
    ), 
    by = 'databaseId'
  )
  
  return(
    list(
      table = result, 
      databaseNames = databaseNames
    )
  )
  
}
