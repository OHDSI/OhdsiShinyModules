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
      resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
      
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
            minTreshold = input$minThreshold
          )
        )


        #get results
        #if(sum(selectedChildChar$databaseId %in% input$databaseIds) > 0){
        if(length(input$databaseIds) > 0){
          result <- characterizatonGetDatabaseComparisonData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = subTargetId(),
            databaseIds = input$databaseIds,
            minThreshold = input$minThreshold
          )
          databaseNames <- result$databaseNames
          
          meanColumns <- lapply(1:nrow(databaseNames), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0("Mean ", databaseNames$databaseName[i]),
                paste0("The mean of the covariate for database ", databaseNames$databaseName[i])
              ),
              cell = function(value) {
                if (value >= 0) round(value, digits = 3) else '< min threshold'
              }
            )
          })
          names(meanColumns) <- unlist(lapply(1:nrow(databaseNames), function(i) paste0('averageValue_',databaseNames$id[i])))
          
          sumColumns <- lapply(1:nrow(databaseNames), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0("Sum ", databaseNames$databaseName[i]),
                paste0("The sums of the covariate for database ", databaseNames$databaseName[i])
              ),
              cell = function(value) {
                if (value >= 0) value else '< min threshold'
              }
            )
          })
          names(sumColumns) <- unlist(lapply(1:nrow(databaseNames), function(i) paste0('sumValue_',databaseNames$id[i])))
          
          columns <- append(
            list(
              covariateName = reactable::colDef(
                header = withTooltip(
                  "Covariate Name",
                  "The name of the covariate"
                )
              ),
              covariateId = reactable::colDef(
                show = F,
                header = withTooltip("Covariate ID",
                                     "Unique identifier of the covariate")
              )
            ),
            append(
              sumColumns,
              meanColumns
            )
          )
          
          resultTableServer(
            id = 'mainTable',
            df = result$table,
            colDefsInput = columns
          ) 
        } else{
          shiny::showNotification('No results')
          resultTableServer(
            id = 'mainTable',
            df = data.frame(),
            colDefsInput = list(
              covariateName = reactable::colDef(
                header = withTooltip(
                  "Covariate Name",
                  "The name of the covariate"
                )
              ),
              covariateId = reactable::colDef(
                show = F,
                header = withTooltip("Covariate ID",
                                     "Unique identifier of the covariate")
              )
            )
          ) 
        }
        
        
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
    addSMD = F # unless two databases?
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
