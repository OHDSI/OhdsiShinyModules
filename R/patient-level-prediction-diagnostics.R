# @file patient-level-prediction-diagnostics.R
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


patientLevelPredictionDiagnosticsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::uiOutput(ns('diagOptions')),
    
    shiny::conditionalPanel(
      condition = "output.viewDiag == 1",
      ns = ns,
      
      shinydashboard::box(
        width = 12,
        title = 'Diagnostic results', 
        status = "info", 
        solidHeader = TRUE,
        
        resultTableViewer(ns('diagnosticSummaryTable'))
        
      )
      
    )
    
  )
  
  
}


patientLevelPredictionDiagnosticsServer <- function(
    id, 
    performances,
    performanceRowIds,
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$viewDiag <- shiny::reactive(0)
      shiny::outputOptions(output, "viewDiag", suspendWhenHidden = FALSE)
      modelOptions <- shiny::reactiveVal(NULL)
      
      diagnosticTable <- shiny::reactiveVal(NULL)
      colDef <- shiny::reactiveVal(NULL)
      
      # update the models that can be selected based on the choosen performances
      shiny::observeEvent(
        eventExpr = performanceRowIds(), {
          
          output$viewDiag <- shiny::reactive(0)
          
          if(length(performanceRowIds()) > 0 & max(performanceRowIds()) != 0){
            
            result <- performances()[performanceRowIds(),] %>%
              dplyr::select(
                "modelDesignId",
                "developmentTargetName", "developmentOutcomeName",
                "developmentTimeAtRisk", "developmentDatabase"
              ) %>%
              dplyr::mutate(
                name = paste0('Model design ',.data$modelDesignId,
                              ': developed in ', .data$developmentTargetName, ' to predict ',
                              .data$developmentOutcomeName, ' during ',
                              .data$developmentTimeAtRisk, ' for database ',
                              .data$developmentDatabase
                )
              ) %>%
              dplyr::distinct() %>%
              dplyr::arrange(.data$modelDesignId)
            
            option <- result$modelDesignId
            names(option) <- result$name
            modelOptions(option)
            
          } else{
            modelOptions(NULL)
          }
        }
      )
      
      # set the options to select the models of interest
      output$diagOptions <- shiny::renderUI(
        
        shinydashboard::box(
          title = 'Pick Models',
          width = 12, 
          collapsible = TRUE,
          
          shiny::div(
            shiny::helpText("Pick which models' diagnostics to view from those previously selected (default is all)"),
            
            shiny::fluidRow(
              style = "background-color: #DCDCDC; width: 98%; margin-left: 1%;margin-right: 1%;", # Apply style directly to fluidRow
              
              shiny::column(
                width = 9,
                
                shinyWidgets::pickerInput(
                  multiple = TRUE,
                  inputId = session$ns('selectedModels'),
                  label = 'Select', 
                  choices = modelOptions(), 
                  selected = modelOptions(), 
                  width = '100%', 
                  options = shinyWidgets::pickerOptions(
                    liveSearch = TRUE,
                    dropupAuto = FALSE
                  )
                )
                
              ),
              
              shiny::column(
                width = 3,
                shiny::div(
                  style = "padding-top: 25px; padding-left: 10px;",
                  shiny::actionButton(
                    inputId = session$ns('generate'), 
                    label = 'Generate' 
                  )
                )
              )
              
            ) # fluid row
          ) # div
        ) # box
        
      ) # renderUI
      
      
      shiny::observeEvent(
        eventExpr = input$selectedModels, {
          output$viewDiag <- shiny::reactive(0)
        })
      
      # when generate is pressed extract diagnostics and show them
      shiny::observeEvent(
        eventExpr = input$generate, {
          
          if( max(performanceRowIds()) != 0 & (length(input$selectedModels) > 0) ){
            
            output$viewDiag <- shiny::reactive(1)
            
            diagnosticTableTemp <- OhdsiReportGenerator::getPredictionDiagnostics(
              connectionHandler = connectionHandler, 
              schema = resultDatabaseSettings$schema, 
              plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
              databaseTable = resultDatabaseSettings$databaseTable, 
              modelDesignIds = as.double(input$selectedModels)
            )
            
            # format
            if(!is.null(diagnosticTableTemp)){
              
              # add developmentTimeAtRisk (could add other columns in future )
              diagnosticTableTemp <- merge(
                x = diagnosticTableTemp,
                y = unique(performances()[performanceRowIds(),c('modelDesignId','developmentTimeAtRisk')]),
                by = 'modelDesignId'
                )
              
              diagnosticTableTemp <- diagnosticTableTemp %>% 
                tidyr::pivot_wider(
                  id_cols = c('probastId','probastDescription'),
                  names_from = c('modelDesignId','developmentOutcomeName','developmentTargetName','developmentTimeAtRisk', 'developmentDatabaseName'), 
                  names_glue = "Model {modelDesignId}: Predicting {developmentOutcomeName} in {developmentTargetName} during {developmentTimeAtRisk} within {developmentDatabaseName}",
                  values_from = 'resultValue'
                )
              
              diagnosticTable(diagnosticTableTemp)
              
              colDefTemp <- list(
                probastId = reactable::colDef(
                  name = 'Probast ID'
                ),
                probastDescription = reactable::colDef(
                  name = 'Description'
                )
              )
              
              for(extraCol in colnames(diagnosticTableTemp)[-(1:2)]){
                colDefTemp[[length(colDefTemp) + 1]] <- reactable::colDef(
                  name = extraCol, 
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")
                    )
                names(colDefTemp)[length(colDefTemp)] <- extraCol
              }
              
              colDef(colDefTemp)
              
              
              resultTableServer(
                id = "diagnosticSummaryTable",
                df = diagnosticTable,
                colDefsInput = colDef(), #colDefsInput,
                elementId = session$ns('diagnosticSummaryTable')
              )
              
            }
          }
          
        })
      


          
    }
  ) # server
}

