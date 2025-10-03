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
    
    shinydashboard::box(
      width = "100%",
      title = 'Diagnostic results', 
      status = "info", 
      solidHeader = TRUE,
      
      resultTableViewer(ns('diagnosticSummaryTable'))
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
      
      diagnosticTable <- shiny::reactiveVal(NULL)
      colDef <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        eventExpr = performanceRowIds(), {
          
          if( max(performanceRowIds()) != 0){
            diagnosticTableTemp <- OhdsiReportGenerator::getPredictionDiagnostics(
              connectionHandler = connectionHandler, 
              schema = resultDatabaseSettings$schema, 
              plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
              databaseTable = resultDatabaseSettings$databaseTable, 
              modelDesignIds = unique(performances()$modelDesign[performanceRowIds()])
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
              
              
              modelTableOutputs <- resultTableServer(
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

