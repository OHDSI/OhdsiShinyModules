# @file data-diagnostic-summary.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring data-diagnostic summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the summary module
#'
#' @export
dataDiagnosticSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns('drugStudyFailSummaryTable'))
}

#' The module server for exploring prediction summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param mySchema the database schema for the model results
#' @param myTableAppend a string that appends the tables in the result schema
#' 
#' @return
#' The server to the summary module
#'
#' @export
dataDiagnosticSummaryServer <- function(
    id, 
    connectionHandler,
    mySchema,
    myTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      resultTable <- shiny::reactive(
        getDrugStudyFailSummary(
          connectionHandler = connectionHandler, 
          mySchema = mySchema, 
          myTableAppend = myTableAppend
        )
      )
      
      # create the colum formating
      columnFormat <- lapply(
        X = 1:ncol(resultTable()[,-1]), 
        
        FUN = function(x){
          return(
            reactable::colDef(
              style = function(value) {
                if (value > 0) {
                  color <- '#e00000'
                } else {
                  color <- "#008000"
                }
                list(color = color, fontWeight = "bold")
              }
            )
          )
        }
        
      )
      names(columnFormat) <- colnames(resultTable()[,-1])
      
      
      # format this to be color based on number
      output$drugStudyFailSummaryTable <- reactable::renderReactable({
        reactable::reactable(
          data = resultTable(),
          defaultPageSize = 20,
          searchable = TRUE,
          
          columns = columnFormat
        )
 
      })
      
    }
  )
}



getDrugStudyFailSummary <- function(
    connectionHandler, 
    mySchema, 
    myTableAppend = ''
){
  
  shiny::withProgress(message = 'Extracting data diagnostic summary', value = 0, {
    
    shiny::incProgress(1/3, detail = paste("Extracting data"))
    
    sql <- "SELECT distinct 
     sum.analysis_id, 
     sum.analysis_name, 
     sum.database_id,
     sum.total_fails
    
    FROM @my_schema.@my_table_appenddata_diagnostics_summary as sum;"
    
    summaryTable <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = mySchema,
      my_table_append = myTableAppend
    )
    
    shiny::incProgress(2/3, detail = paste("Data extracted"))
  
    
    summaryTable <- tidyr::pivot_wider(
      data = summaryTable, 
      id_cols = 'databaseId', 
      names_from = 'analysisName', 
      values_from = 'totalFails', 
      values_fill = -1
      )
    
    shiny::incProgress(3/3, detail = paste("Finished"))
    
    ParallelLogger::logInfo("Got database diagnostic summary")
    
  })
  
  return(summaryTable)  
}
