# @file prediction-designSummary.R
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


#' The module viewer for exploring prediction designs that have been run
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the prediction design module
#'
#' @export
predictionDesignSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns('designSummaryTable'))
}

#' The module server for exploring prediction designs in the results
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
#' The server to the prediction design module
#'
#' @export
predictionDesignSummaryServer <- function(
  id, 
  connectionHandler,
  mySchema,
  myTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      designSummaryTable <- getDesignSummary(
        connectionHandler = connectionHandler, 
        mySchema = mySchema, 
        myTableAppend = myTableAppend 
      )
      
      # check if this makes drpdwn filter
      designSummaryTable$target <- as.factor(designSummaryTable$target)
      designSummaryTable$outcome <- as.factor(designSummaryTable$outcome)
      
      output$designSummaryTable <- reactable::renderReactable({
        reactable::reactable(
          data = cbind(
            designSummaryTable,
            diagnostic = rep("",nrow(designSummaryTable)),
            details = rep("",nrow(designSummaryTable)),
            report = rep("",nrow(designSummaryTable))
          ), 
          columns = list(
            # Render a "show details" button in the last column of the table.
            # This button won't do anything by itself, but will trigger the custom
            # click action on the column.
            modelDesignId = reactable::colDef( 
              header = withTooltip(
                "Design ID", 
                "A unique identifier for the model design"
              )),
            modelType = reactable::colDef( 
              header = withTooltip(
                "Model Type", 
                "The classifier/survivial model"
              )),
            target = reactable::colDef( 
              header = withTooltip(
                "Target Pop", 
                "The patients who the risk model is applied to"
              )),
            outcome = reactable::colDef( 
              header = withTooltip(
                "Outcome", 
                "The outcome being predicted"
              )),
            TAR = reactable::colDef( 
              header = withTooltip(
                "TAR", 
                "The time-at-risk when the outcome is being predicted relative to the target pop index"
              ),
              sortable = TRUE
              ),
            
            diagDatabases = reactable::colDef(
              header = withTooltip(
                "Num. Diagnostic Dbs", 
                "The number of databases with the model design diagnostics evaluated"
              ),
              filterable = FALSE,
              sortable = TRUE
            ),
            devDatabases = reactable::colDef(
              header = withTooltip(
                "Num. Development Dbs", 
                "The number of databases where a model was developed using the design"
              ),
              filterable = FALSE,
              sortable = TRUE
            ),
            minAuroc = reactable::colDef(
              header = withTooltip(
                "min AUROC", 
                "The minimum AUROC across internal and external validations for this model design"
              ),
              sortable = TRUE,
              filterable = FALSE,
              format = reactable::colFormat(digits = 3)
              ),
            meanAuroc = reactable::colDef(
              header = withTooltip(
                "mean AUROC", 
                "The mean AUROC across internal and external validations for this model design"
              ),
              sortable = TRUE,
              filterable = FALSE,
              format = reactable::colFormat(digits = 3)
            ),
            maxAuroc = reactable::colDef(
              header = withTooltip(
                "max AUROC", 
                "The max AUROC across internal and external validations for this model design"
              ),
              filterable = FALSE,
              sortable = TRUE,
              format = reactable::colFormat(digits = 3)
            ),
            valDatabases = reactable::colDef(
              header = withTooltip(
                "Num. Validation Dbs", 
                "The number of databases where a model with the design was validated"
              ),
              sortable = TRUE,
              filterable = FALSE
            ),
            diagnostic = reactable::colDef(
              name = "",
              sortable = FALSE,
              filterable = FALSE,
              cell = function() htmltools::tags$button("View Diagnostics")
            ),
            details = reactable::colDef(
              name = "",
              sortable = FALSE,
              filterable = FALSE,
              cell = function() htmltools::tags$button("View Results")
            ),
            report = reactable::colDef(
              name = "",
              sortable = FALSE,
              filterable = FALSE,
              cell = function() htmltools::tags$button("View Report")
            )
          ),
          onClick = reactable::JS(paste0("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id !== 'details' & column.id !== 'report' & column.id !== 'diagnostic') {
      return
    }

    // Display an alert dialog with details for the row
    //window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2))

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if(column.id == 'details'){
      Shiny.setInputValue('",session$ns('show_details'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    if(column.id == 'report'){
      Shiny.setInputValue('",session$ns('show_report'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    if(column.id == 'diagnostic'){
      Shiny.setInputValue('",session$ns('show_diagnostic'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
  }")
          ),
          #groupBy = c("outcome","TAR", "target"), feedback was this wasnt nice
          filterable = TRUE
        )
      })
      
      # reactive of modelDesignId for exporing results
      modelDesignId <- shiny::reactiveVal(value = NULL)
      
      shiny::observeEvent(input$show_details, {
        #print(designSummaryTable$modelDesignId[input$show_details$index])
        if(designSummaryTable$devDatabases[input$show_details$index] > 0){
          modelDesignId(NULL)
          modelDesignId(designSummaryTable$modelDesignId[input$show_details$index])
        } else{
          shiny::showNotification("No models available for this model design.")
        }
      })
      
      reportId <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$show_report, {
        reportId(NULL)
        idForReport <- designSummaryTable$modelDesignId[input$show_report$index]
        reportId(idForReport)
        #writeLines('Testing123', file.path(tempdir(), 'report.html'))
        #createProtocol(connection = connection, modelDesignId = idForReport, outputLocation = file.path(tempdir(), 'report.html'))
      })
      
      diagnosticId <- shiny::reactiveVal(value = NULL)
      shiny::observeEvent(input$show_diagnostic, {
        
        if(designSummaryTable$diagDatabases[input$show_diagnostic$index] > 0){
          diagnosticId(NULL)
          diagnosticId(designSummaryTable$modelDesignId[input$show_diagnostic$index])
        } else{
          shiny::showNotification("No diagnostic results available for this model design.")
        }
      })
      
      return(
        list(
          designSummaryTable = designSummaryTable,
          modelDesignId = modelDesignId, # a reactive 
          diagnosticId = diagnosticId, # a reactive 
          reportId = reportId # a reactive
        )
      )
      
    }
  )
}



getDesignSummary <- function(
    connectionHandler, 
    mySchema, 
    myTableAppend = '' 
    ){

  shiny::withProgress(message = 'Generating model design summary', value = 0, {
    
  sql <- "SELECT 
          model_designs.model_design_id, 
          model_settings.model_type AS model_type, 
          targets.cohort_name AS target, 
          outcomes.cohort_name AS outcome,
          tars.tar_start_day, 
          tars.tar_start_anchor, 
          tars.tar_end_day, 
          tars.tar_end_anchor,
          COUNT(distinct diag.database_id) as diag_databases,
          COUNT(distinct d.database_id) dev_databases,
          MIN(p.value) min_AUROC,
          AVG(p.value) mean_AUROC,
          MAX(p.value) max_AUROC,
          COUNT(distinct v.database_id) val_databases

       FROM 
          @my_schema.@my_table_appendmodel_designs as model_designs 
          inner join
          @my_schema.@my_table_appendmodel_settings as model_settings
          on model_designs.model_setting_id = model_settings.model_setting_id
         
          LEFT JOIN
          @my_schema.@my_table_appendperformances AS results
            
           on model_designs.model_design_id = results.model_design_id
           
        LEFT JOIN (select * from @my_schema.@my_table_appendEVALUATION_STATISTICS where EVALUATION = 'Test' and METRIC = 'AUROC') p
           on p.performance_id = results.performance_id
             
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS targets ON model_designs.target_id = targets.cohort_id
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS outcomes ON model_designs.outcome_id = outcomes.cohort_id
        LEFT JOIN @my_schema.@my_table_appendtars AS tars ON model_designs.tar_id = tars.tar_id
         
        LEFT JOIN @my_schema.@my_table_appenddatabase_details AS d ON results.development_database_id = d.database_id 
        LEFT JOIN @my_schema.@my_table_appenddatabase_details AS v ON results.validation_database_id = v.database_id 
        
        LEFT JOIN @my_schema.@my_table_appenddiagnostics AS diag ON results.development_database_id = diag.database_id 
        
        GROUP BY model_designs.model_design_id, targets.cohort_name, 
          outcomes.cohort_name, tars.tar_start_day, tars.tar_start_anchor, 
          tars.tar_end_day, tars.tar_end_anchor;"
  
  
  shiny::incProgress(1/3, detail = paste("Extracting data"))
  
  summaryTable <- connectionHandler$queryDb(
    sql = sql, 
    my_schema = mySchema,
    my_table_append = myTableAppend
  )
  
  shiny::incProgress(2/3, detail = paste("Extracted data"))
  
  summaryTable <- editTar(summaryTable) %>%
    dplyr::relocate("TAR", .after = "outcome")  %>% 
    dplyr::relocate("devDatabases", .before = "valDatabases") %>%
    dplyr::relocate("diagDatabases", .before = "devDatabases")
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  
  return(summaryTable)
  
}

editTar <- function(summaryTable){
  
  summaryTable <- summaryTable %>% 
    dplyr::mutate(TAR = paste0('(',trimws(.data$tarStartAnchor),' + ',.data$tarStartDay, ') - (',trimws(.data$tarEndAnchor),' + ',.data$tarEndDay, ')' )) %>%
    dplyr::select(-c("tarStartAnchor", "tarStartDay", "tarEndAnchor", "tarEndDay"))
  
  return(summaryTable)
}
