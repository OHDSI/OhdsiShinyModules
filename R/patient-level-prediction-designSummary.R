# @file patient-level-prediction-designSummary.R
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


#' The module viewer for exploring prediction designs that have been run
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the prediction design module
#'
#' @export
patientLevelPredictionDesignSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    inputSelectionViewer(ns("input-selection")),
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      resultTableViewer(ns('designSummaryTable'))
    )
  )
}

#' The module server for exploring prediction designs in the results
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family PatientLevelPrediction
#' @return
#' The server to the prediction design module
#'
#' @export
patientLevelPredictionDesignSummaryServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      targetIds <- getPlpCohortIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        type = 'target'
      )
      outcomeIds <- getPlpCohortIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        type = 'outcome'
      )
      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = targetIds,
              selected = targetIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                dropupAuto = F,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              selected = outcomeIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10, 
                dropupAuto = F,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      designSummaryTable <- shiny::reactive({
        getPredictionDesignSummary(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = inputSelected()$targetIds,
          outcomeIds = inputSelected()$outcomeIds
        )
      })
      
      colDefsInput = list(
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
              ), 
              minWidth = 300
              ),
            outcome = reactable::colDef( 
              header = withTooltip(
                "Outcome", 
                "The outcome being predicted"
              ), 
              minWidth = 300
              ),
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
            )
          )
      
      tableOutputs <- resultTableServer(
        id = "designSummaryTable", # how is this working without session$ns
        df = designSummaryTable,
        colDefsInput = colDefsInput,
        addActions = c('models', 'diagnostics', 'report')
      )
          
      # reactive of modelDesignId for exporing results
      modelDesignId <- shiny::reactiveVal(value = NULL)
      reportId <- shiny::reactiveVal(NULL)
      diagnosticId <- shiny::reactiveVal(value = NULL)
      
      shiny::observeEvent(tableOutputs$actionCount(), {
        if(!is.null(tableOutputs$actionType())){
          if(tableOutputs$actionType() == 'diagnostics'){
            diagnosticId(NULL)
            diagnosticId(designSummaryTable()$modelDesignId[tableOutputs$actionIndex()$index])
          }
          if(tableOutputs$actionType() == 'report'){
            reportId(NULL)
            reportId(designSummaryTable()$modelDesignId[tableOutputs$actionIndex()$index])
          }
          if(tableOutputs$actionType() == 'models'){
            modelDesignId(NULL)
            modelDesignId(designSummaryTable()$modelDesignId[tableOutputs$actionIndex()$index])
          }
        }
      })

      return(
        list(
          modelDesignId = modelDesignId, # a reactive 
          diagnosticId = diagnosticId, # a reactive 
          reportId = reportId # a reactive
        )
      )
      
    }
  )
}


getPlpCohortIds <- function(
  connectionHandler,
  resultDatabaseSettings,
  type = 'target'
){
  
  sql <- "SELECT distinct cohorts.cohort_id, cohorts.cohort_name
          
       FROM 
          @schema.@plp_table_prefixmodel_designs as model_designs 
          inner join
        (SELECT c.cohort_id, cd.cohort_name FROM @schema.@plp_table_prefixcohorts c
        inner join @schema.@cg_table_prefixcohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS cohorts
        ON model_designs.@type_id = cohorts.cohort_id
        
        order by cohorts.cohort_name asc
        ;"
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    type = type
  )
  
  res <- result$cohortId
  names(res) <- result$cohortName
  
  return(res)
}

getPredictionDesignSummary <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetIds,
    outcomeIds
    ){

  if(length(targetIds) == 0 | length(outcomeIds) == 0){
    return(data.frame())
  }

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
          @schema.@plp_table_prefixmodel_designs as model_designs 
          inner join
          @schema.@plp_table_prefixmodel_settings as model_settings
          on model_designs.model_setting_id = model_settings.model_setting_id
         
          LEFT JOIN
          @schema.@plp_table_prefixperformances AS results
            
           on model_designs.model_design_id = results.model_design_id
           
        LEFT JOIN (select * from @schema.@plp_table_prefixEVALUATION_STATISTICS where EVALUATION = 'Test' and METRIC = 'AUROC') p
           on p.performance_id = results.performance_id
             
        LEFT JOIN 
        (SELECT c.cohort_id, cd.cohort_name FROM @schema.@plp_table_prefixcohorts c
        inner join @schema.@cg_table_prefixcohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS targets 
        ON model_designs.target_id = targets.cohort_id
        LEFT JOIN 
        (SELECT c.cohort_id, cd.cohort_name FROM @schema.@plp_table_prefixcohorts c
        inner join @schema.@cg_table_prefixcohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS outcomes 
        ON model_designs.outcome_id = outcomes.cohort_id
        LEFT JOIN @schema.@plp_table_prefixtars AS tars 
        ON model_designs.tar_id = tars.tar_id
         
        LEFT JOIN @schema.@plp_table_prefixdatabase_details AS d 
        ON results.development_database_id = d.database_id 
        LEFT JOIN @schema.@plp_table_prefixdatabase_details AS v 
        ON results.validation_database_id = v.database_id 
        
        LEFT JOIN @schema.@plp_table_prefixdiagnostics AS diag 
        ON results.development_database_id = diag.database_id 
        
        WHERE targets.cohort_id in (@target_ids)
        AND outcomes.cohort_id in (@outcome_ids)
        
        GROUP BY model_designs.model_design_id, model_settings.model_type, targets.cohort_name, 
          outcomes.cohort_name, tars.tar_start_day, tars.tar_start_anchor, 
          tars.tar_end_day, tars.tar_end_anchor;"
  
  
  shiny::incProgress(1/3, detail = paste("Extracting data"))
  
  summaryTable <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    target_ids = paste(targetIds, collapse = ','),
    outcome_ids = paste(outcomeIds, collapse = ',')
  )
  
  shiny::incProgress(2/3, detail = paste("Extracted data"))
  
  summaryTable <- editTar(summaryTable) %>%
    dplyr::relocate("TAR", .after = "outcome")  %>% 
    dplyr::relocate("devDatabases", .before = "valDatabases") %>%
    dplyr::relocate("diagDatabases", .before = "devDatabases")
  
  ##summaryTable <- cbind(
  ##  actions = rep("",nrow(summaryTable)),
  ##  summaryTable
  ##)
  
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
