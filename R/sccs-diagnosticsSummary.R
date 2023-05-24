# @file sccs-diagnosticsSummary
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


sccsDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    inputSelectionViewer(ns("input-selection")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('diagnosticsTablePanel'),
        shiny::tabPanel(
          title = 'Summary',
          resultTableViewer(ns("diagnosticsSummaryTable"))
        ),
        shiny::tabPanel(
          title = 'Full',
          resultTableViewer(ns("diagnosticsTable"))
        )
      )
    )
  )
  
}

sccsDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      targetIds <- getSccsDiagTargets(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      outcomeIds <- getSccsDiagOutcomes(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      analysisIds <- getSccsDiagAnalyses(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = targetIds,
              selected = targetIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
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
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              selected = outcomeIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 12,
            varName = 'analysisIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Analysis: ',
              choices = analysisIds,
              selected = analysisIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
    
      data <- shiny::reactive({
        getSccsAllDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = inputSelected()$targetIds,
          outcomeIds = inputSelected()$outcomeIds,
          analysisIds = inputSelected()$analysisIds
        )
      })
      
      data2 <- shiny::reactive({ # use CM diag function
        diagnosticSummaryFormat(
          data = data,
          idCols = c('databaseName','exposure','covariateName'),
          namesFrom = c('outcome','analysis')
          )
      })
        
        customColDefs <- list(
          databaseName = reactable::colDef(
            header = withTooltip(
              "Database",
              "The database name"
            )
          ),
          exposure = reactable::colDef(
            header = withTooltip(
              "Exposure",
              "The exposure of interest "
            )
          ),
          outcome = reactable::colDef(
            header = withTooltip(
              "Outcome",
              "The outcome of interest "
            )
          ),
          analysis = reactable::colDef(
            header = withTooltip(
              "Analysis",
              "The analysis name "
            )
          ),
          covariateName = reactable::colDef(
            header = withTooltip(
              "Time Period",
              "The time period of interest"
            )
          ),
          mdrr = reactable::colDef(
            header = withTooltip(
              "mdrr",
              "The minimum detectible relative risk"
            )
          ),
          ease = reactable::colDef(
            header = withTooltip(
              "ease",
              "The ..."
            )
          ),
          timeTrendP = reactable::colDef(
            header = withTooltip(
              "timeTrendP",
              "The ..."
            )
          ),
          preExposureP = reactable::colDef(
            header = withTooltip(
              "preExposureP",
              "The ..."
            )
          ),
          mdrrDiagnostic = reactable::colDef(
            header = withTooltip(
              "mdrrDiagnostic",
              "The ..."
            )
          ),
          easeDiagnostic = reactable::colDef(
            header = withTooltip(
              "easeDiagnostic",
              "The ..."
            )
          ),
          timeTrendDiagnostic = reactable::colDef(
            header = withTooltip(
              "timeTrendDiagnostic",
              "The ..."
            )
          ),
          preExposureDiagnostic = reactable::colDef(
            header = withTooltip(
              "preExposureDiagnostic",
              "The ..."
            )
          ),
          
          unblind = reactable::colDef(
            header = withTooltip(
              "unblind",
              "If the value is 1 then the diagnostics passed and results can be unblinded"
            )
          )
          
        )
        
        resultTableServer(
          id = "diagnosticsTable",
          df = data,
          colDefsInput = customColDefs
        )
        
        
        # Summary table
        customColDefs2 <- list(
          databaseName = reactable::colDef(
            header = withTooltip(
              "Database",
              "The database name"
            )
          ),
          exposure = reactable::colDef(
            header = withTooltip(
              "exposure",
              "The exposure cohort of interest "
            )
          ),
          covariateName = reactable::colDef(
            header = withTooltip(
              "Time Period",
              "The time period of interest"
            )
          )
        )
        
        resultTableServer(
          id = "diagnosticsSummaryTable",
          df = data2,
          colDefsInput = styleColumns(customColDefs2, outcomeIds, analysisIds)
        )
      
    }
  )
}



getSccsDiagAnalyses <- function(
  connectionHandler,
  resultDatabaseSettings
){
  
  sql <- "
  SELECT distinct
  a.analysis_id,
  a.description as analysis
 
  FROM
  @database_schema.@table_prefixanalysis a
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  res <- result$analysisId
  names(res) <- result$analysis
    
  return(res)
}


getSccsDiagOutcomes <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  sql <- "
  SELECT distinct
  c.cohort_name as outcome, 
  c.cohort_definition_id
 
  FROM @database_schema.@table_prefixdiagnostics_summary ds
            inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @database_schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  res <- result$cohortDefinitionId
  names(res) <- result$outcome
  
  return(res)
}

getSccsDiagTargets <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  sql <- "
  SELECT distinct
  c2.cohort_name as target, 
  c2.cohort_definition_id
 
 FROM @database_schema.@table_prefixdiagnostics_summary ds

  INNER JOIN
  @database_schema.@table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @database_schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  res <- result$cohortDefinitionId
  names(res) <- result$target
  
  return(res)
}


getSccsAllDiagnosticsSummary <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    outcomeIds,
    analysisIds
) {
  sql <- "
  SELECT 
  d.cdm_source_abbreviation as database_name,
  c.cohort_name as outcome, 
  c2.cohort_name as exposure,
  a.description as analysis,
  cov.covariate_name,
  ds.*
  FROM @database_schema.@table_prefixdiagnostics_summary ds
            inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @database_schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   
   INNER JOIN
  @database_schema.@database_table_prefix@database_table d
  on d.database_id = ds.database_id
  
  INNER JOIN
  @database_schema.@table_prefixanalysis a
  on a.analysis_id = ds.analysis_id
  
  INNER JOIN
  @database_schema.@table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @database_schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
   
   
   where
   
   c2.cohort_definition_id in (@target_ids) and
   c.cohort_definition_id in (@outcome_ids) and
   a.analysis_id in (@analysis_ids) 
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
    table_prefix = resultDatabaseSettings$tablePrefix,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    
    target_ids = paste0(targetIds, collapse = ','),
    outcome_ids = paste0(outcomeIds, collapse = ','),
    analysis_ids = paste0(analysisIds, collapse = ','),
    
    snakeCaseToCamelCase = TRUE
  )
  
  result <- result %>% 
    dplyr::select(-c("analysisId","exposuresOutcomeSetId","databaseId","covariateId"))
  
  result$summaryValue <- apply(
    X = result[, grep('Diagnostic', colnames(result))], 
    MARGIN = 1, 
    FUN = function(x){
      
      if(sum(x %in% c('FAIL'))>0){
        return('Fail')
      } else if(sum(x %in% c('WARNING')) >0){
        return(sum(x %in% c('WARNING'), na.rm = T))
      } else{
        return('Pass')
      }
    }
  )
  return(result)
  
}
