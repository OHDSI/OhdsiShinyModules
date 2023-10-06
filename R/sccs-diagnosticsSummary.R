# @file sccs-diagnosticsSummary
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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
  
  #shinydashboard::box(
  #  status = 'info', 
  #  width = '100%',
  #  title = shiny::span('Diagnostic Results'),
  #  solidHeader = TRUE,
  shiny::div(  
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
  
}

sccsDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    inputSelected
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      data <- shiny::reactive({
        
        getSccsAllDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = inputSelected()$exposure,
          outcomeIds = inputSelected()$outcome,
          analysisIds = inputSelected()$analysis
        )
      })
      
      data2 <- shiny::reactive({ # use CM diag function
        diagnosticSummaryFormat(
          data = data,
          idCols = c('databaseName','target','covariateName'),
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
          target = reactable::colDef(
            header = withTooltip(
              "Target",
              "The target cohort of interest "
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
        
        
        resultTableServer(
          id = "diagnosticsSummaryTable",
          df = data2,
          colDefsInput = getColDefsSccsDiag(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          )
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
  @schema.@sccs_table_prefixanalysis a
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
 
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
            inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
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
 
 FROM @schema.@sccs_table_prefixdiagnostics_summary ds

  INNER JOIN
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
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
    analysisIds = NULL
) {
  
  if(is.null(targetIds)){
    return(NULL)
  }
  
  sql <- "
  SELECT 
  d.cdm_source_abbreviation as database_name,
  c.cohort_name as outcome, 
  c2.cohort_name as target,
  a.description as analysis,
  cov.covariate_name,
  ds.*
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
            inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   
   INNER JOIN
  @schema.@database_table_prefix@database_table d
  on d.database_id = ds.database_id
  
  INNER JOIN
  @schema.@sccs_table_prefixanalysis a
  on a.analysis_id = ds.analysis_id
  
  INNER JOIN
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
   
   
   where
   
   c2.cohort_definition_id in (@target_ids)
   and c.cohort_definition_id in (@outcome_ids)
   {@use_analysis}?{and a.analysis_id in (@analysis_ids)}
  ;
  "
  result <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    
    target_ids = paste0(targetIds, collapse = ','),
    outcome_ids = paste0(outcomeIds, collapse = ','),
    analysis_ids = paste0(analysisIds, collapse = ','),
    use_analysis = !is.null(analysisIds),
    
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


getColDefsSccsDiag <- function(
    connectionHandler,
    resultDatabaseSettings
){      
  
  fixedColumns =  list(
    databaseName = reactable::colDef(
      header = withTooltip(
        "Database",
        "The database name"
      ),
      sticky = "left"
    ),
    target = reactable::colDef(
      header = withTooltip(
        "Target",
        "The target cohort of interest "
      ),
      sticky = "left"
    ),
    covariateName = reactable::colDef(
      header = withTooltip(
        "Time Period",
        "The time period of interest"
      ),
      sticky = "left"
    )
  )
    
  outcomes <- getSccsDiagOutcomes(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
  )
  analyses <- getSccsDiagAnalyses(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
  )
  
  colnameFormat <- merge(unique(names(outcomes)), unique(names(analyses)))
  colnameFormat <- apply(colnameFormat, 1, function(x){paste(x, collapse = '_', sep = '_')})
  
  styleList <- lapply(
    colnameFormat, 
    FUN = function(x){
      reactable::colDef(
        header = withTooltip(
          substring(x,1,40),
          x
        ),
        style = function(value) {
          color <- 'orange'
          if(is.na(value)){
            color <- 'black'
          }else if(value == 'Pass'){
            color <- '#AFE1AF'
          }else if(value == 'Fail'){
            color <- '#E97451'
          }
          list(background = color)
        }
      )
    }
  )
  names(styleList) <- colnameFormat
  result <- append(fixedColumns, styleList)
  
  return(result)
}
