# @file cohort-method-diagnosticsSummary
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


#' The module viewer for rendering the PLE diagnostics results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method diagnostics viewer
#' 
#' @export
cohortMethodDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
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
    #)
  )
}


#' The module server for rendering the PLE diagnostics summary
#'
#' @param id the unique reference id for the module
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @param inputSelected  The target id, comparator id, outcome id and analysis id selected by the user
#'
#' @return
#' the PLE diagnostics summary results
#' 
#' @export
cohortMethodDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    inputSelected
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      data <- shiny::reactive({
        getCmDiagnosticsData(
          connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          inputSelected = inputSelected
        )
      })
      
      data2 <- shiny::reactive({
          diagnosticSummaryFormat(data)
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
            "The target cohort of interest"
          ),
          minWidth = 300
        ),
        comparator = reactable::colDef(
          header = withTooltip(
            "Comparator",
            "The comparator cohort of interest"
          ),
          minWidth = 300
        ),
        outcome = reactable::colDef(
          header = withTooltip(
            "Outcome",
            "The outcome of interest"
          )
        ),
        analysis = reactable::colDef(
          header = withTooltip(
            "Analysis",
            "The analysis name"
          )
        ),
        
        mdrr = reactable::colDef(
          header = withTooltip(
            "MDRR",
            "The minimum detectible relative risk"
          ),
          format = reactable::colFormat(digits = 4)
        ),
        ease = reactable::colDef(
          header = withTooltip(
            "EASE",
            "The expected absolute systematic error"
          ),
          format = reactable::colFormat(digits = 4)
        ),
        maxSdm = reactable::colDef(
          header = withTooltip(
            "Max SDM",
            "The maximum absolute standardized difference of mean"
          ),
          format = reactable::colFormat(digits = 4)
        ),
        sharedMaxSdm = reactable::colDef(
          header = withTooltip(
            "Shared Max SDM",
            "The maximum absolute standardized difference of mean of the shared balance (shared across outcomes)"
          ),
          format = reactable::colFormat(digits = 4)
        ),
        equipoise = reactable::colDef(
          header = withTooltip(
            "Equipoise",
            "The fraction of the study population with a preference score between 0.3 and 0.7"
          ),
          format = reactable::colFormat(digits = 4)
        ),
        balanceDiagnostic = reactable::colDef(
          header = withTooltip(
            "Balance Diagnostic",
            "Pass / warning / fail classification of the balance diagnostic (Max SDM)"
          )
        ),
        mdrrDiagnostic = reactable::colDef(
          header = withTooltip(
            "MDRR Diagnostic",
            "Pass / warning / fail classification of the MDRR diagnostic"
          )
        ),
        sharedBalanceDiagnostic = reactable::colDef(
          header = withTooltip(
            "Shared Balance Diagnostic",
            "Pass / warning / fail classification of the shared balance diagnostic (Shared Max SDM)"
          )
        ),
        easeDiagnostic = reactable::colDef(
          header = withTooltip(
            "Ease Diagnostic",
            "Pass / warning / fail classification of the EASE diagnostic"
          )
        ),
        equipoiseDiagnostic = reactable::colDef(
          header = withTooltip(
            "Equipoise Diagnostic",
            "Pass / warning / fail classification of the equipoise diagnostic"
          )
        ),
        
        unblind = reactable::colDef(
          header = withTooltip(
            "Unblind",
            "If the value is 1 then the diagnostics passed and results can be unblinded"
          )
        ),
        
        summaryValue = reactable::colDef(show = F)
        
      )
        
      resultTableServer(
        id = "diagnosticsTable",
        df = data,
        colDefsInput = customColDefs
      )
      
      resultTableServer(
        id = "diagnosticsSummaryTable",
        df = data2,
        colDefsInput = getColDefsCmDiag(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      )
      
    }
  )
}

getColDefsCmDiag <- function(
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
            "The target cohort of interest"
          ),
          sticky = "left"
        ),
        comparator = reactable::colDef(
          header = withTooltip(
            "Comparator",
            "The comparator cohort of interest"
          ),
          sticky = "left"
        )
      )
     
     outcomes <- getCmCohorts(
       connectionHandler = connectionHandler,
       resultDatabaseSettings = resultDatabaseSettings,
       type = 'outcome'
     )
     analyses <- getCmAnalyses(
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
          paste0(substring(x,1,35), "...", sep=""),
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

diagnosticSummaryFormat <- function(
    data, 
    idCols = c('databaseName','target', 'comparator'),
    namesFrom = c('outcome','analysis')
    ){
  
  if(is.null(data())){
    return(NULL)
  }
  
  data2 <- tidyr::pivot_wider(
    data = data(), 
    id_cols = idCols, 
    names_from = namesFrom, 
    values_from = c('summaryValue')
    )
  
  return(data2)
}



getCmDiagnosticsData <- function(
    connectionHandler, 
    resultDatabaseSettings,
    inputSelected
) {
  
  targetIds = inputSelected()$targetIds
  outcomeIds = inputSelected()$outcomeIds
  comparatorIds = inputSelected()$comparatorIds
  analysisIds = inputSelected()$analysisIds
  
  if(is.null(targetIds) || is.null(outcomeIds)){
    return(NULL)
  }
  
  sql <- "
    SELECT DISTINCT
      dmd.cdm_source_abbreviation database_name,
      cma.description analysis,
      cgcd1.cohort_name target,
      cgcd2.cohort_name comparator,
      cgcd3.cohort_name outcome,
      cmds.max_sdm,
      cmds.shared_max_sdm,
      cmds.equipoise,
      cmds.mdrr,
      cmds.ease,
      cmds.balance_diagnostic,
      cmds.shared_balance_diagnostic, -- added back
      cmds.equipoise_diagnostic,
      cmds.mdrr_diagnostic,
      cmds.ease_diagnostic,
      cmds.unblind
    FROM
      @schema.@cm_table_prefixdiagnostics_summary cmds
      INNER JOIN @schema.@cm_table_prefixanalysis cma ON cmds.analysis_id = cma.analysis_id
      INNER JOIN @schema.@database_table dmd ON dmd.database_id = cmds.database_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd1 ON cmds.target_id = cgcd1.cohort_definition_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd2 ON cmds.comparator_id = cgcd2.cohort_definition_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd3 ON cmds.outcome_id = cgcd3.cohort_definition_id
      
      where cgcd1.cohort_definition_id in (@targets)
      {@use_comparators}?{and cgcd2.cohort_definition_id in (@comparators)}
      and cgcd3.cohort_definition_id in (@outcomes)
      {@use_analyses}?{and cma.analysis_id in (@analyses)}
      ;
  "
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
  
    targets = paste0(targetIds, collapse = ','),
    comparators = paste0(comparatorIds, collapse = ','),
    outcomes = paste0(outcomeIds, collapse = ','),
    analyses = paste0(analysisIds, collapse = ','),
    
    use_comparators = !is.null(comparatorIds),
    use_analyses = !is.null(analysisIds)
  )
  
  # adding percent fail for summary
  result$summaryValue <- apply(
    X = result[, grep('Diagnostic', colnames(result))], 
    MARGIN = 1, 
    FUN = function(x){
      
      if(sum(x %in% c('FAIL'))>0){
        return('Fail')
      } else if(sum(x %in% c('WARNING')) >0){
        return(sum(x %in% c('WARNING')))
      } else{
        return('Pass')
      }
    }
  )
  
  return(
    result
  )
}
