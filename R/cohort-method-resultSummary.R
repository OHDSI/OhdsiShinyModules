# @file cohort-method-resultSummary
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


#' The module viewer for rendering the cohort method results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method diagnostics viewer
#' 
#' @export
cohortMethodResultSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    type = 'hidden',
    id = ns('resultPanel'),
    
    shiny::tabPanel(
      title = "Table",
      #shinydashboard::box(
      #  status = 'info', 
      #  width = '100%',
      #  title = shiny::span('Result Summary'),
     #   solidHeader = TRUE,
        resultTableViewer(ns("resultSummaryTable"))
     # )
    ),
    
    shiny::tabPanel(
      title = "Results",
      shiny::actionButton(
        inputId = ns('goBackCmResults'), 
        label = "Back To Result Summary",
        shiny::icon("arrow-left"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      cohortMethodFullResultViewer(ns("cmFullResults"))
    )
    
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
cohortMethodResultSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    inputSelected
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        eventExpr = input$goBackCmResults,
        {
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Table") 
        })
      
      data <- shiny::reactive({
        getCmResultData(
          connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          inputSelected = inputSelected
        )
      })
      
      resultTableOutputs <- resultTableServer(
        id = "resultSummaryTable",
        df = data,
        colDefsInput = getCmResultSummaryTableColDef(), 
        addActions = c('results')
      )
      
      selectedRow <- shiny::reactiveVal(value = NULL)
      shiny::observeEvent(resultTableOutputs$actionCount(), {
        if(resultTableOutputs$actionType() == 'results'){
          selectedRow(data()[resultTableOutputs$actionIndex()$index,])
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Results")
        }
      })
      
      cohortMethodFullResultServer(
        id = "cmFullResults", 
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        selectedRow = selectedRow,
        actionCount = resultTableOutputs$actionCount
      )

    }
  )
}


getCmResultSummaryTableColDef <- function(){
  result <- list(
    
    analysisId = reactable::colDef(show = F),
    description = reactable::colDef(
      header = withTooltip(
        "Analysis",
        "The analysis description"
      ), 
      minWidth = 140
    ),
    databaseId = reactable::colDef(show = F),
    
    cdmSourceAbbreviation = reactable::colDef(
      header = withTooltip(
        "Database",
        "The database name"
      )
    ),
    
    targetId = reactable::colDef(show = F),
    target = reactable::colDef(
      header = withTooltip(
        "Target",
        "The target cohort of interest"
      ),
      minWidth = 300
    ),
    
    comparatorId = reactable::colDef(show = F),
    comparator = reactable::colDef(
      header = withTooltip(
        "Comparator",
        "The comparator cohort of interest"
      ),
      minWidth = 300
    ),
    
    outcomeId = reactable::colDef(show = F),
    outcome = reactable::colDef(
      header = withTooltip(
        "Outcome",
        "The outcome of interest"
      ),
      minWidth = 300
    ),
    
    rr = reactable::colDef(
      header = withTooltip(
        "RR",
        "The estimated relative risk (e.g. the hazard ratio)"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    ci95Lb = reactable::colDef(
      header = withTooltip(
        "Lower 95% CI",
        "The lower bound of the 95% confidence internval of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    ci95Ub = reactable::colDef(
      header = withTooltip(
        "Upper 95% CI",
        "The upper bound of the 95% confidence internval of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    p = reactable::colDef(
      header = withTooltip(
        "p-val",
        "The two-sided p-value of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedRr = reactable::colDef(
      header = withTooltip(
        "Calibrated RR",
        "The calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedCi95Lb = reactable::colDef(
      header = withTooltip(
        "Calibrated Lower 95% CI",
        "The lower bound of the 95% confidence internval of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedCi95Ub = reactable::colDef(
      header = withTooltip(
        "Calibrated Upper 95% CI",
        "The upper bound of the 95% confidence internval of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedP = reactable::colDef(
      header = withTooltip(
        "Calibrated p-val",
        "The two-sided p-value of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    logRr = reactable::colDef(show = F),
    seLogRr = reactable::colDef(show = F),
    targetSubjects = reactable::colDef(show = F),
    comparatorSubjects  = reactable::colDef(show = F),
    targetDays = reactable::colDef(show = F),
    comparatorDays  = reactable::colDef(show = F),
    targetOutcomes = reactable::colDef(show = F),
    comparatorOutcomes  = reactable::colDef(show = F),
    calibratedLogRr = reactable::colDef(show = F),
    calibratedSeLogRr = reactable::colDef(show = F),
    calibratedSeLogRr = reactable::colDef(show = F),
    unblind = reactable::colDef(show = F)
  )
  
  return(result)
}

getCmResultData <- function(
    connectionHandler, 
    resultDatabaseSettings,
    inputSelected
) {
  
  targetIds = inputSelected()$targetIds
  outcomeIds = inputSelected()$outcomeIds
  comparatorIds = inputSelected()$comparatorIds
  analysisIds = inputSelected()$analysisIds
  
  if(is.null(comparatorIds) || is.null(targetIds) || is.null(outcomeIds) || is.null(analysisIds)){
    return(NULL)
  }
  
  sql <- "
  SELECT
  cma.analysis_id,
  cma.description description,
  dmd.database_id database_id,
  dmd.cdm_source_abbreviation cdm_source_abbreviation,
  cmr.target_id,
  cg1.cohort_name as target,
  cmr.outcome_id,
  cg2.cohort_name as outcome,
  cmr.comparator_id,
  cg3.cohort_name as comparator,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.rr end rr,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.ci_95_lb end ci_95_lb,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.ci_95_ub end ci_95_ub,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.p end p,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.log_rr end log_rr,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.se_log_rr end se_log_rr,
  cmr.target_subjects,
  cmr.comparator_subjects, 
  cmr.target_days,
  cmr.comparator_days,
  cmr.target_outcomes,
  cmr.comparator_outcomes,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_rr end calibrated_rr,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_ci_95_lb end calibrated_ci_95_lb,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_ci_95_ub end calibrated_ci_95_ub,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_p end calibrated_p,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_log_rr end calibrated_log_rr,
  case when COALESCE(cmds.unblind, 0) = 0 then NULL else cmr.calibrated_se_log_rr end calibrated_se_log_rr,
  COALESCE(cmds.unblind, 0) unblind 
FROM
  @schema.@cm_table_prefixanalysis cma
  JOIN @schema.@cm_table_prefixresult cmr 
  on cmr.analysis_id = cma.analysis_id
  
  JOIN @schema.@database_table dmd 
  on dmd.database_id = cmr.database_id
  
  LEFT JOIN @schema.@cm_table_prefixdiagnostics_summary cmds 
  on cmds.analysis_id = cmr.analysis_id
	AND cmds.target_id = cmr.target_id
	AND cmds.comparator_id = cmr.comparator_id
	AND cmds.outcome_id = cmr.outcome_id
	AND cmds.database_id = cmr.database_id
	
	inner join @schema.@cg_table_prefixcohort_definition cg1
	on cg1.cohort_definition_id = cmr.target_id
	
	inner join @schema.@cg_table_prefixcohort_definition cg2
	on cg2.cohort_definition_id = cmr.outcome_id
	
	inner join @schema.@cg_table_prefixcohort_definition cg3
	on cg3.cohort_definition_id = cmr.comparator_id
  
      where cmr.target_id in (@targets)
      {@use_comparators}?{and cmr.comparator_id in (@comparators)}
      and cmr.outcome_id in (@outcomes)
      {@use_analyses}?{and cmr.analysis_id in (@analyses)}
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
  
  return(
    result
  )
}
