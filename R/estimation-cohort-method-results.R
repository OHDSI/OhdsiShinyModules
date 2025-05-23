# @file cohort-method-resultSummary
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


estimationCmResultsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    type = 'hidden',
    id = ns('resultPanel'),
    
    shiny::tabPanel(
      title = "Table",
      resultTableViewer(ns("resultSummaryTable"))
    ),
    
    shiny::tabPanel(
      title = "Results",
      shiny::actionButton(
        inputId = ns('goBackCmResults'), 
        label = "Back To Result Summary",
        shiny::icon("arrow-left"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      estimationCmFullResultViewer(ns("cmFullResults"))
    )
    
  )
  
 
}


estimationCmResultsServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    comparatorIds,
    outcomeId
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        eventExpr = input$goBackCmResults,
        {
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Table") 
        })
      
      # extract results from CM tables
      cmData <- shiny::reactive({
        estimationGetCmResultData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = targetIds,
          comparatorIds = comparatorIds,
          outcomeId = outcomeId
        )
      })
        
      # extract results from ES tables if tables exist
      esData <- shiny::reactive({
        tryCatch(
          {
            estimationGetCMMetaEstimation(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = targetIds,
              outcomeId = outcomeId
            )
          }, error = function(e){print('CM ES error');return(NULL)}
        )
      })
        
      data <- shiny::reactive({
        rbind(cmData(), esData())
      })
      
      resultTableOutputs <- resultTableServer(
        id = "resultSummaryTable",
        df = data,
        colDefsInput = estimationGetCmResultSummaryTableColDef(), 
        addActions = c('results') # TODO wont work for esData
      )
      
      selectedRow <- shiny::reactiveVal(value = NULL)
      shiny::observeEvent(resultTableOutputs$actionCount(), {
        if(resultTableOutputs$actionType() == 'results'){ # add an and here to only work for cmData
          selectedRow(data()[resultTableOutputs$actionIndex()$index,])
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Results")
        }
      })
      
      estimationCmFullResultServer(
        id = "cmFullResults", 
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        selectedRow = selectedRow,
        actionCount = resultTableOutputs$actionCount
      )
      
      
      return(data)

    }
  )
}


estimationGetCmResultSummaryTableColDef <- function(){
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
    
    targetId = reactable::colDef(header = withTooltip(
      "Target ID",
      "The ID of the target cohort of interest"
      )
    ),
    
    target = reactable::colDef(
      header = withTooltip(
        "Target",
        "The target cohort of interest"
      ),
      minWidth = 300
    ),
    
    comparatorId = reactable::colDef(header = withTooltip(
      "Comparator ID",
      "The ID of the comparator cohort of interest"
      )
    ),
    
    comparator = reactable::colDef(
      header = withTooltip(
        "Comparator",
        "The comparator cohort of interest"
      ),
      minWidth = 300
    ),
    
    outcomeId = reactable::colDef(header = withTooltip(
      "Outcome ID",
      "The ID of the outcome of interest"
      )
    ),
    
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

estimationGetCmResultData <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetIds,
    comparatorIds,
    outcomeId,
    runEvidenceSynthesis = F
) {
  
  targetIds = targetIds()
  comparatorIds = comparatorIds()
  outcomeId = outcomeId()
  
  if(is.null(comparatorIds) || is.null(targetIds) || is.null(outcomeId) ){
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
    outcomes = paste0(outcomeId, collapse = ','),
    use_comparators = !is.null(comparatorIds),
  )
  
  return(
    result
  )
}


estimationGetCMMetaEstimation <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    outcomeId
){
  targetIds <- targetIds()
  outcomeId <- outcomeId()
  
  sql <- "select 
  r.analysis_id, 
  a.description,
  0 as database_id,
  ev.evidence_synthesis_description as cdm_source_abbreviation,
  r.target_id,
  c1.cohort_name as target,
  r.outcome_id, 
  c3.cohort_name as outcome,
  r.comparator_id, 
  c2.cohort_name as comparator,
  NULL as rr, 
  NULL as ci_95_lb, 
  NULL as ci_95_ub, 
  NULL as p,
  NULL as log_rr, 
  NULL as se_log_rr,
  0 as target_subjects,
  0 as comparator_subjects, 
  0 as target_days,
  0 as comparator_days,
  0 as target_outcomes,
  0 as comparator_outcomes,
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, 
  r.calibrated_ci_95_ub, 
  r.calibrated_p,
  r.calibrated_log_rr, 
  r.calibrated_se_log_rr,
  1 unblind 
  
  from 
   @schema.@es_table_prefixcm_result as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id in (@target_ids) and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    es_table_prefix = resultDatabaseSettings$esTablePrefix,
    outcome_id = outcomeId,
    target_ids = paste0(targetIds, collapse = ',')
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )

  return(unique(result))
}


# Function to format results
# used by both cm and sccs
computeTraditionalP <- function(
    logRr, 
    seLogRr, 
    twoSided = TRUE, 
    upper = TRUE
) 
{
  z <- logRr/seLogRr
  
  pUpperBound <- 1 - stats::pnorm(z)
  pLowerBound <- stats::pnorm(z)
  
  if (twoSided) {
    return(2 * pmin(pUpperBound, pLowerBound))
  }
  else if (upper) {
    return(pUpperBound)
  }
  else {
    return(pLowerBound)
  }
}
