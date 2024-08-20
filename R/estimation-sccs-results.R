estimationSccsResultsViewer <- function(id = "sccs-results") {
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
        inputId = ns('goBackSccsResults'), 
        label = "Back To Result Summary",
        shiny::icon("arrow-left"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      estimationSccsFullResultViewer(ns("sccsFullResults"))
    )
    
  )
  
  
  
}
  

estimationSccsResultsServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    targetIds,
    outcomeId
) {
  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(
      eventExpr = input$goBackSccsResults,
      {
        shiny::updateTabsetPanel(session, "resultPanel", selected = "Table") 
      }
      )
    
    sccsData <- shiny::reactive({
      estimationGetSccsResults(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        exposureIds = targetIds,
        outcomeIds = outcomeId
      )
    })
    
    # add evidence synth if existsesData <- shiny::reactive({
    esData <- shiny::reactive({
      tryCatch(
        {
          estimationGetSccsEsResults(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposureIds = targetIds,
            outcomeIds = outcomeId
          )
        }, error = function(e){print('SCCS ES error');return(NULL)}
      )
    })
  
  data <- shiny::reactive({
    rbind(sccsData(), esData())
  })
    
    resultTableOutputs <- resultTableServer(
      id = "resultSummaryTable",
      df = data,
      colDefsInput = estimationGetSccsResultSummaryTableColDef(), 
      addActions = c('results')
    )
    
    selectedRow <- shiny::reactiveVal(value = NULL)
    shiny::observeEvent(resultTableOutputs$actionCount(), {
      if(resultTableOutputs$actionType() == 'results'){ # TODO only work if non meta
        selectedRow(data()[resultTableOutputs$actionIndex()$index,])
        shiny::updateTabsetPanel(session, "resultPanel", selected = "Results")
      }
    })
    
    estimationSccsFullResultServer(
      id = "sccsFullResults", 
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      selectedRow = selectedRow,
      actionCount = resultTableOutputs$actionCount
    )
    
    # return data for plot server
    return(data)
  }
  )
}


estimationGetSccsResultSummaryTableColDef <- function(){
  
  results <- list(
    
    databaseId = reactable::colDef(show = F),
    covariateId = reactable::colDef(show = F),
    eraId = reactable::colDef(show = F),
    covariateAnalysisId = reactable::colDef(show = F),
    analysisId = reactable::colDef(show = F),
    outcomeId = reactable::colDef(show = F),
    indicationId = reactable::colDef(show = F),
    outcomeSubjects = reactable::colDef(show = F),
    outcomeEvents = reactable::colDef(show = F),
    outcomeObservationPeriods = reactable::colDef(show = F),
    covariateSubjects = reactable::colDef(show = F),
    covariateDays = reactable::colDef(show = F),
    covariateEras = reactable::colDef(show = F),
    covariateOutcomes = reactable::colDef(show = F),
    observedDays = reactable::colDef(show = F),
    mdrr = reactable::colDef(show = F),
    unblind = reactable::colDef(show = F),
    exposuresOutcomeSetId = reactable::colDef(show = F),
    
    logRr = reactable::colDef(show = F),
    seLogRr = reactable::colDef(show = F),
    calibratedLogRr = reactable::colDef(show = F),
    calibratedSeLogRr = reactable::colDef(show = F),
    llr = reactable::colDef(show = F),

    description = reactable::colDef( 
      filterable = TRUE,
      header = withTooltip(
        "Analysis", 
        "Analysis"
      ),
      minWidth = 300
      ),
    databaseName = reactable::colDef( 
      filterable = TRUE,
      header = withTooltip(
        "Data source", 
        "Data source"
      )),
    target = reactable::colDef( 
      filterable = TRUE,
      header = withTooltip(
        "Target", 
        "Target Cohort"
      ),
      minWidth = 300
    ),
    indication = reactable::colDef( 
      filterable = TRUE,
      header = withTooltip(
        "Indication", 
        "Target cohort is nested in this indication"
      ),
      minWidth = 300
    ),
    outcome = reactable::colDef( 
      filterable = TRUE,
      header = withTooltip(
        "Outcome", 
        "Outcome of interest"
      ),
      minWidth = 300
    ),
    rr = reactable::colDef( 
      header = withTooltip(
        "IRR", 
        "Incidence rate ratio (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    ci95Lb = reactable::colDef( 
      header = withTooltip(
        "LB", 
        "Lower bound of the 95 percent confidence interval (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    ci95Ub = reactable::colDef( 
      header = withTooltip(
        "UB", 
        "Upper bound of the 95 percent confidence interval (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    p = reactable::colDef( 
      header = withTooltip(
        "P", 
        "Two-sided p-value (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedRr = reactable::colDef( 
      header = withTooltip(
        "Cal.IRR", 
        "Incidence rate ratio (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedCi95Lb = reactable::colDef( 
      header = withTooltip(
        "Cal.LB", 
        "Lower bound of the 95 percent confidence interval (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedCi95Ub = reactable::colDef( 
      header = withTooltip(
        "Cal.UB", 
        "Upper bound of the 95 percent confidence interval (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedP = reactable::colDef( 
      header = withTooltip(
        "Cal.P", 
        "Two-sided p-value (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      )
  )
  
  return(results)
}

estimationGetSccsResults <- function(connectionHandler,
                           resultDatabaseSettings,
                           exposureIds,
                           outcomeIds
                           ) {
  exposureIds <- exposureIds()
  outcomeIds <- outcomeIds()
  
  sql <- "
  SELECT
  
    ds.cdm_source_abbreviation as database_name,
    sr.database_id,
    sc.covariate_id,
    sc.covariate_name,
    sc.era_id,
    sc.covariate_analysis_id,
    sr.analysis_id,
    a.description,
    eos.outcome_id,
    cg1.cohort_name as outcome,
    cg2.cohort_name as target,
    cg3.cohort_name as indication,
    eos.nesting_cohort_id as indication_id,
    eos.exposures_outcome_set_id,
      
  sr.outcome_subjects,
  sr.outcome_events,
  sr.outcome_observation_periods,
  sr.covariate_subjects, 
  sr.covariate_days,
  sr.covariate_eras,
  sr.covariate_outcomes,
  sr.observed_days,

  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.rr end rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.ci_95_lb end ci_95_lb,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.ci_95_ub end ci_95_ub,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.p end p,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.log_rr end log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.se_log_rr end se_log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_rr end calibrated_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_ci_95_lb end calibrated_ci_95_lb,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_ci_95_ub end calibrated_ci_95_ub,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_p end calibrated_p,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_log_rr end calibrated_log_rr,
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.calibrated_se_log_rr end calibrated_se_log_rr,
  
  case when COALESCE(sds.unblind, 0) = 0 then NULL else sr.llr end llr,


    sds.mdrr,
  --sds.ease,
  --sds.time_trend_p,
  --sds.pre_exposure_p,
  --sds.mdrr_diagnostic,
  --sds.ease_diagnostic,
  --sds.time_trend_diagnostic,
  --sds.pre_exposure_diagnostic,
  sds.unblind
  
  FROM @schema.@sccs_table_prefixresult sr
  INNER JOIN 
  @schema.@database_table_prefix@database_table ds 
  ON sr.database_id = ds.database_id
  INNER JOIN 
  @schema.@sccs_table_prefixdiagnostics_summary sds ON (
    sds.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sds.database_id = sr.database_id AND
    sds.analysis_id = sr.analysis_id AND
    sds.covariate_id = sr.covariate_id
  )
  INNER JOIN 
  @schema.@sccs_table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )
  INNER JOIN @schema.@sccs_table_prefixexposures_outcome_set eos
  ON 
  eos.exposures_outcome_set_id = sr.exposures_outcome_set_id
  INNER JOIN
  @schema.@sccs_table_prefixanalysis a
  on a.analysis_id = sr.analysis_id
  
  inner join 
  @schema.@cg_table_prefixcohort_definition cg1
	on cg1.cohort_definition_id = eos.outcome_id
	
	inner join
  @schema.@cg_table_prefixcohort_definition as cg2
  on cg2.cohort_definition_id = sc.era_id
  
   left join
   @schema.@cg_table_prefixcohort_definition as cg3
   on eos.nesting_cohort_id = cg3.cohort_definition_id
  
  WHERE 
  eos.outcome_id IN (@outcome_ids)
  AND sc.era_id IN (@exposure_ids)
  ;
  "
  
  results <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_ids = paste(outcomeIds, collapse = ','),
    exposure_ids = paste(exposureIds, collapse = ','),
    snakeCaseToCamelCase = TRUE
  )
  
  return(results)
}

 
estimationGetSccsEsResults <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposureIds,
    outcomeIds
) {
  
  exposureIds <- exposureIds()
  outcomeIds <- outcomeIds()
  
sql <- "select distinct
  ev.evidence_synthesis_description as database_name,
  0 as database_id,
  cov.covariate_id, -- exists?
  cov.covariate_name,
  cov.era_id, 
  0 as covariate_analysis_id,
  esr.analysis_id, 
  a.description,
  eos.outcome_id, 
  c3.cohort_name as outcome,
  c1.cohort_name as target,
  c4.cohort_name as indication,
  eos.nesting_cohort_id as indication_id,
  eos.exposures_outcome_set_id,
  esr.outcome_subjects,
  esr.outcome_events,
  esr.outcome_observation_periods,
  esr.covariate_subjects, 
  esr.covariate_days,
  esr.covariate_eras,
  esr.covariate_outcomes,
  esr.observed_days,
  esr.rr,
  esr.ci_95_lb,
  esr.ci_95_ub,
  esr.p,
  esr.log_rr,
  esr.se_log_rr,
  esr.calibrated_rr, 
  esr.calibrated_ci_95_lb, 
  esr.calibrated_ci_95_ub,  
  esr.calibrated_p,
  esr.calibrated_log_rr, 
  esr.calibrated_se_log_rr,
  NULL as llr,
  esd.mdrr,
  esd.unblind as unblind
  
  from 
   @schema.@es_table_prefixsccs_result as esr
   inner join 
   @schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   esr.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixcovariate as cov
   on 
   esr.covariate_id = cov.covariate_id and
   esr.analysis_id = cov.analysis_id and
   esr.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @schema.@es_table_prefixsccs_diagnostics_summary as esd
   on
   esr.analysis_id = esd.analysis_id and 
   esr.exposures_outcome_set_id = esd.exposures_outcome_set_id and 
   esr.covariate_id = esd.covariate_id and
   esr.evidence_synthesis_analysis_id = esd.evidence_synthesis_analysis_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @schema.@sccs_table_prefixanalysis as a
   on a.analysis_id = esr.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = esr.evidence_synthesis_analysis_id
   
  left join
   @schema.@cg_table_prefixcohort_definition as c4
   on eos.nesting_cohort_id = c4.cohort_definition_id
   
   where 
   esr.calibrated_rr != 0 and
   esd.unblind = 1 and
   cov.era_id in (@target_ids) and
   eos.outcome_id in (@outcome_id)
  ;"

result <- connectionHandler$queryDb(
  sql = sql,
  schema = resultDatabaseSettings$schema,
  es_table_prefix = resultDatabaseSettings$esTablePrefix,
  sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
  cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
  outcome_id = paste0(outcomeIds, collapse = ','),
  target_ids = paste0(exposureIds, collapse = ',')
)

return(result)
}
