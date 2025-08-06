estimationSccsDiagnosticViewer <- function(id=1) {
  ns <- shiny::NS(id)
  resultTableViewer(ns("sccsDiagnosticsTable"))
}


estimationSccsDiagnosticServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    targetIds,
    outcomeId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      
      sccsDiagnostics <- shiny::reactive({
        estimationGetSccsDiagnostics(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds =  targetIds,
          outcomeId = outcomeId
        )
      })
      
      resultTableServer(
        id = "sccsDiagnosticsTable",
        df = sccsDiagnostics,
        colDefsInput = estimationGetSccsDiagnosticColDefs(),
        selectedCols = c(
          'databaseName', 
          'analysis',
          'target',
          'indication',
          'summaryValue'
        ),
        elementId = session$ns('sccsDiagnosticsTable')
      )
      
      
    }
  )
}


estimationGetSccsDiagnostics <- function(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds =  targetIds,
    outcomeId = outcomeId
){
  targetIds <- targetIds()
  outcomeId <- outcomeId()
  
  sql <- "
  SELECT 
  d.cdm_source_abbreviation as database_name,
  a.description as analysis,
  c2.cohort_name as target,
  c3.cohort_name as indication,
  c.cohort_name as outcome, 
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
   
   left join
   @schema.@cg_table_prefixcohort_definition as c3
   on eos.nesting_cohort_id = c3.cohort_definition_id

   where
   
   cov.era_id in (@target_ids)
   and eos.outcome_id in (@outcome_ids)
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
    outcome_ids = paste0(outcomeId, collapse = ','),
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
  
  # add summaryValue after outcome
  result <- result %>% 
    dplyr::relocate("summaryValue", .after = "outcome")
  
  return(
    result
  )
  
}





estimationGetSccsDiagnosticColDefs <- function(){
  result <- list(
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip(
        "Database",
        "The database name"
      )
    ),
    target = reactable::colDef(
      name = "Target",
      header = withTooltip(
        "Target",
        "The target cohort of interest "
      )
    ),
    outcome = reactable::colDef(
      name = "Outcome",
      header = withTooltip(
        "Outcome",
        "The outcome of interest "
      )
    ),
    indication = reactable::colDef(
      name = "Indication",
      header = withTooltip(
        "Indication",
        "The indication of interest "
      )
    ),
    summaryValue =  reactable::colDef(
      name = "Diagnostic",
      header = withTooltip(
        "Diagnostic",
        "The overall result of the diagostics"
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
    ),
    analysis = reactable::colDef(
      name = "Analysis",
      header = withTooltip(
        "Analysis",
        "The analysis name "
      )
    ),
    covariateName = reactable::colDef(
      name = "Time Period",
      header = withTooltip(
        "Time Period",
        "The time period of interest"
      )
    ),
    mdrr = reactable::colDef(
      name = "mdrr",
      header = withTooltip(
        "mdrr",
        "The minimum detectible relative risk"
      )
    ),
    ease = reactable::colDef(
      name = "ease",
      header = withTooltip(
        "ease",
        "The ..."
      )
    ),
    timeTrendP = reactable::colDef(
      name = "timeTrendP",
      header = withTooltip(
        "timeTrendP",
        "The ..."
      )
    ),
    preExposureP = reactable::colDef(
      name = "preExposureP",
      header = withTooltip(
        "preExposureP",
        "The ..."
      )
    ),
    mdrrDiagnostic = reactable::colDef(
      name = "mdrrDiagnostic",
      header = withTooltip(
        "mdrrDiagnostic",
        "The ..."
      )
    ),
    easeDiagnostic = reactable::colDef(
      name = "easeDiagnostic",
      header = withTooltip(
        "easeDiagnostic",
        "The ..."
      )
    ),
    timeTrendDiagnostic = reactable::colDef(
      name = "timeTrendDiagnostic",
      header = withTooltip(
        "timeTrendDiagnostic",
        "The ..."
      )
    ),
    preExposureDiagnostic = reactable::colDef(
      name = "preExposureDiagnostic",
      header = withTooltip(
        "preExposureDiagnostic",
        "The ..."
      )
    ),
    
    unblind = reactable::colDef(
      name = "unblind",
      header = withTooltip(
        "unblind",
        "If the value is 1 then the diagnostics passed and results can be unblinded"
      )
    )
  )
  
  return(result)
}
