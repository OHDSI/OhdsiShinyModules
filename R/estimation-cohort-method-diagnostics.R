estimationCmDiagnosticViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  resultTableViewer(ns("cmDiagnosticsTable"))
  
}


estimationCmDiagnosticServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    targetIds,
    comparatorIds,
    outcomeId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      
      cmDiagnostics <- shiny::reactive({
        estimationGetCmDiagnostics(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds =  targetIds,
          comparatorIds = comparatorIds,
          outcomeId = outcomeId
        )
      })
      
      resultTableServer(
        id = "cmDiagnosticsTable",
        df = cmDiagnostics,
        colDefsInput = estimationGetCmDiagnosticColDefs(),
        selectedCols = c(
          'databaseName', 
          'analysis',
          'target',
          'comparator',
          'summaryValue'
        ),
        elementId = session$ns('cmDiagnosticsTable')
      )
      
      
    }
  )
}


estimationGetCmDiagnostics <- function(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds =  targetIds,
    comparatorIds = comparatorIds,
    outcomeId = outcomeId
){
  targetIds <- targetIds()
  comparatorIds <- comparatorIds()
  outcomeId <- outcomeId()
  
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
      cmds.generalizability_max_sdm,
      cmds.ease,
      cmds.balance_diagnostic,
      cmds.shared_balance_diagnostic, -- added back
      cmds.equipoise_diagnostic,
      cmds.mdrr_diagnostic,
      cmds.generalizability_diagnostic,
      cmds.ease_diagnostic,
      cmds.unblind,
      cmds.unblind_for_evidence_synthesis
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
    outcomes = paste0(outcomeId, collapse = ','),
    
    use_comparators = ifelse(is.null(comparatorIds), F, T),
    use_analyses = F
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
  
  # add summaryValue after outcome
  result <- result %>% 
    dplyr::relocate("summaryValue", .after = "outcome")
  
  return(
    result
  )
  
}


estimationGetCmDiagnosticColDefs <- function(){
  result <- list(
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip(
        "Database",
        "The database name"
      ),
      sticky = "left"
    ),
    target = reactable::colDef(
      name = "Target",
      header = withTooltip(
        "Target",
        "The target cohort of interest"
      ),
      sticky = "left"
    ),
    comparator = reactable::colDef(
      name = "Comparator",
      header = withTooltip(
        "Comparator",
        "The comparator cohort of interest"
      ),
      sticky = "left"
    ),
    outcome = reactable::colDef(
      show = FALSE
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
        "The analysis name"
      )
    ),
    
    mdrr = reactable::colDef(
      name = "MDRR",
      header = withTooltip(
        "MDRR",
        "The minimum detectible relative risk"
      ),
      format = reactable::colFormat(digits = 4)
    ),
    ease = reactable::colDef(
      name = "EASE",
      header = withTooltip(
        "EASE",
        "The expected absolute systematic error"
      ),
      format = reactable::colFormat(digits = 4)
    ),
    maxSdm = reactable::colDef(
      name = "Max SDM",
      header = withTooltip(
        "Max SDM",
        "The maximum absolute standardized difference of mean"
      ),
      format = reactable::colFormat(digits = 4)
    ),
    sharedMaxSdm = reactable::colDef(
      name = "Shared Max SDM",
      header = withTooltip(
        "Shared Max SDM",
        "The maximum absolute standardized difference of mean of the shared balance (shared across outcomes)"
      ),
      format = reactable::colFormat(digits = 4)
    ),
    equipoise = reactable::colDef(
      name = "Equipoise",
      header = withTooltip(
        "Equipoise",
        "The fraction of the study population with a preference score between 0.3 and 0.7"
      ),
      format = reactable::colFormat(digits = 4)
    ),
    generalizabilityMaxSdm = reactable::colDef(
      name = "Generalizability Max SDM",
      header = withTooltip(
        "Generalizability Max SDM",
        "The maximum absolute standardized difference of mean comparing before to after adjustment."
      ),
      format = reactable::colFormat(digits = 4)
    ),
    balanceDiagnostic = reactable::colDef(
      name = "Balance Diagnostic",
      header = withTooltip(
        "Balance Diagnostic",
        "Pass / warning / fail classification of the balance diagnostic (Max SDM)"
      )
    ),
    mdrrDiagnostic = reactable::colDef(
      name = "MDRR Diagnostic",
      header = withTooltip(
        "MDRR Diagnostic",
        "Pass / warning / fail classification of the MDRR diagnostic"
      )
    ),
    sharedBalanceDiagnostic = reactable::colDef(
      name = "Shared Balance Diagnostic",
      header = withTooltip(
        "Shared Balance Diagnostic",
        "Pass / warning / fail classification of the shared balance diagnostic (Shared Max SDM)"
      )
    ),
    generalizabilityDiagnostic = reactable::colDef(
      name = "Generalizability Diagnostic",
      header = withTooltip(
        "Generalizability Diagnostic",
        "Pass / warning / fail classification of the generalizability diagnostic."
      )
    ),
    easeDiagnostic = reactable::colDef(
      name = "Ease Diagnostic",
      header = withTooltip(
        "Ease Diagnostic",
        "Pass / warning / fail classification of the EASE diagnostic"
      )
    ),
    equipoiseDiagnostic = reactable::colDef(
      name = "Equipoise Diagnostic",
      header = withTooltip(
        "Equipoise Diagnostic",
        "Pass / warning / fail classification of the equipoise diagnostic"
      )
    ),
    
    unblind = reactable::colDef(
      name = "Unblind",
      header = withTooltip(
        "Unblind",
        "If the value is 1 then the diagnostics passed and results can be unblinded"
      )
    ),
    
    unblindForEvidenceSynthesis = reactable::colDef(
      name = "Unblind for Evidence Synthesis",
      header = withTooltip(
        "Unblind for Evidence Synthesis",
        "Is unblinding the result for inclusion in evidence synthesis recommended? This ignores the MDRR diagnostic. (1 = yes, 0 = no)"
      )
    )
  )
  
  return(result)
}
