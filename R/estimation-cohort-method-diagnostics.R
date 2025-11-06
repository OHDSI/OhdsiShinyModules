estimationCmDiagnosticViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  resultTableViewer(ns("cmDiagnosticsTable"))
  
}


estimationCmDiagnosticServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    targetIds,
    outcomeId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      
      cmDiagnostics <- shiny::reactive({
        OhdsiReportGenerator::getCmDiagnosticsData(
          connectionHandler = connectionHandler, 
          schema = resultDatabaseSettings$schema, 
          cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
          databaseTable = resultDatabaseSettings$databaseTable, 
          targetIds = targetIds(), 
          outcomeIds = outcomeId()
        )
      })
      
      resultTableServer(
        id = "cmDiagnosticsTable",
        df = cmDiagnostics,
        colDefsInput = estimationGetCmDiagnosticColDefs(),
        selectedCols = c(
          'databaseName', 
          'description',
          'targetName',
          'comparatorName',
          'summaryValue'
        ),
        elementId = session$ns('cmDiagnosticsTable')
      )
      
      
    }
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
    targetName = reactable::colDef(
      name = "Target",
      header = withTooltip(
        "Target",
        "The target cohort of interest"
      ),
      sticky = "left"
    ),
    comparatorName = reactable::colDef(
      name = "Comparator",
      header = withTooltip(
        "Comparator",
        "The comparator cohort of interest"
      ),
      sticky = "left"
    ),
    outcomeName = reactable::colDef(
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
    description = reactable::colDef(
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
    #generalizabilityMaxSdm = reactable::colDef(
    #  name = "Generalizability Max SDM",
    #  header = withTooltip(
    #    "Generalizability Max SDM",
    #    "The maximum absolute standardized difference of mean comparing before to after adjustment."
    #  ),
    #  format = reactable::colFormat(digits = 4)
    #),
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
    #generalizabilityDiagnostic = reactable::colDef(
    #  name = "Generalizability Diagnostic",
    #  header = withTooltip(
    #    "Generalizability Diagnostic",
    #    "Pass / warning / fail classification of the generalizability diagnostic."
    #  )
    #),
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
    ),
    
    attritionDiagnostic = reactable::colDef(
      name = "Attrition Diagnostic",
      header = withTooltip(
        "Attrition Diagnostic",
        "(Depreciated) Pass / warning / fail classification of the Attrition diagnostic"
      )
    ),
    
    attritionFraction = reactable::colDef(
      name = "Attrition Fraction",
      header = withTooltip(
        "Attrition Fraction",
        "(Depreciated) The attrition fraction for the analysis"
      )
    ),
    
    outcomeId = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    comparatorId = reactable::colDef(show = FALSE),
    databaseId = reactable::colDef(show = FALSE),
    analysisId = reactable::colDef(show = FALSE)
  )
  
  return(result)
}
