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
        getSccsDiagnostics(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema, 
          sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix, 
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
          databaseTable = resultDatabaseSettings$databaseTable,
          targetIds =  targetIds,
          outcomeIds = outcomeId
        )
      })
      
      resultTableServer(
        id = "sccsDiagnosticsTable",
        df = sccsDiagnostics,
        colDefsInput = estimationGetSccsDiagnosticColDefs(),
        selectedCols = c(
          'databaseName', 
          'description',
          'targetName',
          'indicationName',
          'summaryValue'
        ),
        elementId = session$ns('sccsDiagnosticsTable')
      )
      
      
    }
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
    databaseId = reactable::colDef(show = FALSE),
    targetName = reactable::colDef(
      name = "Target",
      header = withTooltip(
        "Target",
        "The target cohort of interest "
      )
    ),
    targetId = reactable::colDef(show = FALSE),
    outcomeName = reactable::colDef(
      name = "Outcome",
      header = withTooltip(
        "Outcome",
        "The outcome of interest "
      )
    ),
    outcomeId = reactable::colDef(show = FALSE),
    indicationName = reactable::colDef(
      name = "Indication",
      header = withTooltip(
        "Indication",
        "The indication of interest "
      )
    ),
    indicationId = reactable::colDef(show = FALSE),
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
        "The analysis name "
      )
    ),
    analysisId = reactable::colDef(show = FALSE),
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
    ),
    
    unblindForEvidenceSynthesis = reactable::colDef(
      name = "Unblind for evidence synthesis",
      header = withTooltip(
        "Unblind for evidence synthesis",
        "If the value is 1 then the diagnostics passed all diagnostics except power"
      )
    ),
    
    timeStabilityP = reactable::colDef(
      name = "timeStabilityP",
      header = withTooltip(
        "timeStabilityP",
        "The ..."
      )
    ),
    
    timeStabilityDiagnostic = reactable::colDef(
      name = "timeStabilityDiagnostic",
      header = withTooltip(
        "timeStabilityDiagnostic",
        "The ..."
      )
    ),
    
    eventExposureLb = reactable::colDef(
      name = "eventExposureLb",
      header = withTooltip(
        "eventExposureLb",
        "The ..."
      )
    ),
    
    eventExposureUb = reactable::colDef(
      name = "eventExposureUb",
      header = withTooltip(
        "eventExposureUb",
        "The ..."
      )
    ),
    
    eventExposureDiagnostic = reactable::colDef(
      name = "eventExposureDiagnostic",
      header = withTooltip(
        "eventExposureDiagnostic",
        "The ..."
      )
    ),
    
    eventObservationLb = reactable::colDef(
      name = "eventObservationLb",
      header = withTooltip(
        "eventObservationLb",
        "The ..."
      )
    ), 
    
    eventObservationUb = reactable::colDef(
      name = "eventObservationUb",
      header = withTooltip(
        "eventObservationUb",
        "The ..."
      )
    ), 
    
    eventObservationDiagnostic = reactable::colDef(
      name = "eventObservationDiagnostic",
      header = withTooltip(
        "eventObservationDiagnostic",
        "The ..."
      )
    ), 
    
    rareOutcomePrevalence = reactable::colDef(
      name = "rareOutcomePrevalence",
      header = withTooltip(
        "rareOutcomePrevalence",
        "The ..."
      )
    ), 
    
    rareOutcomeDiagnostic = reactable::colDef(
      name = "rareOutcomeDiagnostic",
      header = withTooltip(
        "rareOutcomeDiagnostic",
        "The ..."
      )
    )
    
  )
  
  return(result)
}


getSccsDiagnostics <- function(
  connectionHandler,
  schema, 
  sccsTablePrefix, 
  cgTablePrefix, 
  databaseTable,
  targetIds,
  outcomeIds
){
  
  targetIds <-  targetIds()
  outcomeIds <- outcomeIds()

  if(is.null(targetIds)){
    return(NULL)
  }
  
  if(is.null(outcomeIds)){
    return(NULL)
  }

result <- OhdsiReportGenerator::getSccsDiagnosticsData(
  connectionHandler = connectionHandler,
  schema = schema, 
  sccsTablePrefix = sccsTablePrefix, 
  cgTablePrefix = cgTablePrefix, 
  databaseTable = databaseTable,
  targetIds = targetIds,
  outcomeIds = outcomeIds
)
  
  return(result)
}
