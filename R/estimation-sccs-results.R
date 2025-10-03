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
        }, error = function(e){print('SCCS ES error');print(e);return(NULL)}
      )
    })
  
  data <- shiny::reactive({
    dplyr::bind_rows(sccsData(), esData())
  })
    
    resultTableOutputs <- resultTableServer(
      id = "resultSummaryTable",
      df = data,
      colDefsInput = estimationGetSccsResultSummaryTableColDef(), 
      addActions = c('results'),
      elementId = session$ns('resultSummaryTable')
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
    covariateName = reactable::colDef(show = FALSE),
    databaseId = reactable::colDef(show = FALSE),
    covariateId = reactable::colDef(show = FALSE),
    eraId = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    covariateAnalysisId = reactable::colDef(show = FALSE),
    analysisId = reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(show = FALSE),
    indicationId = reactable::colDef(show = FALSE),
    outcomeSubjects = reactable::colDef(show = FALSE),
    outcomeEvents = reactable::colDef(show = FALSE),
    outcomeObservationPeriods = reactable::colDef(show = FALSE),
    covariateSubjects = reactable::colDef(
      name = 'Persons exposed', 
      show = TRUE
      ), # target
    covariateDays = reactable::colDef(show = FALSE),
    covariateEras = reactable::colDef(show = FALSE),
    covariateOutcomes = reactable::colDef(
      name = 'Outcomes while exposed', 
      show = TRUE
      ), # outcome
    observedDays = reactable::colDef(show = FALSE),
    mdrr = reactable::colDef(show = FALSE),
    unblind = reactable::colDef(show = FALSE),
    exposuresOutcomeSetId = reactable::colDef(show = FALSE),
    
    logRr = reactable::colDef(show = FALSE),
    seLogRr = reactable::colDef(show = FALSE),
    calibratedLogRr = reactable::colDef(show = FALSE),
    calibratedSeLogRr = reactable::colDef(show = FALSE),
    llr = reactable::colDef(show = FALSE),

    description = reactable::colDef(
      name = "Analysis",
      filterable = TRUE,
      header = withTooltip(
        "Analysis", 
        "Analysis"
      ),
      minWidth = 300
      ),
    databaseName = reactable::colDef( 
      name = "Data source",
      filterable = TRUE,
      header = withTooltip(
        "Data source", 
        "Data source"
      )),
    targetName = reactable::colDef( 
      name = "Target", 
      filterable = TRUE,
      header = withTooltip(
        "Target", 
        "Target Cohort"
      ),
      minWidth = 300
    ),
    indicationName = reactable::colDef( 
      name = "Indication", 
      filterable = TRUE,
      header = withTooltip(
        "Indication", 
        "Target cohort is nested in this indication"
      ),
      minWidth = 300
    ),
    outcomeName = reactable::colDef( 
      name = "Outcome",
      filterable = TRUE,
      header = withTooltip(
        "Outcome", 
        "Outcome of interest"
      ),
      minWidth = 300
    ),
    rr = reactable::colDef( 
      name = "IRR", 
      header = withTooltip(
        "IRR", 
        "Incidence rate ratio (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    ci95Lb = reactable::colDef( 
      name = "LB", 
      header = withTooltip(
        "LB", 
        "Lower bound of the 95 percent confidence interval (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    ci95Ub = reactable::colDef( 
      name = "UB",
      header = withTooltip(
        "UB", 
        "Upper bound of the 95 percent confidence interval (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    p = reactable::colDef( 
      name = "P",
      header = withTooltip(
        "P", 
        "Two-sided p-value (uncalibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedRr = reactable::colDef( 
      name = "Cal.IRR", 
      header = withTooltip(
        "Cal.IRR", 
        "Incidence rate ratio (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedCi95Lb = reactable::colDef( 
      name = "Cal.LB", 
      header = withTooltip(
        "Cal.LB", 
        "Lower bound of the 95 percent confidence interval (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedCi95Ub = reactable::colDef( 
      name = "Cal.UB", 
      header = withTooltip(
        "Cal.UB", 
        "Upper bound of the 95 percent confidence interval (calibrated)"
      ),
      format = reactable::colFormat(digits = 4),
      na = "-"
      ),
    calibratedP = reactable::colDef( 
      name = "Cal.P",
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
  
  results <- OhdsiReportGenerator::getSccsEstimation(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    databaseTable = resultDatabaseSettings$databaseTable,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    targetIds = exposureIds, 
    outcomeIds = outcomeIds
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
  
  result <- OhdsiReportGenerator::getSccsMetaEstimation(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    esTablePrefix = resultDatabaseSettings$esTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    targetIds = exposureIds, 
    outcomeIds = outcomeIds
  )

return(result)
}
