context("patient-level-prediction-designSummary")

shiny::testServer(
  app = patientLevelPredictionDesignSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    
    expect_true(is.null(diagnosticId()))

    newTars <- getPlpCohortIds(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      type = 'target'
    )
    expect_true(length(newTars)>0)
    
    
    designSummary <- getPredictionDesignSummary(
      connectionHandler = connectionHandler, #plp?
      resultDatabaseSettings = resultDatabaseSettings,#plp?
      targetIds = targetIds[1],
      outcomeIds = outcomeIds[1]
    )
    
  })
