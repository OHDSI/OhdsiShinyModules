context("patient-level-prediction-designSummary")

shiny::testServer(
  app = patientLevelPredictionDesignSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    #session$setInputs(show_details = list(index = 1))
    #expect_true(!is.null(modelDesignId()))
    
    expect_true(is.null(reportId()))
    #session$setInputs(show_report = list(index = 1))
    #expect_true(!is.null(reportId()))
    
    expect_true(is.null(diagnosticId()))
    #session$setInputs(show_diagnostic = list(index = 1))
    #expect_true(!is.null(diagnosticId()))
    
    designSummary <- getPredictionDesignSummary(
      connectionHandler = connectionHandler, #plp?
      resultDatabaseSettings = resultDatabaseSettings,#plp?
      targetIds = targetIds[1],
      outcomeIds = outcomeIds[1]
    )
    
  })
