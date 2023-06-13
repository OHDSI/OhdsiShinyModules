context("prediction-designSummary")

shiny::testServer(
  app = predictionDesignSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    schema = resultDatabaseSettingsPlp$schema,
    plpTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix,
    cohortTablePrefix = resultDatabaseSettingsPlp$cohortTablePrefix
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
    
    designSummary <- getDesignSummary(
      connectionHandler = connectionHandlerPlp, 
      schema = resultDatabaseSettingsPlp$schema, 
      plpTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix,
      cohortTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix,
      targetIds = targetIds[1],
      outcomeIds = outcomeIds[1]
    )
    
  })
