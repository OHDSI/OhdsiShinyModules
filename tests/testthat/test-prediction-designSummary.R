context("prediction-designSummary")

shiny::testServer(
  app = predictionDesignSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    mySchema = resultDatabaseSettingsPlp$schema,
    myTableAppend = resultDatabaseSettingsPlp$tablePrefix
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    session$setInputs(show_details = list(index = 1))
    expect_true(!is.null(modelDesignId()))
    
    expect_true(is.null(reportId()))
    session$setInputs(show_report = list(index = 1))
    expect_true(!is.null(reportId()))
    
    expect_true(is.null(diagnosticId()))
    session$setInputs(show_diagnostic = list(index = 1))
    expect_true(!is.null(diagnosticId()))
    
  })