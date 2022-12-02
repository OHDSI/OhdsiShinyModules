context("prediction-main")

shiny::testServer(
  app = predictionServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    designSummary$modelDesignId(1)
    ##expect_true(!is.null(modelDesignId()))
    
    performance$performanceId(1)
    # check performanceId() and developmentDatabaseId()
    ##expect_true(!is.null(performanceId()))
    ##expect_true(!is.null(developmentDatabaseId()))
    
    designSummary$reportId(NULL)
    designSummary$reportId(1)
    ##expect_true(file.exists(file.path(tempdir(), 'main.html')))
    
    designSummary$diagnosticId(1)
    
    session$setInputs(allView = 'Model Designs Summary')
    
    
  })