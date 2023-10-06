context("patient-level-prediction-main")

shiny::testServer(
  app = patientLevelPredictionServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    # designSummary 
    ##designSummary$modelDesignId(1)
    ##expect_true(!is.null(modelDesignId()))
    
    ##designSummary$diagnosticId(1)
    
    ##designSummary$reportId(1)
    ##expect_true(file.exists(file.path(tempdir(), 'main.html')))
    
    ##performance$performanceId(1)
    # check performanceId() and developmentDatabaseId()
    ##expect_true(!is.null(performanceId()))
    ##expect_true(!is.null(developmentDatabaseId()))
    
    session$setInputs(allView = 'Model Designs Summary')
    session$setInputs(backToModelSummary = T)
    session$setInputs(backToDesignSummary = T)
    
  })



test_that("Test prediction ui", {
  # Test ui
  ui <- patientLevelPredictionViewer()
  checkmate::expect_list(ui)
})



