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
    
    result <- getPlpResultSelection( # prediction??
        connectionHandler = connectionHandlerPlp, 
        resultDatabaseSettings = resultDatabaseSettingsPlp,
        modelDesignId = 1,
        performanceId = 1
      )
    
    testthat::expect_is(result, 'shiny.tag.list')
    
  })



test_that("Test prediction ui", {
  # Test ui
  ui <- patientLevelPredictionViewer()
  checkmate::expect_list(ui)
})



